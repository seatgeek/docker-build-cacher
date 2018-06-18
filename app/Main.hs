{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Control.Foldl as Fold
import Control.Monad (guard, when)
import Data.Either (rights)
import Data.List.NonEmpty (toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import Filesystem.Path (FilePath)
import Language.Docker hiding (workdir)
import Language.Docker.Syntax (Tag(..))
import Prelude hiding (FilePath)
import Text.ParserCombinators.ReadP
import Text.Read
import Turtle

{- Glossary:
    - InstructionPos: in the AST for a docker file, each of the lines are described with the type InstructionPos
    - Dockerfile: A list of InstructionPos
    - stage: Each of the FROM instructions in a Dockerfile
    - cache buster: Files inside a docker image that can be compared with files locally under the same path
-}
newtype App =
    App Text
    deriving (Show)

newtype Branch =
    Branch Text
    deriving (Show)

data SourceImage =
    SourceImage

data CachedImage =
    CachedImage

newtype ImageName a =
    ImageName Text
    deriving (Show, Eq)

newtype BuildOptions =
    BuildOptions Text
    deriving (Show)

newtype CacheLabels =
    CacheLabels [(Text, Text)]
    deriving (Show)

-- | A Stage is one of the FROM directives ina dockerfile
--
data Stage a = Stage
    { stageName :: !Text -- ^ The image name, for example ubuntu in "ubuntu:16"
    , stageTag :: !Text -- ^ The image tag, for example latest in "python:latest"
    , stageImageName :: !(ImageName a) -- ^ The name of the docker image to generate a separate cache for
                                       -- this is pretty much the "template" image to use for 'buildImageName'
                                       --
    , stageFallbackImage :: !(Maybe (ImageName a)) -- ^ If the stageImageName does not exist, then we can try building
                                                   -- from another template, usually the one from the master branch
                                                   --
    , stagePos :: !Linenumber -- ^ Where in the docker file this line is found
    , stageAlias :: !Text -- ^ The alias in the FROM instruction, for example "builder" in "FROM ubuntu:16.10 AS builder"
    , buildImageName :: !(ImageName a) -- ^ The resulting image image, after caching all assets
    , directives :: !Dockerfile -- ^ Dockerfile is an alias for [InstructionPos]
    } deriving (Show, Eq)

newtype NotInspectedCache =
    NotInspectedCache StageCache

data StageCache
    = NotCached (Stage SourceImage) -- When the image has not yet been built separetely
    | FallbackCache (Stage SourceImage) -- When the cache was not present but we looked at a falback key
                    StageCache -- The fallback cache
    | Cached { stage :: Stage CachedImage -- When the image was built and tagged as a separate image
             , cacheBusters :: [(SourcePath, TargetPath)] -- List of files that are able to bust the cache
              }
    | CacheInvalidated (Stage CachedImage)
    deriving (Show)

-- | This script has 2 modes. One for building the Dockerfile, and another for caching its stages
data Mode
    = Build
    | Cache
    deriving (Show)

instance Read Mode where
    readPrec =
        Text.Read.choice $
        strValMap
            [ ("Build", Build) -- Accept both casings
            , ("build", Build)
            , ("Cache", Cache)
            , ("cache", Cache)
            ]
      where
        strValMap = map (\(flag, result) -> lift $ string flag >> return result)

data Args = Args
    { mode :: Mode
    , noCacheStages :: Bool
    , noBuildCache :: Bool
    }

-- | Describes the arguments this script takes from the command line
parser :: Parser Args
parser =
    Args <$> -- Pass the parsed arguments into the Args data container
    argRead "mode" "Whether to build or to cache (options: build | cache)" <*>
    switch
        "no-cache-stages"
        's'
        "Each of the FROM instruction will be cached in separate images if this flag is not set" <*>
    switch "no-cache-build" 'n' "Skip the internal docker cache when building the image"

main :: IO ()
main = do
    Args {mode, noCacheStages, noBuildCache} <-
        options "Builds a docker file and caches its stages" parser -- Parse the CLI arguments as a Mode
    app <- App <$> needEnv "APP_NAME" -- Get the APP environment variable and then wrap it in App
    branch <- Branch <$> needEnv "GIT_BRANCH"
    fallbackBranch <- fmap Branch <$> need "FALLBACK_BRANCH"
    maybeFile <-
        do file <- need "DOCKERFILE" -- Get the dockerfile path if any
           return (fmap fromText file) -- And transform it to a FilePath
  --
  -- if DOCKERFILE is not present, we assume is in the current directory
    currentDirectory <- pwd
    let Just file = maybeFile <|> Just (currentDirectory </> "Dockerfile")
  --
  -- Now we try to parse the dockefile
    dockerFile <- parseFile (Text.unpack (format fp file)) -- Convert the dockerfile to an AST
    case dockerFile of
        Left message -> error ("There was an error parsing the docker file: " <> show message)
        Right ast ->
            case mode of
                Cache ->
                    if noCacheStages
                        then echo "Skipping... I was told not to cache any stages separetely"
                        else sh (cacheBuild app branch fallbackBranch ast)
                Build -> do
                    name <- ImageName <$> needEnv "DOCKER_TAG"
                    buildOPtions <-
                        do opts <- need "DOCKER_BUILD_OPTIONS"
                           if noBuildCache
                               then return $
                                    Just (opts & fromMaybe "" & (<> " --no-cache") & BuildOptions) -- Append the docker --no-cache option
                               else return (fmap BuildOptions opts)
                    if noCacheStages
                        then sh (build app name buildOPtions ast)
                        else sh (buildFromCache app branch fallbackBranch name buildOPtions ast)

-- | Builds the provided Dockerfile. If it is a multi-stage build, check if those stages are already cached
--   and change the dockerfile to take advantage of that.
buildFromCache ::
       App
    -> Branch
    -> Maybe Branch
    -> ImageName SourceImage
    -> Maybe BuildOptions
    -> Dockerfile
    -> Shell ()
buildFromCache app branch fallbackBranch imageName buildOptions ast = do
    changedStages <- getChangedStages app branch fallbackBranch ast -- Inspect the dockerfile and return
                                                                    -- the stages that got their cache invalidated. We need
                                                                    -- them to rewrite the docker file and replace the stages
                                                                    -- with the ones we have in local cache.
    let cachedStages = replaceStages (mapMaybe alreadyCached changedStages) ast -- We replace the busted stages
                                                                                -- with cached primed ones
    build app imageName buildOptions cachedStages
  where
    alreadyCached :: StageCache -> Maybe (Stage CachedImage)
    alreadyCached (CacheInvalidated stage) = Just stage -- We want to replace stages where the cache
                                                        -- was invalidated by any file changes.
    alreadyCached (Cached stage _) = Just stage -- Likewise, once we have a cached stage, we need to keep using it
                                                -- in succesive builds, so the cache is not invalidated again.
    alreadyCached (FallbackCache _ (CacheInvalidated stage)) = Just stage -- Finally, if we are using a fallback, we apply
                                                                          -- the same rules as above for the fallback key
    alreadyCached (FallbackCache _ (Cached stage _)) = Just stage
    alreadyCached _ = Nothing

build :: App -> ImageName SourceImage -> Maybe BuildOptions -> Dockerfile -> Shell ()
build app imageName buildOptions ast = do
    echo "I'll start building now the main Dockerfile"
    status <- buildDockerfile app imageName buildOptions ast -- Build the main docker file
                                                       -- which may already have been rewritten to use the
                                                       -- cached stages.
    case status of
        ExitSuccess ->
            echo
                "I built the main dockerfile without a problem. Now call this same script with the `Cache` mode"
        ExitFailure _ -> die "Boo, I could not build the project"

-- | One a dockefile is built, we can extract each of the stages separately and then tag them, so the cache
--   can be retreived at a later point.
cacheBuild :: App -> Branch -> Maybe Branch -> Dockerfile -> Shell ()
cacheBuild app branch fallbackBranch ast = do
    inspectedStages <- getChangedStages app branch fallbackBranch ast -- Compare the current dockerfile with whatever we have
                                                                      -- in the cache. If there are any chages, then we will need
                                                                      -- to rebuild the cache for each of the changed stages.
    let stagesToBuildFresh = [stage | NotCached stage <- inspectedStages]
    let stagesToReBuild = [stage | CacheInvalidated stage <- inspectedStages]
    let stagesToBuildFromFallback =
            [(uncached, cached) | FallbackCache uncached (Cached cached _) <- inspectedStages]
    when (stagesToBuildFresh /= []) $ do
        echo "--> Let's build the cache for the first time"
        mapM_ (buildAssetStage app) stagesToBuildFresh -- Build cached images for the first time
    when (stagesToBuildFromFallback /= []) $ do
        echo "--> Let's build the cache for the first time using a fallback"
        mapM_ (uncurry (reBuildFromFallback app)) stagesToBuildFromFallback
    when (stagesToReBuild /= []) $ do
        echo "--> Let's re-build the cache for stages that changed"
        mapM_ (reBuildAssetStage app) stagesToReBuild -- Build each of the stages so they can be reused later

-- | Returns a list of stages which needs to either be built separately or that did not have their cached busted
--   by the introduction of new code.
getChangedStages :: App -> Branch -> Maybe Branch -> Dockerfile -> Shell [StageCache]
getChangedStages app branch fallbackBranch ast = do
    let assetStages = init (getStages ast) -- Filter out the main FROM at the end and only
                                           -- keep the contents of the file before that instruction.
        stages = mapMaybe (toStage app branch fallbackBranch) assetStages -- For each the for found stages, before the main
                                                                           -- FROM instruction, convert them to Stage records
    when (length assetStages > length stages) showAliasesWarning
    fold
        (getAlreadyCached stages >>= -- Find the stages that we already have in local cache
         shouldBustCache -- Determine whether or not the cache was invalidated
         )
        Fold.list
  where
    showAliasesWarning = do
        echo "::::::WARNING::::::"
        echo "I found some FROM directives in the dockerfile that did not have an `as` alias"
        echo "I'm not smart enough to build multi-stage docker files without aliases."
        echo "While this is safe to do, you will get no cache benefits"
        echo ""
        echo "Please always write your FROM directives as `FROM image:tag as myalias`"

-- | Check whehther or not the imageName exists for each of the passed stages
--   and return only those that already exist.
getAlreadyCached :: [Stage SourceImage] -> Shell StageCache
getAlreadyCached stages = do
    echo "--> I'm checking whether or not the stage exists as a docker image already"
    stage@Stage {buildImageName, stageFallbackImage} <- select stages -- foreach stages as stage
    exists <- cacheKeyExists stage buildImageName
    if exists
        then do
            echo "------> It already exists, so I will then check if the cache files changed"
            inspectCache stage
        else do
            echo "------> It does not exist, so I will need to build it myself later"
            maybe (return (NotCached stage)) (getFallbackCache stage) stageFallbackImage
  where
    cacheKeyExists stage (ImageName imageName) = do
        printLn ("----> Looking for image " %s) imageName
        existent <-
            fold
                (inproc "docker" ["image", "ls", imageName, "--format", "{{.Repository}}"] empty)
                Fold.list -- Get the output of the command as a list of lines
        if existent == mempty
            then return False
                -- For the cache to be valid, we need to make sure that the stored image is based on the same
                -- base image and tag. Otherwise we will need to rebuild the cache anyway
            else imageAndTagMatches (ImageName (stageName stage)) (Tag (stageTag stage)) imageName
    --
    --
    getFallbackCache stage fallbackName = do
        exists <- cacheKeyExists stage fallbackName
        if exists
            then do
                echo "------> The fallback image exists, using it to build the initial cache"
                cachedStage <-
                    inspectCache
                        (stage {stageImageName = fallbackName, buildImageName = fallbackName})
                return (FallbackCache stage cachedStage)
            else do
                echo "------> There is not fallback cache image"
                return (NotCached stage)

-- | This will inspect how an image was build and extrack the ONBUILD directives. If any of those
--   instructions are copying or adding files to the build, they are considered "cache busters".
inspectCache :: Stage SourceImage -> Shell StageCache
inspectCache sourceStage@Stage {..} = do
    history <- imageHistory buildImageName
    let onBuildLines = extractOnBuild history
        workdir = extractWorkdir history
        parsedDirectivesWithErrors = fmap parseText onBuildLines -- Parse each of the lines
        parsedDirectives = (getFirst . rights) parsedDirectivesWithErrors -- We only keep the good lines
        cacheBusters = extractCachePaths workdir parsedDirectives
    return $ Cached (toCachedStage sourceStage) cacheBusters
  where
    extractCachePaths workdir dir =
        let filePairs = concat (mapMaybe doExtract dir) -- Get the (source, target) pairs of files copied
        in fmap (prependWorkdir workdir) filePairs -- Some target paths need to have the WORKDIR prepended
    --
    -- | Prepend a given target dir to the target path
    prependWorkdir workdir (source, TargetPath target) =
        let dest = fromText target
            prependedDest = format fp (collapse (workdir </> dest))
        in if relative dest -- If the target path is relative, we need to prepend the workdir
               then (source, TargetPath prependedDest) -- Remove the ./ prefix and prepend workdir
               else (source, TargetPath target)
    --
    -- | COPY allows multiple paths in the same line, we need to convert each to a separate path
    doExtract (InstructionPos (Copy CopyArgs {sourcePaths, targetPath}) _ _) =
        Just (zip (toList sourcePaths) (repeat targetPath))
    --
    -- | This case is simpler, we only need to convert the source and target from ADD
    doExtract (InstructionPos (Add AddArgs {sourcePaths, targetPath}) _ _) =
        Just (zip (toList sourcePaths) (repeat targetPath))
    doExtract _ = Nothing
    getFirst (first:_) = first
    getFirst [] = []

toCachedStage :: Stage SourceImage -> Stage CachedImage
toCachedStage Stage {..} =
    let stage = Stage {..}
        ImageName sImageName = stageImageName
        ImageName bImageName = buildImageName
    in stage
       { stageImageName = ImageName sImageName
       , stageFallbackImage = Nothing
       , buildImageName = ImageName bImageName
       }

-- | Here check each of the cache buster from the image and compare them with those we have locally,
--   if the files do not match, then we return the stage back as a result, otherwise return Nothing.
shouldBustCache :: StageCache -> Shell StageCache
shouldBustCache cached@Cached {..} = do
    printLn ("----> Checking cache buster files for stage " %s) (stageName stage)
    withContainer (buildImageName stage) checkFiles -- Create a container to inspect the files
  where
    checkFiles containerId = do
        hasChanged <- fold (mfilter isJust (checkFileChanged containerId cacheBusters)) Fold.head
        -- ^ Get the cache buster files that have changed since last time
        -- The following is executed for each of the files found
        if isJust hasChanged
            then do
                printLn ("----> The stage " %s % " changed") (stageName stage)
                return (CacheInvalidated stage)
            else do
                printLn ("----> The stage " %s % " did not change") (stageName stage)
                return cached
    -- |
    checkFileChanged containerId files = do
        (SourcePath src, TargetPath dest) <- select files
        let file = fromText src
        let targetDir = fromText dest
        printLn ("------> Checking file '" %fp % "' in directory " %fp) file targetDir
        currentDirectory <- pwd
        tempFile <- mktempfile currentDirectory "comp"
        let targetFile = targetDir </> file
        status <-
            proc
                "docker"
                ["cp", format (s % ":" %fp) containerId targetFile, format fp tempFile]
                empty
        guard (status == ExitSuccess)
        local <- liftIO (readTextFile file)
        remote <- liftIO (readTextFile tempFile)
        if local == remote
            then return Nothing
            else return (Just file)
-- In any other case return the same inspected stage
shouldBustCache c@NotCached {} = return c
shouldBustCache c@CacheInvalidated {} = return c
shouldBustCache c@FallbackCache {} = return c

-- | Creates a container from a stage and passes the container id to the
--   given shell as an argument
withContainer :: ImageName a -> (Text -> Shell b) -> Shell b
withContainer (ImageName imageName) action = do
    containerId <- inproc "docker" ["create", imageName] empty
    result <- fold (action (format l containerId)) Fold.list
    _ <- removeContainer containerId -- Ignore the return code of this command
    select result -- yield each result as a separate line
  where
    removeContainer containerId = proc "docker" ["rm", format l containerId] empty

-- | The goal is to create a temporary dockefile in this same folder with the contents
--   if the stage variable, call docker build with the generated file and tag the image
--   so we can find it later.
buildAssetStage :: App -> Stage SourceImage -> Shell ()
buildAssetStage app Stage {..} = do
    printLn
        ("\n--> Building asset stage " %s % " at line " %d % " for the first time")
        stageName
        stagePos
    let fromInstruction = filter isFrom directives
        cacheLabels = [("cached_image", stageName <> ":" <> stageTag)]
        newDockerfile =
            toDockerfile $ do
                embed fromInstruction
                label cacheLabels
    doStageBuild app stageImageName buildImageName (CacheLabels cacheLabels) newDockerfile
  where
    isFrom (InstructionPos (From _) _ _) = True
    isFrom _ = False

-- | The goal is to create a temporary dockefile in this same folder with the contents
--   if the stage variable, call docker build with the generated file and tag the image
--   so we can find it later.
reBuildAssetStage :: App -> Stage CachedImage -> Shell ()
reBuildAssetStage app Stage {..} = do
    printLn ("\n--> Rebuilding asset stage " %s % " at line " %d) stageName stagePos
    let cacheLabels = [("cached_image", stageName <> ":" <> stageTag)]
        newDockerfile =
            toDockerfile $ do
                let ImageName t = stageImageName
                from $ toImage t `tagged` "latest" -- Use the cached image as base for the new one
                label cacheLabels
    doStageBuild app stageImageName buildImageName (CacheLabels cacheLabels) newDockerfile

reBuildFromFallback :: App -> Stage SourceImage -> Stage CachedImage -> Shell ()
reBuildFromFallback app uncached cached = do
    let cacheLabels = [("cached_image", stageName uncached <> ":" <> stageTag uncached)]
        newDockerfile =
            toDockerfile $ do
                let ImageName t = buildImageName cached
                from $ toImage t `tagged` "latest" -- Use the cached image as base for the new one
                label cacheLabels
    doStageBuild
        app
        (stageImageName uncached)
        (buildImageName uncached)
        (CacheLabels cacheLabels)
        newDockerfile

doStageBuild :: App -> ImageName a -> ImageName b -> CacheLabels -> Dockerfile -> Shell ()
doStageBuild app sourceImageName targetImageName cacheLabels directives = do
    status <- buildDockerfile app sourceImageName Nothing directives -- Only build the FROM
    guard (status == ExitSuccess) -- Break if previous command failed
    history <- imageHistory sourceImageName -- Get the commands used to build the docker image
    newDockerfile <- createDockerfile sourceImageName cacheLabels (extractOnBuild history) -- Append the ONBUILD lines to the new file
    finalStatus <- buildDockerfile app targetImageName Nothing newDockerfile -- Now build it
    guard (finalStatus == ExitSuccess) -- Stop here if previous command failed
    echo ""
    echo "--> I have tagged a cache container that I can use next time to speed builds!"

-- | Simply call docker build for the passed arguments
buildDockerfile :: App -> ImageName a -> Maybe BuildOptions -> Dockerfile -> Shell ExitCode
buildDockerfile (App app) (ImageName imageName) buildOPtions directives = do
    currentDirectory <- pwd
    tmpFile <- mktempfile currentDirectory "Dockerfile."
    let BuildOptions opts = fromMaybe (BuildOptions "") buildOPtions
    let allBuildOptions =
            [ "build"
            , "--build-arg"
            , "APP_NAME=" <> app
            , "-f"
            , format fp tmpFile
            , "-t"
            , imageName
            , "."
            ] <>
            [opts]
    liftIO (writeTextFile tmpFile (LT.toStrict (prettyPrint directives))) -- Put the Dockerfile contents in the tmp file
    shell ("docker " <> Text.intercalate " " allBuildOptions) empty -- Build the generated dockerfile

-- | Given a list of instructions, build a dockerfile where the imageName is the FROM for the file and
--   the list of instructions are wrapped with ONBUILD
createDockerfile :: ImageName a -> CacheLabels -> [Text] -> Shell Dockerfile
createDockerfile (ImageName imageName) (CacheLabels cacheLabels) onBuildLines = do
    let eitherDirectives = map parseText onBuildLines
        validDirectives = rights eitherDirectives -- Just in case, filter out bad directives
        file =
            toDockerfile $ do
                from $ toImage imageName `tagged` "latest"
                label cacheLabels
                mapM (onBuildRaw . toInstruction) validDirectives -- Append each of the ONBUILD instructions
    return file
  where
    toInstruction [InstructionPos inst _ _] = inst
    toInstruction _ = error "This is not possible"

-- | Extracts the list of instructions appearing in ONBUILD for a given docker history for a tag
extractOnBuild :: [Line] -> [Text]
extractOnBuild = doExtract
  where
    onBuildPrefix = "/bin/sh -c #(nop)  ONBUILD "
    --
    -- | First get the ONBUILD lines then strip the common prefix out of each.
    --   Arguments flow from right to left in the functions chain
    doExtract = mapMaybe (Text.stripPrefix onBuildPrefix . lineToText) . findOnBuildLines
    --
    -- | First drop the lines not starting with ONBUILD, then take only those.
    --   Arguments flow from right to left in the functions chain
    findOnBuildLines = takeWhile isOnBuild . dropWhile (not . isOnBuild)
    isOnBuild line = Text.isPrefixOf onBuildPrefix (lineToText line)

-- | Extracts the label from the cached image passed in the last argument and checks
-- if it matches the passeed image name and tag name. This is used to avoid using a
-- cached image tht was built using a different base iamge
imageAndTagMatches :: ImageName Text -> Tag -> Text -> Shell Bool
imageAndTagMatches (ImageName imageName) (Tag tagName) cachedImage = do
    printLn ("------> Checking the stored cached key in a label for " %s) cachedImage
    value <- fold getCacheLabel Fold.head -- Get the only line
    let expected = unsafeTextToLine (imageName <> ":" <> tagName)
    return (Just expected == value)
  where
    getCacheLabel =
        inproc
            "docker"
            ["inspect", "--format", "{{ index .Config.Labels \"cached_image\"}}", cachedImage]
            empty

extractWorkdir :: [Line] -> FilePath
extractWorkdir instructions =
    case reverse (doExtract instructions) of
        [] -> fromText "/"
        lastWorkdir:_ -> fromText lastWorkdir -- We are on;y intereted in the latest WORKDIR declared
  where
    onBuildPrefix = "/bin/sh -c #(nop)  ONBUILD " -- Curiously, ONBUILD is lead by 2 spaces
    workdirPrefix = "/bin/sh -c #(nop) WORKDIR " -- Whereas WORKDIR only by one space
    --
    -- | First find all relevant instructions, then keep the lines starting with the workdir prefix
    doExtract = mapMaybe (Text.stripPrefix workdirPrefix . lineToText) . findRelevantLines
    --
    -- | Take lines until a ONBUILD is found
    --   Arguments flow from right to left in the functions chain
    findRelevantLines = takeWhile (not . isOnBuild)
    isOnBuild line = Text.isPrefixOf onBuildPrefix (lineToText line)

-- | Calls docker history for the given image name and returns the output as a list
imageHistory :: ImageName a -> Shell [Line]
imageHistory (ImageName name) = do
    printLn ("----> Checking the docker image history for " %s) name
    out <- fold fetchHistory Fold.list -- Buffer all the output of the imageHistory shell
    return (reverse out) -- The history comes in reverse order, sort it naturally
  where
    fetchHistory =
        inproc "docker" ["history", "--no-trunc", "--format", "{{.CreatedBy}}", name] empty

--
-- | Returns a list of directives grouped by the appeareance of the FROM directive
--   This will return the group of all stages found in the Dockerfile
getStages :: Dockerfile -> [Dockerfile]
getStages ast = filter startsWithFROM (group ast [])
  where
    group [] acc = reverse acc -- End of recursion
    group (directive@(InstructionPos (From _) _ _):rest) acc = group rest ([directive] : acc) -- Append a new group
    group (directive:rest) [] = group rest [[directive]] -- Create a new group
    group (directive:rest) (current:prev) = group rest ((current ++ [directive]) : prev) -- Continue the currently open group
    --
    -- | Returns true if the first element in the list is a FROM directive
    startsWithFROM (InstructionPos (From _) _ _:_) = True
    startsWithFROM _ = False

-- | Converts a list of instructions into a Stage record
toStage :: App -> Branch -> Maybe Branch -> Dockerfile -> Maybe (Stage a)
toStage (App app) branch fallback directives = do
    (stageName, stageTag, stagePos, stageAlias) <- extractInfo directives -- If getStageInfo returns Nothing, skip the rest
    let newImageName (Branch branchName) =
            app <> "__branch__" <> branchName <> "__stage__" <> sanitize stageName
        stageImageName = ImageName (newImageName branch)
        buildImageName = ImageName (newImageName branch <> "-build")
        stageFallbackImage = fmap (\br -> ImageName (newImageName br <> "-build")) fallback
    return Stage {..}
  where
    extractInfo :: Dockerfile -> Maybe (Text, Text, Linenumber, Text)
    extractInfo (InstructionPos {instruction, lineNumber}:_) = getStageInfo instruction lineNumber
    extractInfo _ = Nothing
    getStageInfo :: Instruction Text -> Linenumber -> Maybe (Text, Text, Linenumber, Text)
    getStageInfo (From (TaggedImage Image {imageName} (Tag tag) (Just (ImageAlias alias)))) pos =
        Just (imageName, tag, pos, alias)
    getStageInfo (From (UntaggedImage Image {imageName} (Just (ImageAlias alias)))) pos =
        Just (imageName, "latest", pos, alias)
    getStageInfo (From (DigestedImage Image {imageName} tag (Just (ImageAlias alias)))) pos =
        Just (imageName, tag, pos, alias)
    getStageInfo _ _ = Nothing
    --
    -- | Makes a string safe to use it as a file name
    sanitize = Text.replace "/" "-" . Text.replace ":" "-"

-- | Given a list of stages and the AST for a Dockerfile, replace all the FROM instructions
--   with their corresponding images as described in the Stage record.
replaceStages :: [Stage CachedImage] -> Dockerfile -> Dockerfile
replaceStages stages =
    fmap (\InstructionPos {..} -> InstructionPos {instruction = replaceStage instruction, ..})
  where
    stagesMap = Map.fromList (map createStagePairs stages)
    createStagePairs stage@Stage {..} = (stageAlias, stage)
    --
    -- | Find whehter or not we have extracted a stage with the same alias
    --   If we did, then replace the FROM directive with our own version
    replaceStage directive@(From (TaggedImage _ _ (Just (ImageAlias imageAlias)))) =
        replaceKnownAlias directive imageAlias
    replaceStage directive@(From (UntaggedImage _ (Just (ImageAlias imageAlias)))) =
        replaceKnownAlias directive imageAlias
    replaceStage directive@(From (DigestedImage _ _ (Just (ImageAlias imageAlias)))) =
        replaceKnownAlias directive imageAlias
    replaceStage directive = directive
    replaceKnownAlias directive imageAlias =
        case Map.lookup imageAlias stagesMap of
            Nothing -> directive
            Just Stage {buildImageName, stageAlias} ->
                let ImageName t = buildImageName
                in From (TaggedImage (toImage t) "latest" (formatAlias stageAlias))
    formatAlias = Just . fromString . Text.unpack

printLn :: MonadIO io => Format (io ()) r -> r
printLn message = printf (message % "\n")

toImage :: Text -> Image
toImage = fromString . Text.unpack

needEnv :: MonadIO m => Text -> m Text
needEnv varName = do
    value <- need varName
    case value of
        Nothing -> error ("I was expecting the " <> show varName <> " env var to be present.")
        Just val -> return val
