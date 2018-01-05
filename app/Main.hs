#!/usr/bin/env stack
{- stack
  runghc
  --resolver lts-10.2
  --install-ghc
  --package turtle
  --package language-dockerfile
  --package containers
-}
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
import Filesystem.Path (FilePath(..))
import Language.Docker hiding (Tag, workdir)
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

newtype Tag a =
    Tag Text
    deriving (Show, Eq)

newtype BuildOptions =
    BuildOptions Text
    deriving (Show)

-- | A Stage is one of the FROM directives ina dockerfile
--
data Stage a = Stage
    { stageName :: Text -- The image name
    , stageTag :: Tag a -- The image tag
    , stagePos :: Linenumber -- Where in the docker file this line is found
    , stageAlias :: Text -- The alias in the FROM instruction
    , buildTag :: Tag a
    , directives :: Dockerfile -- Dockerfile is an alias for [InstructionPos]
    } deriving (Show, Eq)

data InspectedStage
    = NotCached (Stage SourceImage) -- When the image has not yet been built separetely
    | CacheNotInspected (Stage SourceImage) -- When the image is cached but not yet inspected
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
        strValMap = map (\(x, y) -> lift $ string x >> return y)

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
                        else sh (cacheBuild app branch ast)
                Build -> do
                    tag <- Tag <$> needEnv "DOCKER_TAG"
                    buildOPtions <-
                        do opts <- need "DOCKER_BUILD_OPTIONS"
                           if noBuildCache
                               then return $
                                    Just (opts & fromMaybe "" & (<> " --no-cache") & BuildOptions) -- Append the docker --no-cache option
                               else return (fmap BuildOptions opts)
                    if noCacheStages
                        then sh (build app tag buildOPtions ast)
                        else sh (buildFromCache app branch tag buildOPtions ast)

-- | Builds the provided Dockerfile. If it is a multi-stage build, check if those stages are already cached
--   and change the dockerfile to take advantage of that.
buildFromCache :: App -> Branch -> Tag SourceImage -> Maybe BuildOptions -> Dockerfile -> Shell ()
buildFromCache app branch tag buildOptions ast = do
    changedStages <- getChangedStages app branch ast -- Inspect the dockerfile and return
                                                     -- the stages that got their cache invalidated. We need
                                                     -- them to rewrite the docker file and replace the stages
                                                     -- with the ones we have in local cache.
    let bustedStages = replaceStages (mapMaybe alreadyCached changedStages) ast -- We replace the busted stages
                                                                                -- with cached primed ones
    build app tag buildOptions bustedStages
  where
    alreadyCached (CacheInvalidated stage) = Just stage -- We only want to replace stages where the cache
                                                        -- was invalidated by any file changes.
    alreadyCached _ = Nothing

build :: App -> Tag SourceImage -> Maybe BuildOptions -> Dockerfile -> Shell ()
build app tag buildOptions ast = do
    echo "I'll start building now the main Dockerfile"
    status <- buildDockerfile app tag buildOptions ast -- Build the main docker file
                                                       -- which may already have been rewritten to use the
                                                       -- cached stages.
    case status of
        ExitSuccess ->
            echo
                "I built the main dockerfile without a problem. Now call this same script with the `Cache` mode"
        ExitFailure _ -> die "Boo, I could not build the project"

-- | One a dockefile is built, we can extrac each of the stages separately and then tag them, so the cache
--   can be retreived at a later point.
cacheBuild :: App -> Branch -> Dockerfile -> Shell ()
cacheBuild app branch ast = do
    inspectedStages <- getChangedStages app branch ast -- Compare the current dockerfile with whatever we have
                                                       -- in the cache. If there are any chages, then we will need
                                                       -- to rebuild the cache for each of the changed stages.
    let stagesToBuildFresh = [s | NotCached s <- inspectedStages]
    let stagesToReBuild = [s | CacheInvalidated s <- inspectedStages]
    when (stagesToBuildFresh /= []) $ do
        echo "--> Let's build the cache for the first time"
        mapM_ (buildAssetStage app) stagesToBuildFresh -- Build cached images for the first time
    when (stagesToReBuild /= []) $ do
        echo "--> Let's make the cache great again"
        mapM_ (reBuildAssetStage app) stagesToReBuild -- Build each of the stages so they can be reused later

-- | Returns a list of stages which needs to either be built separately or that did not have their cached busted
--   by the introduction of new code.
getChangedStages :: App -> Branch -> Dockerfile -> Shell [InspectedStage]
getChangedStages app branch ast = do
    let mainFile = last (getStages ast) -- Parse all the FROM instructions in the dockerfile and only
                                        -- keep the last FROM, which is the main stage. Anything before that
                                        -- is considered a cacheable stage.
        assetStages = takeWhile (/= mainFile) (getStages ast) -- Filter out the main FROM at the end and only
                                                              -- keep the contents of the file before that instruction.
        stages = mapMaybe (toStage app branch) assetStages -- For each the for found stages, before the main
                                                           -- FROM instruction, convert them to Stage records
    when (length assetStages > length stages) showAliasesWarning
    fold
        (getAlreadyCached stages >>= -- Find the stages that we already have in local cache
         inspectCache >>= -- Gather information to determine whether or not the cache was invalidated
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

-- | Check whehther or not the tag exists for each of the passed stages
--   and return only those that already exist.
getAlreadyCached :: [Stage SourceImage] -> Shell InspectedStage
getAlreadyCached stages = do
    echo "--> I'm checking whether or not the stage exists as a docker image already"
    stage@Stage {buildTag} <- select stages -- foreach stages as stage
    let Tag tag = buildTag -- Get the raw text value for the build tag
    printLn ("----> Looking for image " %s) tag
    existent <-
        fold
            (inproc "docker" ["image", "ls", tag, "--format", "{{.Repository}}"] empty)
            Fold.list -- Get the output of the command as a list of lines
    if existent == mempty
        then do
            echo "------> It does not exist, so I will need to build it myself later"
            return (NotCached stage)
        else do
            echo "------> It already exists, so I will then check if the cache files changed"
            return (CacheNotInspected stage)

-- | This will inspect how an image was build and extrack the ONBUILD directives. If any of those
--   instructions are copying or adding files to the build, they are considered "cache busters".
inspectCache :: InspectedStage -> Shell InspectedStage
inspectCache (CacheNotInspected sourceStage@Stage {..}) = do
    history <- imageHistory buildTag
    let onBuildLines = extractOnBuild history
        workdir = extractWorkdir history
        parsedDirectivesWithErrors = fmap (parseString . Text.unpack) onBuildLines -- Parse each of the lines
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
        let dest = fromString target
            prependedDest = Text.unpack . format fp $ collapse (workdir </> dest)
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
-- In any other case return the same inspected stage
inspectCache c@(NotCached _) = return c
inspectCache c@(Cached _ _) = return c
inspectCache c@(CacheInvalidated _) = return c

toCachedStage :: Stage SourceImage -> Stage CachedImage
toCachedStage Stage {..} =
    let stage = Stage {..}
        Tag sTag = stageTag
        Tag bTag = buildTag
    in stage {stageTag = Tag sTag, buildTag = Tag bTag}

-- | Here check each of the cache buster from the image and compare them with those we have locally,
--   if the files do not match, then we return the stage back as a result, otherwise return Nothing.
shouldBustCache :: InspectedStage -> Shell InspectedStage
shouldBustCache cached@Cached {..} = do
    printLn ("----> Checking cache buster files for stage " %s) (stageName stage)
    withContainer (buildTag stage) checkFiles -- Create a container to inspect the files
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
        let file = fromText $ Text.pack src
        let targetDir = fromText $ Text.pack dest
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
shouldBustCache c@(NotCached _) = return c
shouldBustCache c@(CacheNotInspected _) = return c
shouldBustCache c@(CacheInvalidated _) = return c

-- | Creates a container from a stage and passes the container id to the
--   given shell as an argument
withContainer :: Tag a -> (Text -> Shell b) -> Shell b
withContainer (Tag tag) action = do
    containerId <- inproc "docker" ["create", tag] empty
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
    let filteredDirectives = filter isFrom directives
    doStageBuild app stageTag buildTag filteredDirectives
  where
    isFrom (InstructionPos (From _) _ _) = True
    isFrom _ = False

-- | The goal is to create a temporary dockefile in this same folder with the contents
--   if the stage variable, call docker build with the generated file and tag the image
--   so we can find it later.
reBuildAssetStage :: App -> Stage CachedImage -> Shell ()
reBuildAssetStage app Stage {..} = do
    printLn ("\n--> Rebuilding asset stage " %s % " at line " %d) stageName stagePos
    let fromInstruction =
            toDockerfile $ do
                let Tag t = buildTag
                from $ Text.unpack t `tagged` "latest" -- Use the cached image as base for the new one
    doStageBuild app stageTag buildTag fromInstruction

doStageBuild :: App -> Tag a -> Tag b -> Dockerfile -> Shell ()
doStageBuild app sourceTag targetTag directives = do
    status <- buildDockerfile app sourceTag Nothing directives -- Only build the FROM
    guard (status == ExitSuccess) -- Break if previous command failed
    history <- imageHistory sourceTag -- Get the commands used to build the docker image
    newDockerfile <- createDockerfile sourceTag (extractOnBuild history) -- Append the ONBUILD lines to the new file
    finalStatus <- buildDockerfile app targetTag Nothing newDockerfile -- Now build it
    guard (finalStatus == ExitSuccess) -- Stop here if previous command failed
    echo ""
    echo "--> I have tagged a cache container that I can use next time to speed builds!"

-- | Simply call docker build for the passed arguments
buildDockerfile :: App -> Tag a -> Maybe BuildOptions -> Dockerfile -> Shell ExitCode
buildDockerfile (App app) (Tag tag) buildOPtions directives = do
    currentDirectory <- pwd
    tmpFile <- mktempfile currentDirectory "Dockerfile."
    let BuildOptions opts = fromMaybe (BuildOptions "") buildOPtions
    let allBuildOptions =
            ["build", "--build-arg", "APP_NAME=" <> app, "-f", format fp tmpFile, "-t", tag, "."] <>
            [opts]
    liftIO (writeTextFile tmpFile (Text.pack (prettyPrint directives))) -- Put the Dockerfile contents in the tmp file
    shell ("docker " <> Text.intercalate " " allBuildOptions) empty -- Build the generated dockerfile

-- | Given a list of instructions, build a dockerfile where the tag is the FROM for the file and
--   the list of instructions are wrapped with ONBUILD
createDockerfile :: Tag a -> [Text] -> Shell Dockerfile
createDockerfile (Tag tag) onBuildLines = do
    let eitherDirectives = map (parseString . Text.unpack) onBuildLines
        file =
            toDockerfile $ do
                from (tagged (Text.unpack tag) "latest")
                mapM (onBuildRaw . toInstruction) (rights eitherDirectives) -- Append each of the ONBUILD instructions
    return file
  where
    toInstruction [InstructionPos inst _ _] = inst
    toInstruction _ = error "This is not possible"

-- | Extracts the list of instructions appearing in ONBUILD for a given docker history for a tag
extractOnBuild :: [Line] -> [Text]
extractOnBuild lines = doExtract lines
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
imageHistory :: Tag a -> Shell [Line]
imageHistory (Tag tag) = do
    printLn ("----> Checking the docker image history for " %s) tag
    out <- fold fetchHistory Fold.list -- Buffer all the output of the imageHistory shell
    return (reverse out) -- The history comes in reverse order, sort it naturally
  where
    fetchHistory =
        inproc "docker" ["history", "--no-trunc", "--format", "{{.CreatedBy}}", tag] empty

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
toStage :: App -> Branch -> Dockerfile -> Maybe (Stage a)
toStage (App app) (Branch branch) directives = do
    (stageName, stagePos, stageAlias) <- extractInfo directives -- If getStageInfo returns Nothing, skip the rest
    let sanitized = sanitize stageName
        tagName = app <> "__branch__" <> branch <> "__stage__" <> sanitized
        stageTag = Tag tagName
        buildTag = Tag (tagName <> "-build")
    return Stage {..}
  where
    extractInfo :: Dockerfile -> Maybe (Text, Linenumber, Text)
    extractInfo (InstructionPos {instruction, lineNumber}:_) = getStageInfo instruction lineNumber
    extractInfo _ = Nothing
    getStageInfo :: Instruction -> Linenumber -> Maybe (Text, Linenumber, Text)
    getStageInfo (From (TaggedImage name _ (Just (ImageAlias alias)))) pos =
        Just (Text.pack name, pos, Text.pack alias)
    getStageInfo (From (UntaggedImage name (Just (ImageAlias alias)))) pos =
        Just (Text.pack name, pos, Text.pack alias)
    getStageInfo (From (DigestedImage name _ (Just (ImageAlias alias)))) pos =
        Just (Text.pack name, pos, Text.pack alias)
    getStageInfo _ _ = Nothing
    --
    -- | Makes a string safe to use it as a file name
    sanitize = Text.replace "/" "-" . Text.replace ":" "-"

-- | Extracts the stage alias out of the FROM directive
parseAlias :: String -> Text
parseAlias =
    Text.strip .
    Text.replace "as " "" . -- Remove the alias
    snd .
    Text.breakOn " as " . -- Split by the alias name and get the second in the tuple
    Text.replace " AS " " as " . -- Normalize AS with as
    Text.pack

-- | Given a list of stages and the AST for a Dockerfile, replace all the FROM instructions
--   with their corresponding images as described in the Stage record.
replaceStages :: [Stage CachedImage] -> Dockerfile -> Dockerfile
replaceStages stages dockerLines =
    fmap
        (\InstructionPos {..} -> InstructionPos {instruction = replaceStage instruction, ..})
        dockerLines
  where
    stagesMap = Map.fromList (map createStagePairs stages)
    createStagePairs stage@(Stage {..}) = (stageAlias, stage)
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
        case Map.lookup (Text.pack imageAlias) stagesMap of
            Nothing -> directive
            Just Stage {buildTag, stageAlias} ->
                let Tag t = buildTag
                in From (TaggedImage (Text.unpack t) "latest" (formatAlias stageAlias))
    formatAlias = Just . fromString . Text.unpack

printLn message = printf (message % "\n")

needEnv varName = do
    value <- need varName
    case value of
        Nothing -> error ("I was expecting the " <> show varName <> " env var to be present.")
        Just val -> return val
