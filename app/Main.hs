#!/usr/bin/env stack
{- stack
  runghc
  --resolver lts-9.14
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
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.Text as Text
import Data.Text (Text)
import Filesystem.Path (FilePath)
import Language.Docker hiding (Tag)
import Prelude hiding (FilePath)
import Text.ParserCombinators.ReadP hiding (choice)
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

newtype Tag =
    Tag Text
    deriving (Show, Eq)

newtype SourcePath =
    SourcePath FilePath
    deriving (Show, Eq)

newtype TargetPath =
    TargetPath FilePath
    deriving (Show, Eq)

newtype BuildOptions =
    BuildOptions Text
    deriving (Show)

-- | A Stage is one of the FROM directives ina dockerfile
--
data Stage = Stage
    { stageName :: Text -- The image name
    , stageTag :: Tag -- The image tag
    , stagePos :: Linenumber -- Where in the docker file this line is found
    , stageAlias :: Text -- The alias used for building
    , directives :: Dockerfile -- Dockerfile is an alias for [InstructionPos]
    , alreadyCached :: Bool -- Whether or not we have this stage built separately on the host
    } deriving (Show, Eq)

data InspectedStage
    = NotCached Stage -- When the image has not yet been built separetely
    | Cached { stage :: Stage -- When the image was built and tagged as a separate image
             , cacheBusters :: [(SourcePath, TargetPath)] -- List of files that are able to bust the cache
              }
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
buildFromCache :: App -> Branch -> Tag -> Maybe BuildOptions -> Dockerfile -> Shell ()
buildFromCache app branch tag buildOptions ast = do
    changedStages <- getChangedStages app branch ast -- Inspect the dockerfile and return the stages that got their cache invalidated
    let bustedStages = replaceStages (filter alreadyCached changedStages) ast -- We replace the busted stages with cached primed ones
    build app tag buildOptions bustedStages

build :: App -> Tag -> Maybe BuildOptions -> Dockerfile -> Shell ()
build app tag buildOptions ast = do
    echo "I'll start building now the main Dockerfile"
    status <- buildDockerfile app tag buildOptions ast -- Build the main docker file with the maybe changed stages
    case status of
        ExitSuccess ->
            echo
                "I built the main dockerfile without a problem. Now call this same script with the `Cache` mode"
        ExitFailure _ -> die "Boo, I could not build the project"

-- | One a dockefile is built, we can extrac each of the stages separately and then tag them, so the cache
--   can be retreived at a later point.
cacheBuild :: App -> Branch -> Dockerfile -> Shell ()
cacheBuild app branch ast = do
    changedStages <- getChangedStages app branch ast
    when (changedStages /= []) $ do
        echo "--> Let's make the cache great again"
        mapM_ (buildAssetStage app) changedStages -- Build each of the stages so they can be reused later

-- | Returns a list of stages which needs to either be built separately or that did not have their cached busted
--   by the introduction of new code.
getChangedStages :: App -> Branch -> Dockerfile -> Shell [Stage]
getChangedStages app branch ast = do
    let mainFile = last (getStages ast) -- Parse all the FROM instructions in the dockerfile
        assetStages = takeWhile (/= mainFile) (getStages ast) -- Filter out the main FROM at the end
        stages = mapMaybe (toStage app branch) assetStages -- Convert to Stage records, filter out errors
    when (length assetStages > length stages) showAliasesWarning
  -- Time to get all the stages having changed cache files
    bustCacheStages <- fold (getAlreadyCached stages >>= inspectCache >>= shouldBustCache) Fold.list
    return (catMaybes bustCacheStages) -- Remove the stages that did not actually change
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
getAlreadyCached :: [Stage] -> Shell Stage
getAlreadyCached stages = do
    echo "--> I'm checking whether or not the stage exists as a docker image already"
    stage@Stage {stageTag} <- select stages -- foreach stages as stage
    printLn ("----> Looking for image " %s) (taggedBuild stageTag)
    existent <-
        fold
            (inproc
                 "docker"
                 ["image", "ls", taggedBuild stageTag, "--format", "{{.Repository}}"]
                 empty)
            Fold.list -- Get the output of the command as a list of lines
    if existent == mempty
        then do
            echo "------> It does not exist, so I will need to build it myself later"
            return (stage {alreadyCached = False})
        else do
            echo "------> It already exists, so I will then check if the cache files changed"
            return (stage {alreadyCached = True})

-- | This will inspect how an image was build and extrack the ONBUILD directives. If any of those
--   instructions are copying or adding files to the build, they are considered "cache busters".
inspectCache :: Stage -> Shell InspectedStage
inspectCache stage@Stage {..} =
    if alreadyCached
        then return (NotCached stage)
        else do
            history <- imageHistory (Tag (taggedBuild stageTag))
            let onBuildLines = extractOnBuild history
                workdir = extractWorkdir history
                parsedDirectivesWithErrors = fmap (parseString . Text.unpack) onBuildLines -- Parse each of the lines
                parsedDirectives = (getFirst . rights) parsedDirectivesWithErrors -- We only keep the good lines
                cacheBusters = extractCachePaths workdir parsedDirectives
            return Cached {..}
  where
    extractCachePaths workdir dir =
        let filePairs = concat (mapMaybe doExtract dir) -- Get the (source, target) pairs of files copied
        in fmap (prependWorkdir workdir) filePairs -- Some target paths need to have the WORKDIR prepended
    --
    -- | Prepend a given target dir to the target path
    prependWorkdir workdir (source, TargetPath target) =
        if relative target -- If the target path is relative, we need to prepend the workdir
            then (source, TargetPath (collapse (workdir </> target))) -- Remove the ./ prefix and prepend workdir
            else (source, TargetPath target)
    --
    -- | COPY allows multiple paths in the same line, we need to convert each to a separate path
    doExtract (InstructionPos (Copy _ t) _ _) =
        let target:allSources = (reverse . Text.words . Text.pack) t -- Make sure we get all the files in COPY
            sourcePaths = fmap toSource allSources
        in Just (zip sourcePaths (repeat (toTarget target)))
    --
    -- | This case is simpler, we only need to convert the source and target from ADD
    doExtract (InstructionPos (Add fr t) _ _) =
        Just [((toSource . Text.pack) fr, (toTarget . Text.pack) t)]
    doExtract _ = Nothing
    getFirst (first:_) = first
    getFirst [] = []

-- | Here check each of the cache buster from the image and compare them with those we have locally,
--   if the files do not match, then we return the stage back as a result, otherwise return Nothing.
shouldBustCache :: InspectedStage -> Shell (Maybe Stage)
shouldBustCache (NotCached stage) = return (Just stage)
shouldBustCache Cached {..} = do
    printLn ("----> Checking cache buster files for stage " %s) (stageName stage)
    withContainer stage checkFiles -- Create a container to inspect the files
  where
    checkFiles containerId = do
        hasChanged <- fold (mfilter isJust (checkFileChanged containerId cacheBusters)) Fold.head
        -- ^ Get the cache buster files that have changed since last time
        -- The following is executed for each of the files found
        if isJust hasChanged
            then do
                printLn ("----> The stage " %s % " changed") (stageName stage)
                return (Just stage)
            else do
                printLn ("----> The stage " %s % " did not change") (stageName stage)
                return Nothing
    -- |
    checkFileChanged containerId files = do
        (SourcePath file, TargetPath targetDir) <- select files
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

-- | Creates a container from a stage and passes the container id to the
--   given shell as an argument
withContainer :: Stage -> (Text -> Shell b) -> Shell b
withContainer stage action = do
    containerId <- inproc "docker" ["create", taggedBuild (stageTag stage)] empty
    result <- fold (action (format l containerId)) Fold.list
    _ <- removeContainer containerId -- Ignore the return code of this command
    select result -- yield each result as a separate line
  where
    removeContainer containerId = proc "docker" ["rm", format l containerId] empty

-- | The goal is to create a temporary dockefile in this same folder with the contents
--   if the stage variable, call docker build with the generated file and tag the image
--   so we can find it later.
buildAssetStage :: App -> Stage -> Shell ()
buildAssetStage app Stage {..} = do
    printLn ("\n--> Building asset stage " %s % " at line " %d) stageName stagePos
    let filteredDirectives = filter isFrom directives
    status <- buildDockerfile app stageTag Nothing filteredDirectives -- Only build the FROM
    guard (status == ExitSuccess) -- Break if previous command failed
    history <- imageHistory stageTag -- Get the commands used to build the docker image
    newDockerfile <- createDockerfile stageTag (extractOnBuild history) -- Append the ONBUILD lines to the new file
    finalStatus <- buildDockerfile app (Tag (taggedBuild stageTag)) Nothing newDockerfile -- Now build it
    guard (finalStatus == ExitSuccess) -- Stop here if previous command failed
    echo ""
    echo "--> I have tagged a cache container that I can use next time to speed builds!"
  where
    isFrom (InstructionPos (From _) _ _) = True
    isFrom _ = False

-- | Simply call docker build for the passed arguments
buildDockerfile :: App -> Tag -> Maybe BuildOptions -> Dockerfile -> Shell ExitCode
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
createDockerfile :: Tag -> [Text] -> Shell Dockerfile
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
imageHistory :: Tag -> Shell [Line]
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
toStage :: App -> Branch -> Dockerfile -> Maybe Stage
toStage (App app) (Branch branch) directives = do
    (stageName, stagePos, stageAlias) <- getStageInfo directives -- If getStageInfo returns Nothing, skip the rest
    let sanitized = sanitize stageName
        stageTag = Tag (app <> "__branch__" <> branch <> "__stage__" <> sanitized)
        alreadyCached = False
    return Stage {..}
  where
    getStageInfo :: [InstructionPos] -> Maybe (Text, Linenumber, Text)
    getStageInfo ((InstructionPos (From (TaggedImage name tag)) _ pos):_) =
        if Text.isInfixOf " as " (Text.pack tag) || Text.isInfixOf " AS " (Text.pack tag) -- Make sure the FROM is aliased
            then Just (Text.pack name, pos, parseAlias tag)
            else Nothing
    getStageInfo _ = Nothing
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

taggedBuild :: Tag -> Text
taggedBuild (Tag tag) = tag <> "-build"

-- | Given a list of stages and the AST for a Dockerfile, replace all the FROM instructions
--   with their corresponding images as described in the Stage record.
replaceStages :: [Stage] -> Dockerfile -> Dockerfile
replaceStages stages dockerLines = fmap replaceStage dockerLines
  where
    stagesMap = Map.fromList (map createStagePairs stages)
    createStagePairs stage@(Stage {..}) = (stageAlias, stage)
    --
    -- | Find whehter or not we have extracted a stage with the same alias
    --   If we did, then replace the FROM directive with our own version
    replaceStage directive@(InstructionPos (From (TaggedImage _ tag)) file pos) =
        case Map.lookup (parseAlias tag) stagesMap of
            Nothing -> directive
            Just Stage {stageTag, stageAlias} ->
                InstructionPos
                    (From
                         (TaggedImage (Text.unpack (taggedBuild stageTag)) (formatAlias stageAlias)))
                    file
                    pos
    replaceStage directive = directive
    formatAlias alias = "latest as " <> Text.unpack alias

printLn message = printf (message % "\n")

-- | Transforms a text to SourcePath
toSource = SourcePath . fromText

-- | Transforms a text to TargetPath
toTarget = TargetPath . fromText

needEnv varName = do
    value <- need varName
    case value of
        Nothing -> error ("I was expecting the " <> show varName <> " env var to be present.")
        Just val -> return val
