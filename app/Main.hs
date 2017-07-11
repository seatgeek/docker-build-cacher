#!/usr/bin/env stack
{- stack
  runghc
  --resolver lts-8.18
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
import Control.Monad (when, guard)
import Data.Either (rights)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, catMaybes, isJust)
import qualified Data.Text as Text
import Data.Text (Text)
import Filesystem.Path (FilePath)
import Language.Dockerfile hiding (Tag)
import Prelude hiding (FilePath)
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
          ,  cacheBusters :: [FilePath] -- List of files that are able to bust the cache
           }
  deriving (Show)

-- | This script has 2 modes. One for building the Dockerfile, and another for caching its stages
data Mode
  = Build
  | Cache
  deriving (Read, Show)

-- | Describes the arguments this script takes from the command line
parser :: Parser Mode
parser = argRead "mode" "Whether to build or to cache (options: Build | Cache)"

main = do
  mode <- options "Builds a docker file and caches its stages" parser -- Parse the CLI arguments as a Mode
  app <- App <$> needEnv "APP_NAME" -- Get the APP environment variable and then wrap it in App
  branch <- Branch <$> needEnv "GIT_BRANCH"
  currentDirectory <- pwd
  let file = currentDirectory </> "_infrastructure/Dockerfile"
  dockerFile <- parseFile (Text.unpack $ format fp file) -- Convert the dockerfile to an AST
  case dockerFile of
    Left message -> error ("There was an error parsing the docker file: " <> show message)
    Right ast ->
      case mode of
        Cache -> sh (cacheBuild app branch ast)
        Build -> do
          tag <- Tag <$> needEnv "DOCKER_TAG"
          sh (build app branch tag ast)

-- | Builds the provided Dockerfile. If it is a multi-stage build, check if those stages are already cached
--   and change the dockerfile to take advantage of that.
build :: App -> Branch -> Tag -> Dockerfile -> Shell ()
build app branch tag ast = do
  changedStages <- getChangedStages app branch ast -- Inspect the dockerfile and return the stages that got their cache invalidated
  echo "I'll start building now the main Dockerfile"
  let bustedStages = replaceStages (filter alreadyCached changedStages) ast -- We replace the busted stages with cached primed ones
  status <- buildDockerfile app tag bustedStages -- Build the main docker file with the maybe changed stages
  case status of
    ExitSuccess -> do
      echo
        "I built the main dockerfile without a problem. Now call this same script with the `Cache` mode"
    ExitFailure _ -> do
      die "Boo, I could not build the project"

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
  stage@(Stage {stageTag}) <- select stages -- foreach stages as stage
  printLn ("----> Looking for image " %s) (taggedBuild stageTag)
  existent <-
    fold
      (inproc "docker" ["image", "ls", taggedBuild stageTag, "--format", "{{.Repository}}"] empty)
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
inspectCache stage@(Stage {..}) = do
  case alreadyCached of
    False -> return (NotCached stage)
    True -> do
      onBuildLines <- extractOnBuild (Tag $ taggedBuild stageTag)
      let parsedDirectivesWithErrors = fmap (parseString . Text.unpack) onBuildLines -- Parse each of the lines
          parsedDirectives = head . rights $ parsedDirectivesWithErrors -- We only keep the good lines
          cachePaths = extractCachePaths parsedDirectives
          cacheBusters = fmap fromText cachePaths
      return $ Cached {..}
  where
    extractCachePaths dir = concat (mapMaybe doExtract dir)
    doExtract (InstructionPos (Copy fr t) _ _) = Just ((Text.pack fr) : explodeAndDropLast t) -- Make sure we get all the files in COPY
    doExtract (InstructionPos (Add fr _) _ _) = Just [Text.pack fr] -- The source path is the cache buster
    doExtract _ = Nothing
    explodeAndDropLast t =
      let _:rest = reverse (fmap Text.strip (Text.splitOn " " . Text.pack $ t))
      in reverse rest

-- | Here check each of the cache buster from the image and compare them with those we have locally,
--   if the files do not match, then we return the stage back as a result, otherwise return Nothing.
shouldBustCache :: InspectedStage -> Shell (Maybe Stage)
shouldBustCache (NotCached stage) = return (Just stage)
shouldBustCache Cached {..} = do
  printLn ("----> Checking cache buster files for stage " %s) (stageName stage)
  containerId <- inproc "docker" ["create", (taggedBuild (stageTag stage))] empty
  hasChanged <- fold (mfilter isJust (checkFileChanged containerId cacheBusters)) Fold.head
  if isJust hasChanged
    then do
      printLn ("----> The stage " %s % " changed") (stageName stage)
      return (Just stage)
    else do
      printLn ("----> The stage " %s % " did not change") (stageName stage)
      return Nothing
  where
    checkFileChanged containerId files = do
      file <- select files
      printLn ("------> Checking file " %fp) file
      currentDirectory <- pwd
      tempFile <- mktempfile currentDirectory "comp"
      status <-
        proc "docker" ["cp", format (l % ":/app/" %fp) containerId file, format fp tempFile] empty
      guard (status == ExitSuccess)
      local <- liftIO $ readTextFile file
      remote <- liftIO $ readTextFile tempFile
      if local == remote
        then return Nothing
        else return (Just file)

-- | The goal is to create a temporary dockefile in this same folder with the contents
--   if the stage variable, call docker build with the generated file and tag the image
--   so we can find it later.
buildAssetStage :: App -> Stage -> Shell ()
buildAssetStage app Stage {..} = do
  printLn ("\n--> Building asset stage " %s % " at line " %d) stageName stagePos
  let filteredDirectives = filter isFrom directives
  status <- buildDockerfile app stageTag filteredDirectives -- Only build the FROM
  guard (status == ExitSuccess) -- Break if previous command failed
  onBuildLines <- extractOnBuild stageTag -- We want to preserve the ONBUILD in the new dockerfile
  newDockerfile <- createDockerfile stageTag onBuildLines -- Append the ONBUILD liens to the new file
  finalStatus <- buildDockerfile app (Tag $ taggedBuild stageTag) newDockerfile -- Now build it
  guard (finalStatus == ExitSuccess) -- Stop here if previous command failed
  echo ""
  echo "--> I have tagged a cache container that I can use next time to speed builds!"
  where
    isFrom (InstructionPos (From _) _ _) = True
    isFrom _ = False

-- | Simply call docker build for the passed arguments
buildDockerfile :: App -> Tag -> Dockerfile -> Shell ExitCode
buildDockerfile (App app) (Tag tag) directives = do
  currentDirectory <- pwd
  tmpFile <- mktempfile currentDirectory "Dockerfile."
  liftIO $ writeTextFile tmpFile (Text.pack (prettyPrint directives)) -- Put the Dockerfile contents in the tmp file
  proc
    "docker"
    ["build", "--build-arg", "APP_NAME=" <> app, "-f", format fp tmpFile, "-t", tag, "."]
    empty -- Build the generated dockerfile

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
    toInstruction [(InstructionPos inst _ _)] = inst
    toInstruction _ = error "This is not possible"

-- | Extracts the list of instructions appearing in ONBUILD for a given docker image tag
extractOnBuild :: Tag -> Shell [Text]
extractOnBuild (Tag tag) = do
  printLn ("----> Checking the docker image history for " %s) tag
  out <- fold (imageHistory tag) Fold.list -- Buffer all the output of the imageHistory shell
  return (reverse (doExtract out)) -- The history comes in reverse order, sort it naturally
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

-- | Calls docker history for the given image name
imageHistory :: Text -> Shell Line
imageHistory tag =
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
    startsWithFROM ((InstructionPos (From _) _ _):_) = True
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
      if Text.isInfixOf " as " (Text.pack tag) -- Make sure the FROM is aliased
        then Just (Text.pack name, pos, parseAlias tag)
        else Nothing
    getStageInfo _ = Nothing
    --
    -- | Makes a string safe to use it as a file name
    sanitize = Text.replace "/" "-" . Text.replace ":" "-"

-- | Extracts the stage alias out of the FROM directive
parseAlias :: String -> Text
parseAlias = Text.strip . Text.replace "as " "" . snd . Text.breakOn " as " . Text.pack

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
            (From (TaggedImage (Text.unpack (taggedBuild stageTag)) (formatAlias stageAlias)))
            file
            pos
    replaceStage directive = directive
    formatAlias alias = "latest as " <> Text.unpack alias

printLn message = printf (message % "\n")

needEnv varName = do
  value <- need varName
  case value of
    Nothing -> error ("I was expecting the " <> show varName <> " env var to be present.")
    Just val -> return val
