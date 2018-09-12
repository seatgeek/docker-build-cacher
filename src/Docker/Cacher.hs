{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Docker.Cacher where

import qualified Control.Foldl                 as Fold
import           Control.Monad                  ( guard
                                                , when
                                                )
import           Data.Either                    ( rights )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe
                                                , mapMaybe
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import qualified Data.Text.Lazy                as LT
import           Language.Docker
import           Language.Docker.Syntax         ( Tag(..) )
import           Prelude                 hiding ( FilePath )
import           Turtle

import qualified Docker.Cacher.Inspect
import           Docker.Cacher.Inspect          ( ImageConfig(..)
                                                , StageCache(..)
                                                )
import           Docker.Cacher.Internal
import qualified Data.List.NonEmpty
import qualified Data.Aeson.Text
import qualified Data.Coerce

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

newtype BuildOptions =
    BuildOptions Text
    deriving (Show)

newtype CacheLabels =
    CacheLabels [(Text, Text)]
    deriving (Show)


-- | Builds the provided Dockerfile. If it is a multi-stage build, check if those stages are already cached
--   and change the dockerfile to take advantage of that.
buildFromCache
  :: App
  -> Branch
  -> Maybe Branch
  -> ImageName SourceImage
  -> Maybe BuildOptions
  -> Dockerfile
  -> Shell ()
buildFromCache app branch fallbackBranch imageName buildOptions ast = do
  -- Inspect the dockerfile and return the stages that got their cache invalidated. We need
  -- them to rewrite the docker file and replace the stages with the ones we have in local cache.
  changedStages <- getChangedStages app branch fallbackBranch ast

  -- We replace the busted stages with cached primed ones
  let cachedStages = replaceStages
        (mapMaybe Docker.Cacher.Inspect.alreadyCached changedStages)
        ast

  build app imageName buildOptions cachedStages


build
  :: App
  -> ImageName SourceImage
  -> Maybe BuildOptions
  -> Dockerfile
  -> Shell ()
build app imageName buildOptions ast = do
  echo "I'll start building now the main Dockerfile"

  -- Build the main docker file which may already have been rewritten to use the
  -- cached stages.
  status <- buildDockerfile app imageName buildOptions ast

  case status of
    ExitSuccess ->
      echo
        "I built the main dockerfile without a problem. Now call this same script with the `Cache` mode"
    ExitFailure _ -> die "Boo, I could not build the project"


-- | One a dockefile is built, we can extract each of the stages separately and then tag them, so the cache
-- can be retreived at a later point.
cacheBuild :: App -> Branch -> Maybe Branch -> Dockerfile -> Shell ()
cacheBuild app branch fallbackBranch ast = do
  -- Compare the current dockerfile with whatever we have in the cache. If there are any chages,
  -- then we will need to rebuild the cache for each of the changed stages.
  inspectedStages <- getChangedStages app branch fallbackBranch ast

  let stagesToBuildFresh = [ stage | NotCached stage <- inspectedStages ]
  let stagesToReBuild =
        [ (uncached, stage)
        | CacheInvalidated uncached stage <- inspectedStages
        ]
  let stagesToBuildFromFallback =
        [ (uncached, cached)
        | FallbackCache uncached (Cached cached _) <- inspectedStages
        ]

  when (stagesToBuildFresh /= []) $ do
    echo "--> Let's build the cache for the first time"
    mapM_ (buildAssetStage app) stagesToBuildFresh -- Build cached images for the first time

  when (stagesToBuildFromFallback /= []) $ do
    echo "--> Let's build the cache for the first time using a fallback"
    mapM_ (uncurry (reBuildFromFallback app)) stagesToBuildFromFallback

  when (stagesToReBuild /= []) $ do
    echo "--> Let's re-build the cache for stages that changed"
    mapM_ (uncurry (reBuildAssetStage app)) stagesToReBuild -- Build each of the stages so they can be reused later


-- | Returns a list of stages which needs to either be built separately or that did not have their cached busted
--   by the introduction of new code.
getChangedStages
  :: App -> Branch -> Maybe Branch -> Dockerfile -> Shell [StageCache]
getChangedStages app branch fallbackBranch ast = do
  let
       -- Filter out the main FROM at the end and only
       -- keep the contents of the file before that instruction.
      assetStages = init (getStages ast)

      -- For each of the found stages, before the main
      -- FROM instruction, convert them to Stage records
      stages      = mapMaybe (toStage app branch fallbackBranch) assetStages

  when (length assetStages > length stages) showAliasesWarning
  fold
    (
        -- Find the stages that we already have in local cache
        Docker.Cacher.Inspect.getAlreadyCached stages
        -- Determine whether or not the cache was invalidated
    >>= uncurry Docker.Cacher.Inspect.shouldBustCache
    )
    Fold.list
 where
  showAliasesWarning = do
    echo "::::::WARNING::::::"
    echo
      "I found some FROM directives in the dockerfile that did not have an `as` alias"
    echo
      "I'm not smart enough to build multi-stage docker files without aliases."
    echo "While this is safe to do, you will get no cache benefits"
    echo ""
    echo
      "Please always write your FROM directives as `FROM image:tag as myalias`"


-- | The goal is to create a temporary dockefile in this same folder with the contents
--   if the stage variable, call docker build with the generated file and tag the image
--   so we can find it later.
buildAssetStage :: App -> Stage SourceImage -> Shell ()
buildAssetStage app Stage {..} = do
  printLn
    ("\n--> Building asset stage " % s % " at line " % d % " for the first time"
    )
    stageName
    stagePos
  let
    fromInstruction = filter isFrom directives
    sourceImage     = ImageName (extractFullName fromInstruction)
    cacheEverything = canCacheDirectives directives
    embeddedFiles =
      if cacheEverything then extractCopiedFiles directives else []

    cacheLabels   = buildCacheLabels stageName stageTag embeddedFiles

    newDockerfile = toDockerfile $ do
      if cacheEverything then embed directives else embed fromInstruction
      label (Data.Coerce.coerce cacheLabels)

  doStageBuild app
               sourceImage
               stageImageName
               buildImageName
               cacheLabels
               newDockerfile
 where
  extractFullName (instr : _) = extractFromInstr (instruction instr)
  extractFullName _           = ""

  extractFromInstr (From (DigestedImage img digest _)) =
    prettyImage img <> "@" <> digest
  extractFromInstr (From (UntaggedImage img _)) = prettyImage img
  extractFromInstr (From (TaggedImage img (Tag tag) _)) =
    prettyImage img <> ":" <> tag
  extractFromInstr _ = ""

  prettyImage (Image Nothing               img) = img
  prettyImage (Image (Just (Registry reg)) img) = reg <> "/" <> img


-- | The goal is to create a temporary dockefile in this same folder with the contents
--   if the stage variable, call docker build with the generated file and tag the image
--   so we can find it later.
reBuildAssetStage :: App -> Stage SourceImage -> Stage CachedImage -> Shell ()
reBuildAssetStage app uncached cached = do
  printLn ("\n--> Rebuilding asset stage " % s % " at line " % d)
          (stageName cached)
          (stagePos cached)
  let embeddedFiles = if canCacheDirectives (directives uncached)
        then extractCopiedFiles (directives uncached)
        else []
      cacheLabels = buildCacheLabels (stageName uncached)
                                     (stageTag uncached)
                                     embeddedFiles
  let ImageName t   = stageImageName cached
      newDockerfile = cacheableDockerFile t (directives uncached) cacheLabels
  doStageBuild app
               (buildImageName cached) -- The source image is the one having the ONBUILD lines
               (stageImageName cached)
               (buildImageName cached)
               cacheLabels
               newDockerfile


reBuildFromFallback :: App -> Stage SourceImage -> Stage CachedImage -> Shell ()
reBuildFromFallback app uncached cached = do
  let embeddedFiles = extractCopiedFiles (directives uncached)
      cacheLabels =
        buildCacheLabels (stageName uncached) (stageTag uncached) embeddedFiles
  let sourceImage@(ImageName t) = buildImageName cached
      newDockerfile = cacheableDockerFile t (directives uncached) cacheLabels
  doStageBuild app
               sourceImage
               (stageImageName uncached)
               (buildImageName uncached)
               cacheLabels
               newDockerfile


doStageBuild
  :: App
  -> ImageName source -- ^ This is the image potentially containing the ONBUILD lines, this image needs to exist
  -> ImageName intermediate -- ^ This is the image name to build as intermediate with no ONBUILD
  -> ImageName target -- ^ This is the final image name to build, after appending the ONBUILD lines to intermediate
  -> CacheLabels
  -> Dockerfile
  -> Shell ()
doStageBuild app sourceImageName intermediateImage targetImageName cacheLabels directives
  = do
    -- Only build the FROM
    status <- buildDockerfile app intermediateImage Nothing directives

    -- Break if previous command failed
    guard (status == ExitSuccess)

    ImageConfig _ onBuildLines _ <- Docker.Cacher.Inspect.imageConfig
      sourceImageName

    -- Append the ONBUILD lines to the new file
    newDockerfile <- createDockerfile intermediateImage cacheLabels onBuildLines

    -- Now build it
    finalStatus   <- buildDockerfile app targetImageName Nothing newDockerfile

    -- Stop here if previous command failed
    guard (finalStatus == ExitSuccess)
    echo ""
    echo
      "--> I have tagged a cache container that I can use next time to speed builds!"


-- | Simply call docker build for the passed arguments
buildDockerfile
  :: App -> ImageName a -> Maybe BuildOptions -> Dockerfile -> Shell ExitCode
buildDockerfile (App app) (ImageName imageName) buildOPtions directives = do
  currentDirectory <- pwd
  tmpFile          <- mktempfile currentDirectory "Dockerfile."
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
          ]
          <> [opts]

  -- Put the Dockerfile contents in the tmp file
  liftIO (writeTextFile tmpFile (LT.toStrict (prettyPrint directives)))

  -- Build the generated dockerfile
  shell ("docker " <> Text.intercalate " " allBuildOptions) empty


-- | Given a list of instructions, build a dockerfile where the imageName is the FROM for the file and
--   the list of instructions are wrapped with ONBUILD
createDockerfile :: ImageName a -> CacheLabels -> [Text] -> Shell Dockerfile
createDockerfile (ImageName imageName) (CacheLabels cacheLabels) onBuildLines =
  do
    let eitherDirectives = map parseText onBuildLines
        validDirectives  = rights eitherDirectives -- Just in case, filter out bad directives
        file             = toDockerfile $ do
          from $ toImage imageName `tagged` "latest"
          label cacheLabels
          -- Append each of the ONBUILD instructions
          mapM (onBuildRaw . toInstruction) validDirectives
    return file
 where
  toInstruction [InstructionPos inst _ _] = inst
  toInstruction _                         = error "This is not possible"


--
-- | Returns a list of directives grouped by the appeareance of the FROM directive
--   This will return the group of all stages found in the Dockerfile
getStages :: Dockerfile -> [Dockerfile]
getStages ast = filter startsWithFROM (group ast [])
 where
  group [] acc = reverse acc -- End of recursion
  group (directive@(InstructionPos (From _) _ _) : rest) acc =
    group rest ([directive] : acc) -- Append a new group
  group (directive : rest) [] = group rest [[directive]] -- Create a new group
  group (directive : rest) (current : prev) =
    group rest ((current ++ [directive]) : prev) -- Continue the currently open group

  --
  -- | Returns true if the first element in the list is a FROM directive
  startsWithFROM (InstructionPos (From _) _ _ : _) = True
  startsWithFROM _ = False


-- | Converts a list of instructions into a Stage record
toStage :: App -> Branch -> Maybe Branch -> Dockerfile -> Maybe (Stage a)
toStage (App app) branch fallback directives = do
  (stageName, stageTag, stagePos, stageAlias) <- extractInfo directives -- If getStageInfo returns Nothing, skip the rest
  let newImageName (Branch branchName) =
        app <> "__branch__" <> branchName <> "__stage__" <> sanitize stageName
      stageImageName = ImageName (newImageName branch)
      buildImageName = ImageName (newImageName branch <> "-build")
      stageFallbackImage =
        fmap (\br -> ImageName (newImageName br <> "-build")) fallback
  return Stage {..}
 where
  extractInfo :: Dockerfile -> Maybe (Text, Text, Linenumber, Text)
  extractInfo (InstructionPos { instruction, lineNumber } : _) =
    getStageInfo instruction lineNumber
  extractInfo _ = Nothing

  getStageInfo
    :: Instruction Text -> Linenumber -> Maybe (Text, Text, Linenumber, Text)
  getStageInfo (From (TaggedImage Image { imageName } (Tag tag) (Just (ImageAlias alias)))) pos
    = Just (imageName, tag, pos, alias)
  getStageInfo (From (UntaggedImage Image { imageName } (Just (ImageAlias alias)))) pos
    = Just (imageName, "latest", pos, alias)
  getStageInfo (From (DigestedImage Image { imageName } tag (Just (ImageAlias alias)))) pos
    = Just (imageName, tag, pos, alias)
  getStageInfo _ _ = Nothing

  --
  -- | Makes a string safe to use it as a file name
  sanitize = Text.replace "/" "-" . Text.replace ":" "-"


-- | Given a list of stages and the AST for a Dockerfile, replace all the FROM instructions
--   with their corresponding images as described in the Stage record.
replaceStages :: [Stage CachedImage] -> Dockerfile -> Dockerfile
replaceStages stages = fmap
  (\InstructionPos {..} ->
    InstructionPos {instruction = replaceStage instruction, ..}
  )
 where
  stagesMap = Map.fromList (map createStagePairs stages)

  createStagePairs stage@Stage {..} = (stageAlias, stage)

  --
  -- | Find whehter or not we have extracted a stage with the same alias
  --   If we did, then replace the FROM directive with our own version
  replaceStage directive@(From (TaggedImage _ _ (Just (ImageAlias imageAlias))))
    = replaceKnownAlias directive imageAlias
  replaceStage directive@(From (UntaggedImage _ (Just (ImageAlias imageAlias))))
    = replaceKnownAlias directive imageAlias
  replaceStage directive@(From (DigestedImage _ _ (Just (ImageAlias imageAlias))))
    = replaceKnownAlias directive imageAlias
  replaceStage directive = directive

  replaceKnownAlias directive imageAlias =
    case Map.lookup imageAlias stagesMap of
      Nothing -> directive
      Just Stage { buildImageName, stageAlias } ->
        let ImageName t = buildImageName
        in  From (TaggedImage (toImage t) "latest" (formatAlias stageAlias))

  formatAlias = Just . fromString . Text.unpack


-- | Finds all COPY and ADD instructions in the dockerfile and returns
-- a concatenated list of all the source paths collected
extractCopiedFiles :: Dockerfile -> [(SourcePath, TargetPath)]
extractCopiedFiles = concatMap (extractFiles . instruction)
 where
  extractFiles (Copy CopyArgs { sourcePaths, sourceFlag = NoSource, targetPath })
    = zip (Data.List.NonEmpty.toList sourcePaths) (repeat targetPath)
  extractFiles (Copy CopyArgs { sourceFlag = _ }) = []
  extractFiles (Add AddArgs { sourcePaths, targetPath }) =
    zip (Data.List.NonEmpty.toList sourcePaths) (repeat targetPath)
  extractFiles _ = []


buildCacheLabels :: Text -> Text -> [(SourcePath, TargetPath)] -> CacheLabels
buildCacheLabels imageName imageTag files =
  CacheLabels $ ("cached_image", imageName <> ":" <> imageTag) : case files of
    [] -> []
    _  -> [("cached_files", encodedFiles)]
 where
  encodedFiles = LT.toStrict (Data.Aeson.Text.encodeToLazyText plainTextList)

  plainTextList :: [(Text, Text)]
  plainTextList = Data.Coerce.coerce files


canCacheDirectives :: Dockerfile -> Bool
canCacheDirectives df = not (null cacheLabels)
 where
  cacheLabels =
    [ True
    | Label pairs <- map instruction df
    , (key, val)  <- pairs
    , key == "cache_instructions"
    , val == "cache"
    ]


cacheableDirectives :: Dockerfile -> Dockerfile
cacheableDirectives df = if canCacheDirectives df
  then filter (not . isFrom) . filter (not . isOnBuild) $ df
  else []


cacheableDockerFile :: Text -> Dockerfile -> CacheLabels -> Dockerfile
cacheableDockerFile t directives (CacheLabels cacheLabels) = toDockerfile $ do
  -- Use the cached image as base for the new one
  from (toImage t `tagged` "latest")
  -- But we want the contents of the original one
  -- without the ONBUILD
  embed (cacheableDirectives directives)
  label cacheLabels


isFrom :: InstructionPos args -> Bool
isFrom (InstructionPos From{} _ _) = True
isFrom _                           = False


isOnBuild :: InstructionPos args -> Bool
isOnBuild (InstructionPos OnBuild{} _ _) = True
isOnBuild _                              = False


toImage :: Text -> Image
toImage = fromString . Text.unpack
