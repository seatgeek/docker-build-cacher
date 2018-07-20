{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Docker.Cacher.Inspect where

import qualified Control.Foldl                 as Fold
import qualified Data.Aeson                    as Aeson
import           Data.Aeson                     ( (.:) )
import           Data.Either                    ( rights )
import           Data.List.NonEmpty             ( toList )
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                , mapMaybe
                                                )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import qualified Data.Text.Encoding
import           Language.Docker         hiding ( workdir )
import           Language.Docker.Syntax         ( Tag(..) )
import           Prelude                 hiding ( FilePath )
import           Turtle

import           Docker.Cacher.Internal

-- | This represents the image config as stored on disk by the docker daemon. We only care here
-- about the .Config.WorkingDir and .Config.OnBuild properties
data ImageConfig = ImageConfig
    { workingDir :: Text
    , onBuildInstructions :: [Text]
    } deriving (Show, Eq)

data StageCache
    = NotCached (Stage SourceImage) -- When the image has not yet been built separetely
    | FallbackCache (Stage SourceImage) -- When the cache was not present but we looked at a falback key
                    StageCache -- The fallback cache
    | Cached { stage :: Stage CachedImage -- When the image was built and tagged as a separate image
             , cacheBusters :: [(SourcePath, TargetPath)] -- List of files that are able to bust the cache
              }
    | CacheInvalidated (Stage CachedImage)
    deriving (Show)

instance Aeson.FromJSON ImageConfig where
    parseJSON =
        Aeson.withObject "ImageConfig" $ \ic -> do
            workingDir <- ic .: "WorkingDir"
            onBuildInstructions <- fromMaybe [] <$> ic .: "OnBuild"
            return $ ImageConfig {..}


-- | Calls docker inspect for the given image name and returns the config
imageConfig :: ImageName a -> Shell ImageConfig
imageConfig (ImageName name) = do
  printLn ("----> Inspecting the config for the docker image: " % s) name
  out <- fmap lineToText <$> fold fetchConfig Fold.list -- Buffer all the output in the out variable

  case decodeJSON out of
    Left decodeErr -> do
      error
        $  "----> Could not decode the response of docker inspect: "
        ++ decodeErr
      return (ImageConfig "" [])

    Right ic -> return ic
 where
  fetchConfig =
    inproc "docker" ["inspect", "--format", "{{.Config | json}}", name] empty
  decodeJSON =
    Aeson.eitherDecodeStrict . Data.Text.Encoding.encodeUtf8 . Text.unlines


-- | Check whether or not the imageName exists for each of the passed stages
--   and return only those that already exist.
getAlreadyCached :: [Stage SourceImage] -> Shell StageCache
getAlreadyCached stages = do
  echo
    "--> I'm checking whether or not the stage exists as a docker image already"

  stage@Stage { buildImageName, stageFallbackImage } <- select stages -- foreach stages as stage
  exists <- cacheKeyExists stage buildImageName

  if exists
    then do
      echo
        "------> It already exists, so I will then check if the cache files changed"
      inspectCache stage
    else do
      echo "------> It does not exist, so I will need to build it myself later"
      maybe (return (NotCached stage))
            (getFallbackCache stage)
            stageFallbackImage
 where
  cacheKeyExists stage (ImageName imageName) = do
    printLn ("----> Looking for image " % s) imageName

    existent <- fold
      (inproc "docker"
              ["image", "ls", imageName, "--format", "{{.Repository}}"]
              empty
      )
      Fold.list -- Get the output of the command as a list of lines

    if existent == mempty
      then return False
            -- For the cache to be valid, we need to make sure that the stored image is based on the same
            -- base image and tag. Otherwise we will need to rebuild the cache anyway
      else imageAndTagMatches (ImageName (stageName stage))
                              (Tag (stageTag stage))
                              imageName
  --
  --
  getFallbackCache stage fallbackName = do
    exists <- cacheKeyExists stage fallbackName

    if exists
      then do
        echo
          "------> The fallback image exists, using it to build the initial cache"
        cachedStage <- inspectCache
          (stage { stageImageName = fallbackName
                 , buildImageName = fallbackName
                 }
          )
        return (FallbackCache stage cachedStage)
      else do
        echo "------> There is not fallback cache image"
        return (NotCached stage)


-- | Here check each of the cache buster from the image and compare them with those we have locally,
--   if the files do not match, then we return the stage back as a result, otherwise return Nothing.
shouldBustCache :: StageCache -> Shell StageCache
shouldBustCache cached@Cached {..} = do
  printLn ("----> Checking cache buster files for stage " % s) (stageName stage)
  withContainer (buildImageName stage) checkFiles -- Create a container to inspect the files
 where
  checkFiles containerId = do
    hasChanged <- fold
      (mfilter isJust (checkFileChanged containerId cacheBusters))
      Fold.head
    -- ^ Get the cache buster files that have changed since last time

    -- The following is executed for each of the files found
    if isJust hasChanged
      then do
        printLn ("----> The stage " % s % " changed") (stageName stage)
        return (CacheInvalidated stage)
      else do
        printLn ("----> The stage " % s % " did not change") (stageName stage)
        return cached

  -- |
  checkFileChanged containerId files = do
    (SourcePath src, TargetPath dest) <- select files
    let file      = fromText src
    let targetDir = fromText dest
    printLn ("------> Checking file '" % fp % "' in directory " % fp)
            file
            targetDir
    currentDirectory <- pwd
    tempFile         <- mktempfile currentDirectory "comp"
    let targetFile = targetDir </> file
    status <- proc
      "docker"
      ["cp", format (s % ":" % fp) containerId targetFile, format fp tempFile]
      empty
    guard (status == ExitSuccess)
    local  <- liftIO (readTextFile file)
    remote <- liftIO (readTextFile tempFile)
    if local == remote then return Nothing else return (Just file)
-- In any other case return the same inspected stage
shouldBustCache c@NotCached{}        = return c
shouldBustCache c@CacheInvalidated{} = return c
shouldBustCache c@FallbackCache{}    = return c


-- | This will inspect how an image was build and extrack the ONBUILD directives. If any of those
--   instructions are copying or adding files to the build, they are considered "cache busters".
inspectCache :: Stage SourceImage -> Shell StageCache
inspectCache sourceStage@Stage {..} = do
  ImageConfig workdir onBuildLines <- imageConfig buildImageName
  let parsedDirectivesWithErrors = fmap parseText onBuildLines -- Parse each of the lines
      parsedDirectives = (getFirst . rights) parsedDirectivesWithErrors -- We only keep the good lines
      cacheBusters = extractCachePaths (fromText workdir) parsedDirectives
  return $ Cached (toCachedStage sourceStage) cacheBusters
 where
  extractCachePaths workdir dir =
    let filePairs = concat (mapMaybe doExtract dir) -- Get the (source, target) pairs of files copied
    in  fmap (prependWorkdir workdir) filePairs -- Some target paths need to have the WORKDIR prepended

  --
  -- | Prepend a given target dir to the target path
  prependWorkdir workdir (source, TargetPath target) =
    let dest          = fromText target
        prependedDest = format fp (collapse (workdir </> dest))
    in  if relative dest -- If the target path is relative, we need to prepend the workdir
          then (source, TargetPath prependedDest) -- Remove the ./ prefix and prepend workdir
          else (source, TargetPath target)

  --
  -- | COPY allows multiple paths in the same line, we need to convert each to a separate path
  doExtract (InstructionPos (Copy CopyArgs { sourcePaths, targetPath }) _ _) =
    Just (zip (toList sourcePaths) (repeat targetPath))
  --
  -- | This case is simpler, we only need to convert the source and target from ADD
  doExtract (InstructionPos (Add AddArgs { sourcePaths, targetPath }) _ _) =
    Just (zip (toList sourcePaths) (repeat targetPath))
  doExtract _ = Nothing

  getFirst (first : _) = first
  getFirst []          = []


toCachedStage :: Stage SourceImage -> Stage CachedImage
toCachedStage Stage {..} =
  let stage                = Stage {..}
      ImageName sImageName = stageImageName
      ImageName bImageName = buildImageName
  in  stage { stageImageName     = ImageName sImageName
            , stageFallbackImage = Nothing
            , buildImageName     = ImageName bImageName
            }


-- | Extracts the label from the cached image passed in the last argument and checks
-- if it matches the passeed image name and tag name. This is used to avoid using a
-- cached image tht was built using a different base iamge
imageAndTagMatches :: ImageName Text -> Tag -> Text -> Shell Bool
imageAndTagMatches (ImageName imageName) (Tag tagName) cachedImage = do
  printLn ("------> Checking the stored cached key in a label for " % s)
          cachedImage
  value <- fold getCacheLabel Fold.head -- Get the only line
  let expected = unsafeTextToLine (imageName <> ":" <> tagName)
  return (Just expected == value)
 where
  getCacheLabel = inproc
    "docker"
    [ "inspect"
    , "--format"
    , "{{ index .Config.Labels \"cached_image\"}}"
    , cachedImage
    ]
    empty


alreadyCached :: StageCache -> Maybe (Stage CachedImage)
-- We want to replace stages where the cache
-- was invalidated by any file changes.
alreadyCached (CacheInvalidated stage) = Just stage

-- Likewise, once we have a cached stage, we need to keep using it
-- in succesive builds, so the cache is not invalidated again.
alreadyCached (Cached stage _) = Just stage

-- Finally, if we are using a fallback, we apply
-- the same rules as above for the fallback key
alreadyCached (FallbackCache _ (CacheInvalidated stage)) = Just stage

alreadyCached (FallbackCache _ (Cached stage _)) = Just stage

alreadyCached _ = Nothing


-- | Creates a container from a stage and passes the container id to the
--   given shell as an argument
withContainer :: ImageName a -> (Text -> Shell b) -> Shell b
withContainer (ImageName imageName) action = do
  containerId <- inproc "docker" ["create", imageName] empty
  result      <- fold (action (format l containerId)) Fold.list
  _           <- removeContainer containerId -- Ignore the return code of this command
  select result -- yield each result as a separate line
 where
  removeContainer containerId =
    proc "docker" ["rm", format l containerId] empty
