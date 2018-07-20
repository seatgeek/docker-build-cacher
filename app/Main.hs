{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           Language.Docker
import           Prelude                 hiding ( FilePath )
import           Text.ParserCombinators.ReadP
import           Text.Read
import           Turtle

import qualified Docker.Cacher
import qualified Docker.Cacher.Internal


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
  Args -- Pass the parsed arguments into the Args data container
    <$> argRead "mode" "Whether to build or to cache (options: build | cache)"
    <*> switch
          "no-cache-stages"
          's'
          "Each of the FROM instruction will be cached in separate images if this flag is not set"
    <*> switch "no-cache-build"
               'n'
               "Skip the internal docker cache when building the image"


main :: IO ()
main = do
  Args { mode, noCacheStages, noBuildCache } <- options  -- Parse the CLI arguments as a Mode
    "Builds a docker file and caches its stages"
    parser

  app            <- Docker.Cacher.App <$> needEnv "APP_NAME" -- Get the APP environment variable and then wrap it in App
  branch         <- Docker.Cacher.Branch <$> needEnv "GIT_BRANCH"
  fallbackBranch <- fmap Docker.Cacher.Branch <$> need "FALLBACK_BRANCH"

  maybeFile      <- do
    file <- need "DOCKERFILE" -- Get the dockerfile path if any
    return (fmap fromText file) -- And transform it to a FilePath

  -- if DOCKERFILE is not present, we assume is in the current directory
  currentDirectory <- pwd
  let Just file = maybeFile <|> Just (currentDirectory </> "Dockerfile")

  -- Now we try to parse the dockefile
  dockerFile <- parseFile (Text.unpack (format fp file)) -- Convert the dockerfile to an AST
  case dockerFile of
    Left message ->
      error ("There was an error parsing the docker file: " <> show message)

    Right ast -> case mode of
      Cache -> if noCacheStages
        then echo "Skipping... I was told not to cache any stages separetely"
        else sh (Docker.Cacher.cacheBuild app branch fallbackBranch ast)

      Build -> do
        name <- Docker.Cacher.Internal.ImageName <$> needEnv "DOCKER_TAG"
        buildOPtions <- do
          opts <- need "DOCKER_BUILD_OPTIONS"
          if noBuildCache
            then
              return
                $ Just
                    ( opts
                    & fromMaybe ""
                    & (<> " --no-cache")  -- Append the docker --no-cache option
                    & Docker.Cacher.BuildOptions
                    )
            else return (fmap Docker.Cacher.BuildOptions opts)

        if noCacheStages
          then sh (Docker.Cacher.build app name buildOPtions ast)
          else sh
            (Docker.Cacher.buildFromCache app
                                          branch
                                          fallbackBranch
                                          name
                                          buildOPtions
                                          ast
            )


needEnv :: MonadIO m => Text -> m Text
needEnv varName = do
  value <- need varName
  case value of
    Nothing -> error
      ("I was expecting the " <> show varName <> " env var to be present.")
    Just val -> return val
