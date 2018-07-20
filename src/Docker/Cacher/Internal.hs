{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Docker.Cacher.Internal where

import           Data.Text                      ( Text )
import           Language.Docker
import           Turtle

newtype ImageName a =
    ImageName Text
    deriving (Show, Eq)

data SourceImage =
    SourceImage

data CachedImage =
    CachedImage

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


printLn :: MonadIO io => Format (io ()) r -> r
printLn message = printf (message % "\n")
