# Docker Build Cacher

This tool is intended to speedup multi-stage Dockerfile build times by caching the results of each of the
stages separately.

## Why?

[Multi-stage docker file](https://docs.docker.com/engine/userguide/eng-image/multistage-build/) builds are great,
but they still miss a key feature: It is not possible to carry from one build to another the statically generated
cache files once the source file in your project change. Here's an example that illustrates the issue:

Imagine you create a generic Dockerfile for building node projects

```Dockerfile
FROM nodejs

RUN apt-get install nodejs yarn

WORKDIR /app

# Whenever this image is used execute these triggers
ONBUILD ADD package.json yarn.lock .
ONBUILD RUN yarn
ONBUILD RUN yarn run dist
```

And then you call

```bash
docker build -t nodejs-build .
```

So now you can use the `nodejs-build` image in other builds, like this:

```Dockerfile
# Automatically build yarn dependencies
FROM nodejs-build as nodedeps

# Build the final container image
FROM scratch

# Copy the generated app.js from yarn run dist
COPY --from=nodedeps /app/app.js .
...
```

So far so good, we have build a pretty lean docker image that discards all the `node_modules`
folder and only keeps the final artifact. For example a bundled reactjs application.

It's also very fast to build! Since each of the steps in the Dockerfile are cached, as long as
none of the files changed.

But that's also where the problem is: Whenever `package.json` or `yarn.lock` files change, docker
will trash all the files in `node_modules` and all the cached yarn packages and will start from
scratch downloading, linking and building every single dependency.

That's far from ideal. What if we could do a change in the process so that changes to those files
do not bust the yarn cache? It turns out that we can!

## Enter docker-build-cacher

This utility overcomes the problem by providing a way to build the docker file and then cache the
intermediate stages. On subsequent builds, it will make sure that the static cache files generated
during previous builds will also be present.

The effect it has should be obvious: your builds will be consistently fast, at the cost of more disk space.

## Installation

There are binaries provided for `linux-x86_64` and MacOS, check
[the releases page](https://github.com/seatgeek/docker-build-cacher/releases) for downloads.

## Usage

`docker-build-cacher` requires the following environment variables to be present in order to correctly build
your Dockerfile:

* `APP_NAME`: The name for application you are trying to build. Usually this is just the folder name you are in.
* `GIT_BRANCH`: The name of the git branch you are building. Used to "namespace" cache results
* `DOCKER_TAG`: It will `docker build -t $DOCKER_TAG .` at some point. Let it know the image tag you want at the end.

This utility has two modes, `Build` and `Cache`. Both modes should be invoked for the cache to work:

```bash
# APP_NAME ispassed as argument in the build process, you can use it as an env var in your Dockerfile
export APP_NAME=fancyapp

# GIT_BRANCH is used as part of the named for the resulting cached image
export GIT_BRANCH=master

# DOCKER_TAG corresponds to the -t argument in docker build, that will be the resulting image name
export DOCKER_TAG=fancyapp:latest

docker-build-cacher build # This will build the docker file
docker-build-cacher cache # This will cache each of the stage results separately
```

Additionally, `docker-build-cacher` accepts the `DOCKERFILE` env variable in case the file is not present in the
current directory:

```bash
DOCKERFILE=buildfiles/Dockerfile docker-build-cacher build
```

At the end of the process you can call `docker images` and see that it has created `fancyapp:latest`, and if you are using
multi-stage builds, it should have created an image tag for each of the stages in your Dockerfile

### Fallback Cache Keys


As mentioned before the `GIT_BRANCH` env variable is used as part of the name for the generated cached image, this means that
the generated cache is scope to that name. This is done so you can keep different caches where you can experiment with widly
different requirements and libraries in the dockerfile.

This has the unfortunate side effect that building other branches will require building the cache from scratch. In order to solve this
you can use the `FALLBACK_BRANCH` environment variable like this:

```bash
export APP_NAME=fancyapp
export GIT_BRANCH=my-feature
export FALLBACK_BRANCH=master
export DOCKER_TAG=fancyapp:latest

docker-build-cacher build
docker-build-cacher cache
```

The above will make the cached image for the `my-feature` branch to be based on the one from the `master` branch.

## How It Works

This works by parsing the Dockerfile and extracting the `COPY` or `ADD` instructions nested inside `ONBUILD` for each of
the stages found in the file.

It will compare the source files present in such `COPY` or `ADD` instructions to check for changes. If it can detect changes,
it rewrites your Dockerfile on the fly so that the `FROM` directives in each of the stages use the locally cached images instead
of the original base image.

The effect this `FROM` swap has, is that disk state for the image is preserved between builds.

## Passing extra arguments to docker build

It is possible to pass extra arguments and flags to the `docker build` step by providing the environment variable `DOCKER_BUILD_OPTIONS` as
shown below:


```bash
DOCKER_BUILD_OPTIONS="--build-arg foo=bar --quiet" docker-build-cacher build
```

## Building from source

Dependencies:

- [Haskell stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)

Install the `stack` tool from the link above. Then `cd` to the root folder of this repo and execute:

```sh
stack setup
stack install
```

If it is the first time, it will take *a lot* of time. Don't worry, it's only once you need to pay this price.
