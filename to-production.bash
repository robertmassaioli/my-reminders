#!/bin/bash -e

# The purpose of this script is to build the production ready docker container and make sure that it
# works.

# TODO make sure that boot2docker has been run on linux
# TODO setup the proper docker config
mkdir -p production/to-copy
cp -R resources production/to-copy
cd production

echo "## Getting the required dependencies out of the build..."
CONTAINER_ID=`docker ps -lq`
time docker cp "$CONTAINER_ID:/home/haskell/build/.cabal-sandbox/bin/ping-me-connect" to-copy

echo "## Building the production Docker image."
time docker build .

echo "## Finished successfully"
