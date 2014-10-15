#!/bin/bash -e

# The purpose of this script is to build the production ready docker container and make sure that it
# works.

# Usage:
# 1: The name of the image to deploy

IMAGE_BUILD_NAME="$1"
IMAGE_NAME="$2"

if [ "x$1" == "x" ]
then
   echo "You need to provide a docker image!"
   exit 2
fi


set -e
echo "## Will fail immediately on error..."

# TODO make sure that boot2docker has been run on linux
# TODO setup the proper docker config
COPY_DIR="production/to-copy"
rm -fr "$COPY_DIR"
mkdir -p "$COPY_DIR"
cp -R resources "$COPY_DIR"

echo "## Getting the required dependencies out of the build..."
if [ "x$CONTAINER_ID" == "x" ]
then
   echo "## Using image $IMAGE_NAME for dependencies...starting and stopping immediately."
   docker run "$IMAGE_NAME" echo "### Started and stopped immediately..."
   CONTAINER_ID=`docker ps -lq`
   echo "## Using last run container: $CONTAINER_ID"
fi

echo "## Searching for container: $CONTAINER_ID"
if docker ps -aq | grep "$CONTAINER_ID" > /dev/null
then
   echo "## Using container: $CONTAINER_ID"
else
   echo "## Failed to find container '$CONTAINER_ID'. Quitting"
   exit 1
fi

# It is important to note that only copying this file implicitly implies that we will statically
# compile the binary.
time docker cp "$CONTAINER_ID:/home/haskell/build/.cabal-sandbox/bin/ping-me-connect" "$COPY_DIR"

echo "## Building the production Docker image."
cd production
time docker build --rm="true" --tag="$IMAGE_NAME" .

echo "## Finished successfully"
set +e
