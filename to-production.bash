#!/bin/bash -e

# The purpose of this script is to build the production ready docker container and make sure that it
# works.

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
   CONTAINER_ID=`docker ps -lq`
   echo "## Could not find container id. Using last container: $CONTAINER_ID"
fi

echo "## Searching for container: $CONTAINER_ID"
if docker ps -aq | grep "$CONTAINER_ID" > /dev/null
then
   echo "## Using container: $CONTAINER_ID"
else
   echo "## Failed to find container '$CONTAINER_ID'. Quitting"
   exit 1
fi

time docker cp "$CONTAINER_ID:/home/haskell/build/.cabal-sandbox/bin/ping-me-connect" "$COPY_DIR"

echo "## Building the production Docker image."
cd production
time docker build .

echo "## Finished successfully"
set +e
