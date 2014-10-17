#!/bin/bash -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -e

# Usage: build-production
IMAGE_TAG=`git describe`
IMAGE_BUILD_TAG="${IMAGE_TAG}-build"

# TODO build the source docker image
echo "## Building code in image: $IMAGE_BUILD_TAG"
docker build --rm=true --tag="$IMAGE_BUILD_TAG" "${DIR}"
echo "## Built code in image: $IMAGE_BUILD_TAG"
# TODO send the source docker image to production
echo "## Building production image: $IMAGE_TAG"
bash to-production.bash "$IMAGE_BUILD_TAG" "$IMAGE_TAG"
echo "## Built production image: $IMAGE_TAG"

exit 0
