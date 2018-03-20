#!/bin/bash -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -e

# Usage: build-production
IMAGE_TAG=`git describe`
DOCKER_CMD=${DOCKER_CMD:-docker}

echo "## Building production image: $IMAGE_TAG"
time ${DOCKER_CMD} build --rm="true" --tag="$IMAGE_TAG" "${DIR}"
echo "## Built production image: $IMAGE_TAG"

exit 0
