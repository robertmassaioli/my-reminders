#!/bin/bash -e

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

set -e
echo "## Will fail immediately on error..."

# Usage: build-production
# 1: Tag Name
IMAGE_TAG=`git describe`

# TODO build the source docker image
docker build --rm=true --tag="$IMAGE_TAG" "${DIR}"
# TODO send the source docker image to production
bash to-production.bash "$IMAGE_TAG"
