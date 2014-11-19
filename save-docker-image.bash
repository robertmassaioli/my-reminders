#!/bin/bash -e

# This script saves a docker image. It assumes that docker image was build from this
# commit using the build-production.bash script

CURRENT_IMAGE_TAG=`git describe`
IMAGE_TAG=${IMAGE_TAG:-$CURRENT_IMAGE_TAG}
DOCKER_CMD=${DOCKER_CMD:-docker}
DOCKER_SAVE_FILE="${DOCKER_SAVE_FILE:-my-reminders.docker.save.tar}"

${DOCKER_CMD} save -o "$DOCKER_SAVE_FILE" "$IMAGE_TAG"
