#!/bin/bash -e

CURRENT_IMAGE_TAG=`git describe`
IMAGE_TAG=${IMAGE_TAG:-$CURRENT_IMAGE_TAG}
DOCKER_CMD=${DOCKER_CMD:-docker}
DOCKER_SAVE_FILE="${DOCKER_SAVE_FILE:-my-reminders.docker.save.tar}"
SERVICE_ID="${SERVICE_ID:-my-reminders}"
DOCKER_REMOTE="${DOCKER_REMOTE:-docker.atlassian.io}"
DOCKER_PUSH_LOCATION="${DOCKER_REMOTE}/atlassian/${SERVICE_ID}"

if [ "x$DOCKER_LOGIN_USERNAME" = "x" -o "x$DOCKER_LOGIN_PASSWORD" = "x" -o "x$DOCKER_LOGIN_EMAIL" = "x" ]
then
   echo "You need to provide your docker login details to push to docker.atlassian.io."
   exit 1
fi

${DOCKER_CMD} login -u "$DOCKER_LOGIN_USERNAME" -p "$DOCKER_LOGIN_PASSWORD" -e "$DOCKER_LOGIN_EMAIL" "$DOCKER_REMOTE"
${DOCKER_CMD} load -i "$DOCKER_SAVE_FILE"
${DOCKER_CMD} tag "$IMAGE_TAG" "$DOCKER_PUSH_LOCATION"
${DOCKER_CMD} push "$DOCKER_PUSH_LOCATION"
