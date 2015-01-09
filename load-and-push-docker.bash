#!/bin/bash -e

CURRENT_IMAGE_TAG=`git describe`
IMAGE_TAG=${IMAGE_TAG:-$CURRENT_IMAGE_TAG}
DOCKER_CMD=${DOCKER_CMD:-docker}
DOCKER_SAVE_FILE="${DOCKER_SAVE_FILE:-my-reminders.docker.save.tar}"
SERVICE_ID="${SERVICE_ID:-my-reminders}"
DOCKER_REMOTE="${DOCKER_REMOTE:-docker.atlassian.io}"
DOCKER_PUSH_LOCATION="${DOCKER_REMOTE}/atlassian/${SERVICE_ID}:${RELEASE_VERSION}"

if [ "x$DOCKER_LOGIN_USERNAME" = "x" -o "x$DOCKER_LOGIN_PASSWORD" = "x" -o "x$DOCKER_LOGIN_EMAIL" = "x" ]
then
   echo "You need to provide your docker login details to push to docker.atlassian.io."
   exit 1
fi

if [ "x$RELEASE_VERSION" == "x" ]
then
   echo "You need to provide a release version."
   exit 1
fi

echo "Logging in to $DOCKER_REMOTE"
${DOCKER_CMD} login -u "$DOCKER_LOGIN_USERNAME" -p "$DOCKER_LOGIN_PASSWORD" -e "$DOCKER_LOGIN_EMAIL" "$DOCKER_REMOTE"

echo "Loading the previously built docker image: $DOCKER_SAVE_FILE"
${DOCKER_CMD} load -i "$DOCKER_SAVE_FILE"

echo "Tagging $IMAGE_TAG as $DOCKER_PUSH_LOCATION"
${DOCKER_CMD} tag -f "$IMAGE_TAG" "$DOCKER_PUSH_LOCATION" 2>&1 > docker-tag.output

if grep 'level="fatal"' docker-tag.output
then
   echo "Error: docker tag experienced a fatal error."
   exit 1
fi

echo "Pussing $DOCKER_PUSH_LOCATION to $DOCKER_REMOTE"
${DOCKER_CMD} push "$DOCKER_PUSH_LOCATION" 2>&1 > docker-push.output

if grep 'level="fatal"' docker-push.output
then
   echo "Error: docker push experienced a fatal error."
   exit 1
fi

echo "Successfully pushed docker image to $DOCKER_REMOTE!"
