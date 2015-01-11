#!/bin/bash -e

DOCKER_CMD=${DOCKER_CMD:-docker}
DOCKER_SAVE_FILE="${DOCKER_SAVE_FILE:-my-reminders.docker.save.tar}"
DOCKER_PROPERTIES_FILE="${DOCKER_PROPERTIES_FILE:-my-reminders.docker.properties}"
SERVICE_ID="${SERVICE_ID:-my-reminders}"
DOCKER_REMOTE="${DOCKER_REMOTE:-docker.atlassian.io}"
DOCKER_REMOTE_REPO="${DOCKER_REMOTE}/atlassian/${SERVICE_ID}"
DOCKER_PUSH_LOCATION="${DOCKER_REMOTE_REPO}:${RELEASE_VERSION}"

if [ ! -f "${DOCKER_PROPERTIES_FILE}" ]
then
   echo "Could not find the docker properties file: $PWD/$DOCKER_PROPERTIES_FILE"
   exit 1
fi

IMAGE_TAG=`grep 'IMAGE_TAG' "${DOCKER_PROPERTIES_FILE}" | sed 's/IMAGE_TAG=//'`
echo "Loaded the IMAGE_TAG: $IMAGE_TAG"

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

${DOCKER_CMD} --version

echo "Listing current images..."
${DOCKER_CMD} images

echo "Logging in to $DOCKER_REMOTE"
${DOCKER_CMD} login -u "$DOCKER_LOGIN_USERNAME" -p "$DOCKER_LOGIN_PASSWORD" -e "$DOCKER_LOGIN_EMAIL" "$DOCKER_REMOTE"

echo "Loading the previously built docker image: $DOCKER_SAVE_FILE"
${DOCKER_CMD} load -i "$DOCKER_SAVE_FILE"

echo "Showing current images..."
${DOCKER_CMD} images

if ! ${DOCKER_CMD} images | grep "$IMAGE_TAG"
then
   echo "Error: the image that was loaded did not match the image tag '$IMAGE_TAG'"
   exit 1
fi

echo "Tagging $IMAGE_TAG as $DOCKER_PUSH_LOCATION"
${DOCKER_CMD} tag -f "$IMAGE_TAG" "$DOCKER_PUSH_LOCATION" 2>&1 > docker-tag.output

echo "Showing current (tagged) images..."
${DOCKER_CMD} images

if ! ${DOCKER_CMD} images | grep "${DOCKER_REMOTE_REPO}"
then
   echo "Error: the image was not tagged successfully. It will not be possible to push image '${DOCKER_PUSH_LOCATION}'"
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
