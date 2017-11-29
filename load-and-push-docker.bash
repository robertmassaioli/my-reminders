#!/bin/bash -e

echo "Starting the load and push of the docker image..."

DOCKER_CMD=${DOCKER_CMD:-docker}
DOCKER_SAVE_FILE="${DOCKER_SAVE_FILE:-my-reminders.docker.save.tar}"
DOCKER_PROPERTIES_FILE="${DOCKER_PROPERTIES_FILE:-my-reminders.docker.properties}"
SERVICE_ID="${SERVICE_ID:-my-reminders}"
DOCKER_REMOTE="${DOCKER_REMOTE:-docker.atl-paas.net}"
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
   echo "You need to provide your docker login details to push to docker.atl-paas.net."
   exit 1
fi

if [ "x$RELEASE_VERSION" == "x" ]
then
   echo "You need to provide a release version."
   exit 1
fi

echo "Printing the docker version"
${DOCKER_CMD} --version

echo "Listing current images..."
${DOCKER_CMD} images

echo "Cleaning up all images and containers..."
# Delete all containers
${DOCKER_CMD} rm -f $(${DOCKER_CMD} ps -a -q)
# Delete all images
${DOCKER_CMD} rmi -f $(${DOCKER_CMD} images -q)

echo "Listing cleansed images..."
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
${DOCKER_CMD} tag "$IMAGE_TAG" "$DOCKER_PUSH_LOCATION" 2>&1 > docker-tag.output

echo "Showing current (tagged) images..."
${DOCKER_CMD} images

if ! ${DOCKER_CMD} images | grep "${DOCKER_REMOTE_REPO}"
then
   echo "Error: the image was not tagged successfully. It will not be possible to push image '${DOCKER_PUSH_LOCATION}'"
   exit 1
fi

echo "Pushing $DOCKER_PUSH_LOCATION to $DOCKER_REMOTE"
${DOCKER_CMD} push "$DOCKER_PUSH_LOCATION" 2>&1 > docker-push.output

if grep 'level="fatal"' docker-push.output
then
   echo "Error: docker push experienced a fatal error."
   exit 1
fi

#CHECK_URL="https://${DOCKER_REMOTE}/v1/repositories/atlassian/${SERVICE_ID}/tags/${RELEASE_VERSION}"
#echo "Checking the docker repository to see if the push succeeded by hitting: ${CHECK_URL}"
#if ! curl -f -q -u "${DOCKER_LOGIN_USERNAME}:${DOCKER_LOGIN_PASSWORD}" "${CHECK_URL}"
#then
#   echo "Error: the docker push seems to have failed because it is not present in the repository."
#   exit 2
#fi

echo "Successfully pushed docker image to $DOCKER_REMOTE!"
