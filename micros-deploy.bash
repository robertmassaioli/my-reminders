#!/bin/bash -e

MICROS_CMD=${MICROS_CMD:-./node_modules/@atlassian/micros-cli/micros.js}

if [ "x$DEPLOY_ENVIRONMENT" = "x" ]
then
   echo "You need to specify the deployment environment!"
   exit 1
fi

MICROS_USER=${bamboo_micros_user} \
MICROS_PASSWORD=${bamboo_micros_password} \
MICROS_TOKEN=${bamboo_micros_token} \
MICROS_URL=${bamboo_micros_url} \
${MICROS_CMD} service:deploy -v -e ${DEPLOY_ENVIRONMENT} -f service-descriptor.json my-reminders
