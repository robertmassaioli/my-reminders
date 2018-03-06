#!/bin/bash -e

TEMPLATES_FILE="${TEMPLATES_FILE:-template-vars.csv}"
echo "RELEASE_VERSION" > "${TEMPLATES_FILE}"
echo "${RELEASE_VERSION}" >> "${TEMPLATES_FILE}"

SERVICE_DESCRIPTOR_TEMPLATE="${SERVICE_DESCRIPTOR_TEMPLATE:-service-descriptor.template.json}"
SERVICE_DESCRIPTOR="${SERVICE_DESCRIPTOR:-service-descriptor.json}"

npm install
npm run --silent mustang -- -t "${SERVICE_DESCRIPTOR_TEMPLATE}" -i "${TEMPLATES_FILE}" > "${SERVICE_DESCRIPTOR}"