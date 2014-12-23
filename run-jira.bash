#!/bin/bash -e

# From: https://developer.atlassian.com/static/connect/docs/guides/getting-started.html

atlas-run-standalone --product jira --version 6.4-OD-11-014 --bundled-plugins com.atlassian.bundles:json-schema-validator-atlassian-bundle:1.0.4,com.atlassian.webhooks:atlassian-webhooks-plugin:1.0.6,com.atlassian.jwt:jwt-plugin:1.2.2,com.atlassian.upm:atlassian-universal-plugin-manager-plugin:2.18.2-D20141112T015724,com.atlassian.plugins:atlassian-connect-plugin:1.1.17 --jvmargs -Datlassian.upm.on.demand=true $@
