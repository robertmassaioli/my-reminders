#!/bin/bash -e

# From: https://developer.atlassian.com/static/connect/docs/guides/getting-started.html

atlas-run-standalone --product jira --version 6.4-OD-05-009 --bundled-plugins com.atlassian.jwt:jwt-plugin:1.1.0,com.atlassian.bundles:json-schema-validator-atlassian-bundle:1.0.4,com.atlassian.webhooks:atlassian-webhooks-plugin:1.0.6,com.atlassian.upm:atlassian-universal-plugin-manager-plugin:2.17.14-D20140902T224549,com.atlassian.plugins:atlassian-connect-plugin:1.1.4 --jvmargs -Datlassian.upm.on.demand=true $@
