import fs from 'fs';

async function main(): Promise<void> {
   let bitbucketBuildNumber: string | undefined = process.env.BITBUCKET_BUILD_NUMBER;

   if (bitbucketBuildNumber === undefined) {
      console.log("Bitbucket build number not present. Defaulting to 'latest' tag.");
      bitbucketBuildNumber = 'latest';
   }

   const sd = {
      buildNumber: bitbucketBuildNumber,
      compose: {
         myReminders: {
            image: "docker.atl-paas.net/sox/atlassian/my-reminders",
            tag: bitbucketBuildNumber,
            ports: ["8080:8080"],
            links: ["cryptor-sidecar"]
         },
         "cryptor-sidecar": {
            image: "docker.atl-paas.net/sox/cryptor-sidecar-application",
            tag: "1.1-stable-release"
         }
      },
      description: "My reminders allows you to set reminders on issues.",
      name: "My Reminders",
      organization: "RD:Customer Purchasing Platform",
      computeClassification: {
         dataType: [
            "UGC/PrimaryIdentifier",
            "UGC/Primary",
            "PII/DirectRestrictedIdentifier",
            "Security/Credential"
         ]
      },
      network: {
         egress: "internet-only"
      },
      resources: [{
         type: "postgres-db",
         name: "encrypted-my-reminders",
         attributes: {
            dataType: [
               "UGC/PrimaryIdentifier",
               "UGC/Primary",
               "PII/DirectRestrictedIdentifier",
               "Security/Credential"
            ]
         }
      }, {
         type: "globaledge",
         name: "ingress",
         attributes: {
            ip_whitelist: [
               "public"
            ]
         }
      }, {
         type: "cryptor",
         name: "secret-data",
         attributes: {
            parameters: {
               encryptingServices: ['my-reminders'],
               decryptingServices: ['my-reminders']
            }
         }
      }],
      environmentOverrides: {
         dev: {
            resources: [{
               type: "cronman",
               name: "send-due-reminders",
               lifecycle: "deployment",
               attributes: {
                  type: "interval",
                  expression: "PT3M",
                  timezone: "UTC",
                  notificationMethod: "http_post",
                  notificationUrl: "/rest/expire?key=c1de7ddc-3a91-11e7-85fb-3c15c2bd8cac"
               }
            }, {
               type: "cronman",
               name: "expire-failing-reminders",
               lifecycle: "deployment",
               attributes: {
                  type: "interval",
                  expression: "PT1H",
                  timezone: "UTC",
                  notificationMethod: "http_post",
                  notificationUrl: "/rest/expire/failing?key=c1de7ddc-3a91-11e7-85fb-3c15c2bd8cac"
               }
            }, {
               type: "cronman",
               name: "tenant-purge",
               lifecycle: "deployment",
               attributes: {
                  type: "interval",
                  expression: "P1D",
                  timezone: "UTC",
                  notificationMethod: "http_post",
                  notificationUrl: "/rest/purge?key=957784e5-b6d8-4a96-bdc3-7a62826aa67f"
               }
            }],
            scaling: {
               autoHibernation: {
                  enabled: false
               }
            },
            config: {
               environmentVariables: {
                  CONNECT_BASE_URL: "https://my-reminders.dev.services.atlassian.com"
               }
            }
         },
         prod: {
            resources: [{
               type: "cronman",
               name: "send-due-reminders",
               lifecycle: "deployment",
               attributes: {
                  type: "interval",
                  expression: "PT3M",
                  timezone: "UTC",
                  notificationMethod: "http_post",
                  notificationUrl: "/rest/expire?key=64d8d3d8-6fb6-11e4-b54d-3c15c2bd8cac"
               }
            }, {
               type: "cronman",
               name: "expire-failing-reminders",
               lifecycle: "deployment",
               attributes: {
                  type: "interval",
                  expression: "PT1H",
                  timezone: "UTC",
                  notificationMethod: "http_post",
                  notificationUrl: "/rest/expire/failing?key=64d8d3d8-6fb6-11e4-b54d-3c15c2bd8cac"
               }
            }, {
               type: "cronman",
               name: "tenant-purge",
               lifecycle: "deployment",
               attributes: {
                  type: "interval",
                  expression: "P1D",
                  timezone: "UTC",
                  notificationMethod: "http_post",
                  notificationUrl: "/rest/purge?key=6b518e30-6fb6-11e4-8091-3c15c2bd8cac"
               }
            }],
            config: {
               environmentVariables: {
                  CONNECT_BASE_URL: "https://my-reminders.services.atlassian.com"
               }
            }
         }
      },
      links: {
         healthcheck: {
            uri: "rest/heartbeat"
         },
         source: {
            url: "https://bitbucket.org/atlassianlabs/my-reminders"
         }
      },
      owners: [
         "rmassaioli@atlassian.com",
         "ekaukonen@atlassian.com"
      ],
      notifications: {
         pagerduty: {
            cloudwatch: "https://events.pagerduty.com/adapter/cloudwatch_sns/v1/124e0f010f214a9b9f30b768e7b18e69",
            apiKey: "5d11612f25b840faaf77422edeff9c76"
         },
         email: "rmassaioli@atlassian.com"
      },
      alarms: {
         overrides: {
            WebServerDiskSpaceUtilizationAlarmHigh: {
               Priority: "Low"
            },
            HighSeverityAlarmWhenTooManyBackend5xxErrors: {
               Priority: "Low"
            },
            HighSeverityAlarmWhenTooManyELB5xxErrors: {
               Priority: "Low"
            },
            HealthyHostCount: {
               Priority: "Low"
            }
         }
      },
      scaling: {
         min: 2
      },
      downstreamServices: [],
      cleanup: false,
      config: {
         environmentVariables: {
            CRYPTOR_SIDECAR_ENCRYPTION_KEY_ALIASES: "micros/my-reminders/secret-data"
         }
      }
   };

   const sdOutFile = 'service-descriptor.json';
   fs.writeFileSync(sdOutFile, JSON.stringify(sd, null, 2));
   console.log(`Wrote service descriptor to: ${sdOutFile}`);
}

main().catch(e => console.error(e));