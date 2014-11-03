# Remind Me for JIRA

The Remind Me plugin for JIRA (and eventually the other products too) is an Atlassian Connect addon
that has been designed to complete the notification flow that is missing in Atlassian products. You
can be notified of changes to things that you are interested in but you don't get notifications for
items that do not change.

Every now and then you want to be notified about an important issue that MUST be completed before a
particular date, for example, if another team promised you that a particular issue would be resolved
by a certain date then you would really like to be reminded to check up on that issue by the given
date. That is what the Remind Me plugin for JIRA solves, it will remind you of an issue in a given
amount of time.

## Developing the Atlassian Connect Addon

### Database setup

You'll need a postgreSQL server available, matching the access configurations in
`snaplets/postgresql-simple/devel.cfg`.

There is a convenience script at `schema/bootstrap.sh` to set up the database for you, assuming that
the current user has permission to create databases.

### Running the plugin

When developing this application you will need to have both the Addon and Atlassian Product running.
To set up the Remind Me addon:

 1. Execute `make`.
 1. Run the application: .cabal-sandbox/bin/remind-me-connect 
    (by default compiled in development mode, which will disable warnings)
 1. Make changes to the application files as you develop.
 1. Reload in the browser, updated files will be compiled automatically.

You can test that the plugin is running by hitting: http://localhost:8000/atlassian-connect.json

However this has only setup half of the picture, to run the Addon in a product locally do the
following:

 1. Run the provided script: `bash run-jira.bash`
 1. Go to http://localhost:2990/jira/plugins/servlet/upm#manage and log in as the admin user. (Credentials: admin / admin)
 1. Click on the 'Settings' link at the bottom of the panel.
 1. Click 'Enable private listings' and hit 'Apply'.
 1. When the page has finished reloading hit 'Upload add-on' and use the url
    http://localhost:8000/atlassian-connect.json to install the Atlassian Connect plugin into your
    running version of JIRA.

Congratulations, at this point in time you should have a working development environment and you
should be able to develop on this Atlassian Connect plugin. Woo! Good luck!

Note: This will have built the project in `development` mode,  which enables dynamic reloading. To
run without dynamic reloading then just use:

    make setup

And run the service again.

## Deploying with Docker

This project has opted to use Docker as the deployment mechanism. To build the development docker
image run the following in the root directory of the project:

    time docker build .

Using the time command means that you can get a feel for how long the docker build will take on your
machine. Once you have the docker build you can make the production docker image by:

    CONTAINER_ID=XXXX bash to-production.bash

Which will copy the build image from the build container and pass it through to the production
container. It will also pull through all of the required resources. This production image should be
possible to deploy independently of everything else. The reason that we have this separation is that
you require > 2GB of image size to create the build docker container and only ~270MB of image size
to create the production container. This allows us to have much more efficient production
deployments.

### Local testing with Docker

The addon will reject installation requests from hosts that aren't whitelisted. If your JIRA
installation has a base URL with a hostname other than "localhost", the hostname of the docker image,
or one of the Atlassian OnDemand public domains, you will need to modify the whitelist (found in
`src/Connect/Connect.hs`).

You will also need to modify `snaplets/postgresql-simple/devel.cfg` to point to your database from
the Docker container, as the database will not be running in the addon's Docker image.

## Dependencies

Remind Me is built against the Haskell Platform, currently version `2013.2.0.0`.
The latest version of Cabal is always recommended, we have tested with at least
`cabal-install version 1.20.0.2`.

Additional dependencies required on `PATH` include:

- `pg_config` (e.g. `apt-get install libpq-dev` on Debian/Ubuntu)

## Running the Code

There is a useful trick where you can hit /admin/reload on localhost and refresh the entire service
without having to restart the process. This is excellent for rapid development.

## Optional dependencies

These are not strictly necessary but make life easier.

### UPX

Compresses the executable. `make dist` currently strips debug symbols _and_ compresses the executable.

    » brew install upx
    
### cabal-constraints

The `make freeze` target "freezes" the exact versions of the dependencies selected by cabal-install. The dependency list will be written to `cabal.config`.
This requires the `cabal-constraints` executable to be available:

    » cabal install cabal-constraints
    
