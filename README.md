# My Reminders for Cloud

The My Reminders plugin for JIRA (and eventually the other products too) is an Atlassian Connect addon
that has been designed to complete the notification flow that is missing in Atlassian products. You
can be notified of changes to things that you are interested in but you don't get notifications for
items that do not change.

Every now and then you want to be notified about an important issue that MUST be completed before a
particular date, for example, if another team promised you that a particular issue would be resolved
by a certain date then you would really like to be reminded to check up on that issue by the given
date. That is what the My Reminders plugin for JIRA solves, it will remind you of an issue in a given
amount of time.

## Screenshots

### Create a Reminder

![Creating a reminder](https://marketplace-cdn.atlassian.com/files/images/3cdcd53d-7492-4378-ac0c-749283ae2028.png)


### Recieve an Email Reminder

![Recieve an Email Reminder](https://marketplace-cdn.atlassian.com/files/images/ef111287-5d77-49a7-ae67-cc596b71dd26.png)

### Manage your Reminders

![Manage your Reminders](https://marketplace-cdn.atlassian.com/files/images/b268f65e-0f7f-4dd7-8cd4-f2dd42b15b7e.png)

## Developing the Atlassian Connect Addon

### Database setup

You'll need a postgreSQL server available, matching the access configurations in
`snaplets/postgresql-simple/devel.cfg`.

There is a convenience script at `schema/bootstrap.sh` to set up the database for you, assuming that
the current user has permission to create databases.

### Running the plugin

First setup a nix-shell environment with the haskell stack command:

``` shell
nix-shell -p postgresql zlib jdk stack libiconv
```

Then use the stack command to build the service:

``` shell
stack build
```

To continuously build the service while developing:

``` shell
stack build --file-watch
```

To run the tests:

``` shell
stack test
```

And to run the service:

``` shell
stack run my-reminders
```

To run the service in a development watch mode:

``` shell
CONNECT_BASE_URL=https://95e354fe3704.ngrok.io yarn dev-watch
```

You can test that the App is running by hitting: http://localhost:8000/atlassian-connect.json

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

My Reminders is built against the Haskell Platform, currently version `2013.2.0.0`.
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
    
## Deploying this service to production

Now that you have built the docker container you are going to want to deploy this service to
production. Just place the docker container somewhere and give it the following environment
variables:

 - http\_proxy: Required if you need to route outgoing requests through a HTTP proxy.
 - https\_proxy: Required if you need to route outgoing requests through a HTTPS proxy.
 - PG\_MY\_REMINDERS\_HOST: The PostgreSQL host.
 - PG\_MY\_REMINDERS\_PORT: The PostgreSQL port.
 - PG\_MY\_REMINDERS\_SCHEMA: The PostgreSQL database name.
 - PG\_MY\_REMINDERS\_ROLE: The PostgreSQL role that has access to that database.
 - PG\_MY\_REMINDERS\_PASSWORD: The password of the role.
 - CONNECT\_BASE\_URL: The base url that should appear in your Atlassian Connect Descriptor.
 - CONNECT\_SECRET\_KEY: The secret key that Atlassian Connect will use for page tokens.
 - EXPIRE\_KEY: The key to pass to the service to give access to the expire handler.
 - PURGE\_KEY: The key to pass to the service to give access to the purge handler.
 - MIGRATION\_KEY: The key to pass to the service to give access to the migration handler.

If you have properly set these environment variables then the service will be up and running in more
time. For greater control you may want to edit the configuration properties in the resources
directory.