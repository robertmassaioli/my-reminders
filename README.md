# Pingme for JIRA

See _Setup_ to set up the application for the first time.

## Dev Loop

1. Execute `make`
2. Run the application (by default compiled in development mode, which will
   disable warnings)
3. Make changes to the application files
4. Reload in the browser, updated files will be compiled automatically

## Testing

Tests are defined in `tests`, run `make test` to compile and run them.

# Deployment to Heroku

There is an example `Procfile` in the application. 

Heroku setup instructions: TBD

# Setup

Note: This project uses submodules, so you will need to run `git submodule init; git submodule update`
before compilation will succeed.

Check the required and optional dependencies below, then run:

    » make setup
    
to setup a cabal sandbox and install the required project dependencies. 

The project can be build (in `development` mode which enables dynamic reloading) by running:

    » make
    
The snap application can then be started by running (listening on port 9000):

    » .cabal-sandbox/bin/ping-me-connect -p 9000
    Listening on http://0.0.0.0:9000/


## Dependencies

Pingme is built against the Haskell Platform, currently version `2013.2.0.0`.
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
    
