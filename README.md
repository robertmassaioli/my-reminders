# Simple Snap Example Application

See _Setup_ to set up the application for the first time.

## Dev Loop

1. Execute `make`
2. Run the application (by default compiled in development mode)
3. Make changes to the application files
4. Reload in the browser, updated files will be compiled automatically

## Testing

Tests are defined in `tests`, run `make test` to compile and run them.



# Deployment to Heroku

There is an example `Procfile` in the application. 

Heroku setup instructions: TBD

# Setup

Check the required and optional dependencies below, then run:

    » make setup
    
to setup a cabal sandbox and install the required project dependencies. 

The project can be build (in `development` mode which enables dynamic reloading) by running:

    » make
    
The snap application can then be started by running (listening on port 9000):

    » .cabal-sandbox/bin/atlassian-snap-example -p 9000
    Listening on http://0.0.0.0:9000/


## Dependencies

The example project was setup using:

    » ghc --version
    The Glorious Glasgow Haskell Compilation System, version 7.6.3
    » cabal --version
    cabal-install version 1.18.0.2
    using version 1.18.1.1 of the Cabal library
    
YMMV


## Optional dependencies

These are not strictly necessary but make life easier.

### UPX

Compresses the executable. `make dist` currently strips debug symbols _and_ compresses the executable.

    » brew install upx
    
### cabal-constraints

The `make freeze` target "freezes" the exact versions of the dependencies selected by cabal-install. The dependency list will be written to `cabal.config`.
This requires the `cabal-constraints` executable to be available:

    » cabal install cabal-constraints
    