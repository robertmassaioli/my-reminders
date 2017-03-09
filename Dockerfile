# The development docker file for the My Reminders Connect Haskell project.
# This docker file is designed to help us build the production executables
# but should not be used to actually generate the production docker images.
# Instead we should make a new production image that takes the executables
# from this image and only includes them. That way we do not need to carry
# the entire Haskell platform with us into production. Just the small set
# of required dependencies.

# TODO replace with my own haskell platform docker creations.
FROM haskell:8.0.2
# FROM ubuntu:14.04
MAINTAINER Robert Massaioli <rmassaioli@atlassian.com>

# Expose the default port, port 8000
EXPOSE 8000

# Install the missing packages
USER root
RUN apt-get update && apt-get install -y libpq-dev pkgconf

# Copy our context into the build directory and start working from there
USER root
ADD /   /home/haskell/build
# RUN chown -R haskell:haskell /home/haskell/build

# Setup the Haskell Envoronment
# USER haskell
WORKDIR /home/haskell/build
ENV LANG en_US.UTF-8 # See: https://github.com/haskell/cabal/issues/1883#issuecomment-44150139
ENV PATH /home/haskell/.cabal/bin:$PATH

# Get the Haskell Dependencies
# TODO Do we require the Haskell Platform [http://packages.ubuntu.com/trusty/haskell-platform] or GHC [http://packages.ubuntu.com/trusty/ghc6]?
# RUN apt-get update && apt-get install -y haskell-platform && cabal update && cabal install cabal-install
RUN cabal update && cabal install cabal-install

# Initiate the build environment and build the executable (assumes that the
# atlassian-connect-haskell source can be found in the vendor/atlassian-connect directory AND that
# it has not been released to hackage yet (which is really where it should live).
#
# IMPORTANT: This must produce a statically-compiled binary (with respect to
# Cabal dependencies) that does not depend on a local cabal installation. The
# production Docker image will not run a cabal install.
RUN cabal sandbox init && cabal install -O2 --force-reinstalls

# Setup the default command to run for the container.
CMD ["/home/haskell/build/.cabal-sandbox/bin/my-reminders", "--access-log=-", "--error-log=stderr"]
