# The development docker file for the My Reminders Connect Haskell project.
# This docker file is designed to help us build the production executables
# but should not be used to actually generate the production docker images.
# Instead we should make a new production image that takes the executables
# from this image and only includes them. That way we do not need to carry
# the entire Haskell platform with us into production. Just the small set
# of required dependencies.

FROM kkarczmarczyk/node-yarn as frontend
LABEL maintainer="Robert Massaioli <rmassaioli@atlassian.com>"

# Build the Frontend
ADD /frontend   /home/frontend
ADD /swagger.yaml /home
WORKDIR /home/frontend
RUN apt-get update && apt-get install -y openjdk-7-jre-headless
RUN yarn install && yarn run get-swagger-codegen && yarn build

FROM haskell:8.0.2 AS build
LABEL maintainer="Robert Massaioli <rmassaioli@atlassian.com>"

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
RUN cabal sandbox init && cabal update && cabal install -O2 --force-reinstalls

# Setup the default command to run for the container.
CMD ["/home/haskell/build/.cabal-sandbox/bin/my-reminders", "--access-log=-", "--error-log=stderr"]

# The production docker file for the Remind Me Connect Haskell project.
# It is designed to be minimal by requiring only the dependencies of the production executable.
# It is also designed to be easy to maintain by being as short as possible.

FROM ubuntu:16.04
LABEL maintainer="Robert Massaioli <rmassaioli@atlassian.com>"

# Expose the default port, port 8080
EXPOSE 8080

# Install the missing packages
USER root
RUN apt-get update && apt-get install -y libpq5 libgmp10 openjdk-8-jre-headless libnss3 libnss-lwres libnss-mdns netbase

# Copy our context into the build directory and start working from there
USER root
COPY --from=frontend /home/frontend/build /service/frontend/build
COPY --from=build /home/haskell/build/snaplets /service/snaplets
COPY --from=build /home/haskell/build/resources /service/resources
COPY --from=build /home/haskell/build/migrations /service/migrations
COPY --from=build /home/haskell/build/static /service/static
COPY --from=build /home/haskell/build/.cabal-sandbox/bin/my-reminders /service

# Setup the Haskell Envoronment
WORKDIR /service
# TODO is the LANG used by Snap or Haskell?
ENV LANG en_US.UTF-8 # 

# Setup the default command to run for the container.
CMD ["/service/my-reminders", "--access-log=-", "--error-log=stderr", "--port=8080"]

