# The development docker file for the My Reminders Connect Haskell project.
# This docker file is designed to help us build the production executables
# but should not be used to actually generate the production docker images.
# Instead we should make a new production image that takes the executables
# from this image and only includes them. That way we do not need to carry
# the entire Haskell platform with us into production. Just the small set
# of required dependencies.

FROM node:14 as frontend
LABEL maintainer="Robert Massaioli <rmassaioli@atlassian.com>"

# Build the Frontend
ADD /frontend   /home/frontend
ADD /openapi.yaml /home
WORKDIR /home/frontend
RUN apt-get update && apt-get install -y openjdk-11-jre-headless
RUN yarn install && yarn build

# FROM haskell:9.8.1 AS build
FROM ubuntu:22.04 AS build
LABEL maintainer="Robert Massaioli <rmassaioli@atlassian.com>"

# Expose the default port, port 8000
EXPOSE 8000

# Install the missing packages
USER root
RUN (apt-get update || true) && apt-get install -y libpq-dev pkgconf haskell-stack build-essential
ENV LANG en_US.UTF-8 # See: https://github.com/haskell/cabal/issues/1883#issuecomment-44150139
ENV PATH /home/haskell/.cabal/bin:$PATH

# Copy our context into the build directory and start working from there
ADD /   /home/haskell/build
# RUN chown -R haskell:haskell /home/haskell/build

# Setup the Haskell Envoronment
# USER haskell
WORKDIR /home/haskell/build

# Initiate the build environment and build the executable (assumes that the
# atlassian-connect-haskell source can be found in the vendor/atlassian-connect directory AND that
# it has not been released to hackage yet (which is really where it should live).
#
# IMPORTANT: This must produce a statically-compiled binary (with respect to
# Cabal dependencies) that does not depend on a local cabal installation. The
# production Docker image will not run a cabal install.
RUN stack setup --reinstall && stack build -j2 && stack install my-reminders

# Setup the default command to run for the container.
CMD ["/root/.local/bin/my-reminders", "--access-log=-", "--error-log=stderr"]

# The production docker file for the Remind Me Connect Haskell project.
# It is designed to be minimal by requiring only the dependencies of the production executable.
# It is also designed to be easy to maintain by being as short as possible.

FROM ubuntu:20.04
LABEL maintainer="Robert Massaioli <rmassaioli@atlassian.com>"

# Expose the default port, port 8080
EXPOSE 8080

# Install the missing packages
USER root
RUN apt-get update && apt-get install -y libpq5 libgmp10 libnss3 libnss-mdns netbase ca-certificates

# Copy our context into the build directory and start working from there
USER root
COPY --from=frontend /home/frontend/build /service/frontend/build
COPY --from=build /home/haskell/build/snaplets /service/snaplets
COPY --from=build /home/haskell/build/resources /service/resources
COPY --from=build /home/haskell/build/dbmigrations /service/dbmigrations
COPY --from=build /home/haskell/build/static /service/static
COPY --from=build /root/.local/bin/my-reminders /service

# Setup the Haskell Envoronment
WORKDIR /service
# TODO is the LANG used by Snap or Haskell?
ENV LANG en_US.UTF-8 #

# Setup the default command to run for the container.
CMD ["/service/my-reminders", "--access-log=-", "--error-log=stderr", "--port=8080"]

