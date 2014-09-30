# The production docker file for the Remind Me Connect Haskell project.
# This docker file is designed to help us build the production executables.
# This production image was designed to be minimal and only require the smallest
# subset of dependencies required to run our code which still being a fairly easy image
# to maintain.

FROM ubuntu:14.04
MAINTAINER Robert Massaioli <rmassaioli@atlassian.com>

# Expose the default port, port 8000
EXPOSE 8000

# Install the missing packages
USER root
RUN apt-get update && apt-get install -y libpq5 libgmp10

# Copy our context into the build directory and start working from there
USER root
ADD /to-copy   /service

# Setup the Haskell Envoronment
WORKDIR /service
# TODO is the LANG used by Snap or Haskell?
ENV LANG en_US.UTF-8 # 

# Setup the default command to run for the container.
CMD ["/service/ping-me-connect", "--access-log=-", "--error-log=stderr", "--port=8000"]
