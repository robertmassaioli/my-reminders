# The development docker file for the Remind Me Connect Haskell project.
# This docker file is designed to help us build the production executables
# but should not be used to actually generate the production docker images.
# Instead we should make a new production image that takes the executables 
# from this image and only includes them. That way we do not need to carry 
# the entire Haskell platform with us into production. Just the small set 
# of required dependencies.

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
