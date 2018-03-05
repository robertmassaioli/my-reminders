#!/bin/sh

echo "Starting the install micros-cli process."
 
curl -O -X GET https://statlas.prod.atl-paas.net/micros-stable/linux/micros.tar.gz
# Ready for cutting edge features and to live life on the edge?
# Installs beta release of micros cli
# curl -O -X GET https://statlas.prod.atl-paas.net/micros-beta/linux/micros.tar.gz
tar xvf micros.tar.gz
chmod +x micros
mkdir -p bin && mv micros bin/
rm -f micros.tar.gz