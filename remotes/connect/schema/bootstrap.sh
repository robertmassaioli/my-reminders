#!/bin/bash

set -e
set -u

if [ "$#" -ne 1 ]; then
  echo "Usage $0 IP"
  exit -1
fi

HOST="$1"

USER="myreminders"
DATABASE="myreminders"
SCRIPTPATH=`dirname $0`

# Does the database exist?
DB_EXISTS=$(psql -lqt | cut -d \| -f 1 | grep -w ${DATABASE} | wc -l | tr -d ' ')
if [[ "$DB_EXISTS" -ne "1" ]]; then
    createdb -O $USER $DATABASE
fi
