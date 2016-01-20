#!/bin/bash -e

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
RJS_CMD=${RJS_CMD:-r.js}

cd "$DIR/.."
${RJS_CMD} -o build.js $@
