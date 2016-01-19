#!/bin/bash -e

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
LESS_CMD=${LESS_CMD:-lessc}

cd "$DIR/.."

if ! which "$LESS_CMD" > /dev/null
then
   echo "You don't have lessc on the command line. Please install it using:"
   echo ""
   echo "   npm install -g less"
   echo ""
   exit 1
fi

STATIC_CSS_DIR="static-css"
mkdir -p "$STATIC_CSS_DIR"
echo "Building the CSS"
${LESS_CMD} --verbose "static/less/app.less" "$STATIC_CSS_DIR/app.css" $@
echo "Built the CSS"
