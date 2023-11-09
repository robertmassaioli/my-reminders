#!/bin/bash -e

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

PORT=${PORT:-8080}

zsh $DIR/fswatch-run.zsh `find snaplets/heist/templates -name '*.tpl'` "curl http://0.0.0.0:${PORT}/admin/reload" &