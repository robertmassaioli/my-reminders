#!/bin/bash -e
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

EXECUTABLE=`find ${DIR}/../dist-newstyle -type f -name my-reminders -executable | sort -r | head -n1`

PORT=${PORT:-8080}
CONNECT_BASE_URL=${CONNECT_BASE_URL:-"http://localhost:${PORT}"} bash "$DIR/dev-run.bash" "${EXECUTABLE}" --port "${PORT}" --access-log=- --error-log=stderr