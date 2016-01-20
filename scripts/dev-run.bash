#!/bin/bash -e

# The purpose of the dev run script is to run an executable in dev and to have it automatically
# restarted when the executable is changed.
# 
# $1 is the executable to run
# $2- is the arguments to pass to it

EXECUTABLE="$1"
shift
ARGS="$@"

echo "# Initialising"
echo "Will run in dev: $EXECUTABLE"
echo "With args: $ARGS"

if which fswatch > /dev/null
then
   echo "fswatch command found."
else
   echo "The fswatch command is not present. brew install fswatch or equivalent required."
   exit 1
fi

echo "" 
echo "# Running"
$EXECUTABLE $ARGS &
EXECUTABLE_PID=$!

while read -r line
do
   kill -15 "$EXECUTABLE_PID"
   $EXECUTABLE $ARGS &
   EXECUTABLE_PID=$!
done < <(fswatch $EXECUTABLE)
