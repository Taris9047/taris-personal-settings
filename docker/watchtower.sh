#!/bin/sh

export CONT_NAME=''

if [ -n "$1" ]; then
  CONT_NAME="$1"
else
  printf 'Please provide a container name to watch!\n'
  return 1;
fi

printf 'Setting up watchtower for %s\n' "${CONT_NAME}"

docker run --detach \
  --name watchtower-"${CONT_NAME}" \
  containrrr/watchtower \
  --interval 1m --cleanup --watch \""${CONT_NAME}"\"


