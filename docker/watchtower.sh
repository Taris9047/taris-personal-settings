#!/bin/sh

printf 'Setting up Docker watchtower\n'

if [ ! -x "$(command -v docker)" ]; then
  printf 'Docker does not exist.\n'
  exit 1
fi

docker run --detach \
  --name watchtower \
  -v /var/run/docker.sock:/var/run/docker.sock \
  containrrr/watchtower \
  --interval 60 --cleanup 

printf 'Running watchtower!\n'
