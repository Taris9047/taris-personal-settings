#!/bin/sh

if [ ! -x "$(command -v docker)" ]; then
  printf 'Docker isn not installed!\n'
  exit 1
fi

printf 'Installing and running watchtower\n'

docker run -d \
  --name watchtower \
  -v /var/run/docker.sock:/var/run/docker.sock \
  containrrr/watchtower \
  gitlab portainer nextcloud
