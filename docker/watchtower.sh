#!/bin/sh

printf 'Setting up watchtower\n'

docker run --detach \
  --name watchtower \
  -v /var/run/docker.sock:/var/run/docker.sock \
  containrrr/watchtower \
  --interval 60 --cleanup 


