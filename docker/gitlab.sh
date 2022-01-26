#!/bin/sh

export GITLAB_HOME=/srv/gitlab

if [ ! -d "$GITLAB_HOME" ]; then
	sudo mkdir -p "$GITLAB_HOME"
fi

MAIN_PORT="30000"

if [ "$MAIN_PORT" = '80' ]; then
  EXT_URL="taris9047.ddns.net"
else
  EXT_URL="taris9047.ddns.net:$MAIN_PORT"
fi

printf "Hostname set: $HOSTNAME\n\n"
printf "Running docker!!\n"
docker run --detach \
  --hostname="$EXT_URL" \
  --env GITLAB_OMNIBUS_CONFIG="external_url 'http://$EXT_URL/';gitlab_rails['lfs_enabled'] = true;" \
  --publish $MAIN_PORT:$MAIN_PORT --publish 9022:22 \
  --name gitlab \
  --restart always \
  --volume $GITLAB_HOME/config:/etc/gitlab \
  --volume $GITLAB_HOME/logs:/var/log/gitlab \
  --volume $GITLAB_HOME/data:/var/opt/gitlab \
  --shm-size 256m \
  --memory=2048m \
  --cpus=2 \
  gitlab/gitlab-ce:latest

#  --env GITLAB_OMNIBUS_CONFIG="external_url 'http://$HOSTNAME:30000'; gitlab_rails['lfs_enabled'] = true;" \
