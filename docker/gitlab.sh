#!/bin/sh

export GITLAB_HOME=/srv/gitlab

if [ ! -d "$GITLAB_HOME" ]; then
	sudo mkdir -p "$GITLAB_HOME"
fi

MAIN_PORT=30000

HOSTNAME='taris9047.ddns.net'
docker run --detach \
  --hostname=$HOSTNAME \
  --env GITLAB_OMNIBUS_CONFIG="gitlab_rails['lfs_enabled'] = true;" \
  --publish 9443:443 --publish $MAIN_PORT:80 --publish 9022:22 \
  --name gitlab \
  --restart always \
  --volume $GITLAB_HOME/config:/etc/gitlab \
  --volume $GITLAB_HOME/logs:/var/log/gitlab \
  --volume $GITLAB_HOME/data:/var/opt/gitlab \
  --shm-size 256m \
  gitlab/gitlab-ce:latest

#  --env GITLAB_OMNIBUS_CONFIG="external_url 'http://$HOSTNAME:30000'; gitlab_rails['lfs_enabled'] = true;" \
