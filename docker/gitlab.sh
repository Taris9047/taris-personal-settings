#!/bin/sh

export GITLAB_HOME=/srv/gitlab

if [ ! -d "$GITLAB_HOME" ]; then
	sudo mkdir -p "$GITLAB_HOME"
fi

sudo docker run --detach \
  --hostname gitlab.example.com \
  --env GITLAB_OMNIBUS_CONFIG="external_url 'taris9047.ddns.net:30000'; gitlab_rails['lfs_enabled'] = true;" \
  --publish 443:443 --publish 30000:80 --publish 9022:22 \
  --env GITLAB_ROOT_PASSWORD="password" \
  --name gitlab \
  --restart always \
  --volume $GITLAB_HOME/config:/etc/gitlab \
  --volume $GITLAB_HOME/logs:/var/log/gitlab \
  --volume $GITLAB_HOME/data:/var/opt/gitlab \
  --shm-size 256m \
  gitlab/gitlab-ce:latest
