#!/bin/sh

export GITLAB_HOME=/srv/gitlab

sudo docker run --detach \
  --hostname gitlab.example.com \
  --env GITLAB_OMNIBUS_CONFIG="external_url '10.0.2.15:30005'; gitlab_rails['lfs_enabled'] = true;"
  --publish 30443:443 --publish 30005:80 --publish 30022:22 \
  --name gitlab \
  --restart always \
  --volume $GITLAB_HOME/config:/etc/gitlab \
  --volume $GITLAB_HOME/logs:/var/log/gitlab \
  --volume $GITLAB_HOME/data:/var/opt/gitlab \
  gitlab/gitlab-ee:latest
