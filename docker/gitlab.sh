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

if [ -f "$GITLAB_HOME/config/gitlab.rb" ]; then
  printf 'Previous configuration found!! deleting it!\n'
  sudo -H truncate -s 0 $GITLAB_HOME/config/gitlab.rb
fi

if [ -z "$(docker ps | grep gitlab)" ]; then
  printf "Hostname set: $EXT_URL\n\n"
  printf "Running gitlab docker!!\n"
  docker run --detach \
    --hostname="$EXT_URL" \
    --env GITLAB_OMNIBUS_CONFIG="external_url 'http://$EXT_URL/';gitlab_rails['lfs_enabled'] = true;puma['worker_processes'] = 0;sidekiq['max_concurrency'] = 10;" \
    --publish $MAIN_PORT:$MAIN_PORT --publish 9022:22 \
    --name gitlab \
    --restart always \
    --volume $GITLAB_HOME/config:/etc/gitlab \
    --volume $GITLAB_HOME/logs:/var/log/gitlab \
    --volume $GITLAB_HOME/data:/var/opt/gitlab \
    --shm-size 256m \
    gitlab/gitlab-ce:latest
fi

sudo -H truncate -s 0 $GITLAB_HOME/config/gitlab.rb
sudo -H tee -a $GITLAB_HOME/config/gitlab.rb > /dev/null <<EOL
puma['worker_processes'] = 0

sidekiq['min_concurrency'] = 1
sidekiq['max_concurrency'] = 6

postgresql['shared_buffers'] = "256MB"

prometheus_monitoring['enable'] = false

gitlab_rails['env'] = {
  'MALLOC_CONF' => 'dirty_decay_ms:1000,muzzy_decay_ms:1000'
}

gitaly['env'] = {
  'LD_PRELOAD' => '/opt/gitlab/embedded/lib/libjemalloc.so',
  'MALLOC_CONF' => 'dirty_decay_ms:1000,muzzy_decay_ms:1000',
  'GITALY_COMMAND_SPAWN_MAX_PARALLEL' => '2'
}

gitaly['concurrency'] = [
  {
    'rpc' => "/gitaly.SmartHTTPService/PostReceivePack",
    'max_per_repo' => 3
  }, {
    'rpc' => "/gitaly.SSHService/SSHUploadPack",
    'max_per_repo' => 3
  }
]
EOL

docker exec gitlab update-permissions
docker restart gitlab
