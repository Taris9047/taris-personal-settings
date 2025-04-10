#!/bin/sh

docker run -d \
  --name=code-server \
  -e PUID="$UID" \
  -e PGID="$(getent group $USER | cut -d: -f3)" \
  -e TZ="$(date +%Z)" \
  -p 8443:8443 \
  -v /srv/vscode-server/config:/config \
  --restart unless-stopped \
  lscr.io/linuxserver/code-server:latest
