#!/bin/sh

if [ ! -x "$(command -v docker)" ]; then
	printf "Well, we obviously need docker to run this crap!!\n"
	exit 1
fi

PORTAINER_HOME=/srv/portainer

if [ ! -d "$PORTAINER_HOME" ]; then
	sudo -p mkdir "$PORTAINER_HOME"
	printf "Portainer directory generated. Make sure you have write permission.\n"
	printf '--> %s\n' "$PORTAINER_HOME"
	exit 0
fi

if [ -z "$(docker ps | grep portainer)" ]; then
	printf 'Running portainer docker image!!\n'
	cd $PORTAINER_HOME && \
	docker run -d -p 8000:8000 -p 9091:9000 --name portainer \
		--restart=always \
		-v /var/run/docker.sock:/var/run/docker.sock \
		-v $PORTAINER_HOME/portainer_data:/data \
		cr.portainer.io/portainer/portainer-ce:latest
fi


