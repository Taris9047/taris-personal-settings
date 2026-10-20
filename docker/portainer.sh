#!/bin/sh

if [ ! -x "$(command -v docker)" ]; then
	printf "Well, we obviously need docker to run this crap!!\n"
	exit 1
fi

PORTAINER_HOME="/srv/portainer"

if [ ! -d "${PORTAINER_HOME}" ]; then
	mkdir -p "${PORTAINER_HOME}" || sudo mkdir -p "${PORTAINER_HOME}"
  chown -R :docker "${PORTAINER_HOME}"
  chmod -R 2775 "${PORTAINER_HOME}"
	printf "Portainer directory generated. Make sure docker group have write permission.\n"
	printf ' %s\n' "${PORTAINER_HOME}"
	exit 0
fi

if [ -z "$(docker ps | grep portainer)" ]; then
	printf 'Running portainer docker image!!\n'
	cd $PORTAINER_HOME && \
	docker run -d -p 9092:9443 -p 9091:9000 --name portainer \
		--restart unless-stopped \
		-v /var/run/docker.sock:/var/run/docker.sock \
		-v $PORTAINER_HOME/portainer_data:/data \
		portainer/portainer-ce:latest
else
  printf 'Docker container seems to be working... updating images instead...\n'
  docker pull portainer/portainer-ce:latest 
fi


