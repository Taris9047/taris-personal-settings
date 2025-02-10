#!/bin/sh

#-e OLLAMA_API_BASE_URL=http://127.0.0.1:11434/api \
printf '** Setting up Open-Webui docker continer. **\n'

# Checking if docker exists...
if [ ! -x "$(command -v docker)" ]; then
  printf 'Docker not found!! exiting!!\n'
  exit 1;
fi

# Dedicated Ollama Model directory
DEF_OLLAMA_MODEL_DIR="/srv/ollama"
OLLAMA_MODEL_DIR="${DEF_OLLAMA_MODEL_DIR}"
if [ ! -z "$1" ]; then
  printf 'Ollama Model directory given: %s\n' "$1"
  if [ ! -z "$1" ]; then
    OLLAMA_MODEL_DIR="$1"
    printf 'Setting up the Ollama Model directory: %s\n' "$1"
  else
    printf 'Given directory: %s not found! Exiting...\n' "$1"
    exit 1;
  fi
else
  OLLAMA_MODEL_DIR="/app/backend/data"
  printf 'Checking if a default Ollama dir at %s\n' "${DEF_OLLAMA_MODEL_DIR}"
  if [ -d "${DEF_OLLAMA_MODEL_DIR}" ]; then
    printf 'Default Ollama model directory found at: %s\n' "${DEF_OLLAMA_MODEL_DIR}"
    OLLAMA_MODEL_DIR="${DEF_OLLAMA_MODEL_DIR}"
  fi
fi


# Now installing docker open-webui
printf 'Now installing Open-Webui via Docker\n'

# Opwn-Webui docker image name
DOCKER_IMG="ghcr.io/open-webui/open-webui:main"

# Removing the previous docker 
DOCKER_CONTAINER_ID="$(docker ps | grep open-webui | awk '{print $1}')"
if [ ! -z "${DOCKER_CONTAINER_ID}" ]; then
  printf 'Previous Open-Webui detected. Deleting it...\n'
  docker stop "${DOCKER_CONTAINER_ID}" && \
  docker container rm "${DOCKER_CONTAINER_ID}"
  DOCKER_IMG_ID="$(docker image ls | grep open-webui | awk '{print $3}')"
  docker image rm "${DOCKER_IMG_ID}" || printf 'Previous Docker image deletion failed!!\n' \
    && exit 1
fi

# Running the docker install
docker run -d \
  -p 3000:8080 \
  --add-host=host.docker.internal:host-gateway \
  -e OLLAMA_MODELS="${OLLAMA_MODEL_DIR}" \
  -v open-webui:/app/backend/data \
  --network=host \
  --name open-webui \
  --restart always "${DOCKER_IMG}" \
  && printf 'Docker should be running!! Exiting!!\n'
