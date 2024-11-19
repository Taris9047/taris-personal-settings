#!/bin/sh

#-e OLLAMA_API_BASE_URL=http://127.0.0.1:11434/api \

docker run -d \
  -p 3000:8080 \
  --add-host=host.docker.internal:host-gateway \
  -e OLLAMA_MODELS=/srv/ollama/ \
  -v open-webui:/app/backend/data \
  --network=host \
  --name open-webui \
  --restart always ghcr.io/open-webui/open-webui:main

