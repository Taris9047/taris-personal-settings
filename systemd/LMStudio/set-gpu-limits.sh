#!/bin/bash

# 1. Read in the configuration
NVIDIA_LIMITS_FILE='/etc/nvidia-limits.conf'
if [ ! -f "${NVIDIA_LIMITS_FILE}" ]; then
	printf 'nvidia-limits.conf is not installed yet!\n'
	exit 1
fi
source "${NVIDIA_LIMITS_FILE}"

# 2.5. Check nvidia-smi status
if [ -z "$(nvidia-smi -L UUID)" ]; then
	printf 'nvidia-smi does not show GPU UUIDs. Please check\n'
	exit 1
fi

# 3. Now do the NVIDIA thing...
#  1) persistence mode ON
nvidia-smi -pm 1
#  2) Set power limits for GPU 0 and 1
if [ ! -z "${LIMIT_0}" ]; then
	nvidia-smi -i 0 -pl "${LIMIT_0}" && \
	printf 'nvidia-smi: Power limit for GPU%d set to %dW\n' "0" "${LIMIT_0}"
fi

if [ ! -z "${LIMIT_1}" ]; then
	nvidia-smi -i 1 -pl "${LIMIT_1}" && \
	printf 'nvidia-smi: Power limit for GPU%d set to %dW\n' "1" "${LIMIT_1}"
fi

