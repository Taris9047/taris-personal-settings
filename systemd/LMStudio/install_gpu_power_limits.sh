#!/bin/bash

# Script installation files for NVIDIA GPU power limits.

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &>/dev/null && pwd)

# 1. Install the limits configuration file.
NVIDIA_LIMITS_FILE='/etc/nvidia-limits.conf'
if [ ! -f "${NVIDIA_LIMITS_FILE}" ]; then
	printf 'NVIDIA limits are not installed on the system.\n'
	printf 'Installing it to %s\n' "${NVIDIA_LIMITS_FILE}"
	ln -sfv "${SCRIPT_DIR}/leila-nvidia-limits.conf" "${NVIDIA_LIMITS_FILE}"
fi

# 2. Install set-gpu-limits.sh script
SET_GPU_LIMITS_FILE='/usr/local/bin/set-gpu-limits.sh'
if [ ! -f "${SET_GPU_LIMITS_FILE}" ]; then
	printf 'Installing set-gpu-limits.sh to %s\n' "${SET_GPU_LIMITS_FILE}"
	ln -sfv "${SCRIPT_DIR}/set-gpu-limits.sh" "${SET_GPU_LIMITS_FILE}"
fi

# 3. Install the systemd script
NVIDIA_LIMITS_FILE='/etc/systemd/system/nvidia-gpu-power-limit.service'
if [ ! -f "${NVIDIA_LIMITS_FILE}" ]; then
	printf 'Installing systemd module\n'
	ln -sfv "${SCRIPT_DIR}/nvidia-gpu-power-limit.service" "${NVIDIA_LIMITS_FILE}"
	systemctl daemon-reload
	systemctl enable nvidia-gpu-power-limit.service
	systemctl start nvidia-gpu-power-limit.service
fi