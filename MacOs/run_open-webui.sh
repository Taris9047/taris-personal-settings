#!/bin/zsh

# Setting up conda environments
CONDA_ROOT="${HOME}/.miniconda3"
CONDA_EXEC="${CONDA_ROOT}/bin/conda"

# Conda environment name for open-webui and other llm relates stuffs..
CONDA_ENVNAME="llm"
	
# if we don't see any conda
if [ -z "$(which conda)" ]; then
	printf 'ERROR: miniconda not found!!\n'
	exit -1
fi

# Initialzing conda
eval "$(${CONDA_EXEC} shell.zsh hook)"
conda init

# Finally calling conda environment
if [ -z "$(conda env list | grep ${CONDA_ENVNAME})" ]; then
	conda create -n "${CONDA_ENVNAME}" "python=3.11" -y
	conda activate "${CONDA_ENVNAME}"
	pip install -U pip
	pip install open-webui
else
	conda activate "${CONDA_ENVNAME}"
fi

if [ -z "$(ps -A | grep open-webui)" ]; then
	open-webui serve
else
	printf 'open-webui is still running!! Not doing anything!!\n'
fi

conda deactivate
