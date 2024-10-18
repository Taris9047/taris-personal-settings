#!/bin/sh

origin_dir="$(pwd -P)"

JPYTER_HOME="${HOME}/.jpyter"
JPYTER_VENV="${JPYTER_HOME}/.jpyter_venv"

# Checking if JPYTER_HOME directory exists...
if [ ! -d "${JPYTER_HOME}" ]; then
  printf 'Jupyter Home not found.. making one..\n'
  mkdir -p "${JPYTER_HOME}"
fi

cd "${JPYTER_HOME}"

# Setting up the Virtual Environment for 
# Jupyter Lab if needed...
#
if [ ! -d "${JPYTER_VENV}" ]; then
  printf 'jupyter venv not found, making one...\n'
  python -m venv "${JPYTER_VENV}"
  . "${JPYTER_VENV}/bin/activate"
  pip install -U pip
  pip install -U jupyterlab notebook numpy scipy matplotlib h5py pandas
  deactivate
fi

# Killing all the other Jupyter servers...
printf 'Killing all the other Jupyter lab processes...\n'
ps -ef | grep 'myProcessName' | grep -v grep | awk '{print $2}' | xargs -r kill -9
sleep 1s

# Finally run the jupyter lab
. "${JPYTER_VENV}/bin/activate" && \
  jupyter notebook --ip="0.0.0.0" --port="8888" --no-browser &


#deactivate
cd "${origin_dir}"