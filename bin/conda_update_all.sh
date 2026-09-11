#!/bin/sh

# Temporary file to hold environment variables
tmp_file="/tmp/conda_envs_list.$$"

if [ -x "$(command -v conda)" ]; then
  rm -rf '/tmp/conda_envs_list.*'
  conda env list | sed '/^#/d; /^$/d; s/.*[[:space:]]//' > "$tmp_file"
else
  printf 'Error! No Conda found in the path!!\n'
  exit 1
fi

if [ ! -s "$tmp_file" ]; then
  printf 'No Conda envs found\n'
  exit 0
fi

# Lists installed conda environments
printf 'Detected following Conda envs\n'
printf '==============================================\n'
cat "$tmp_file"
printf '==============================================\n'

# Read the file line by line to handle paths that may contain spaces safely
while IFS= read -r env_path; do
  if [ -n "$env_path" ]; then
    printf '==========================================\n'
    printf 'Updating environment located at: %s\n' $env_path
    printf '==========================================\n'
        
    # Update all packages in the specific path without prompting
    conda update --prefix "$env_path" --all -y
  fi
done < "$tmp_file"

rm -f "$tmp_file"

printf 'All Conda environments have been updated!!\n'
return 0
