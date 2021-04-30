#!/usr/bin/env bash

# Cleaning up those dirty git history!! 
#
# People says we should always work as branch to keep git history clean.
# I'll do that later!!
#

# Anyway, this script has been tested and works fine! 
# The original source is:
# https://stackoverflow.com/a/13102849

git checkout --orphan newBranch
git add -A  # Add all files and commit them
git commit
git branch -D master  # Deletes the master branch
git branch -m master  # Rename the current branch to master
git push -f origin master  # Force push master branch to github
git gc --aggressive --prune=all     # remove the old files
