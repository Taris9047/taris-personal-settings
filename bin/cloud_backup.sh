#!/bin/sh

die () {
    printf '%s\n' "$1"
    exit 1
}

# This script needs rsync!!
if [ ! -x "$(command -v rsync)" ]; then
    die "We do not have rsync!!"
fi

# This script also needs 2 arguments:
# usage: cloud_backup.sh <source_dir> <target_dir>
#
if [ "$#" -ne 2 ]; then
    printf 'Usage: %s <source dir> <target dir>\n' "$0"
    exit 0
fi

src_dir="$1"
tgt_dir="$2"

# Actually running the syncing
printf 'Running sync operation for %s to %s\n' "$src_dir" "$tgt_dir"
rsync -avu --delete "$src_dir" "$tgt_dir" && \
    printf 'Sync operation successful\n'
