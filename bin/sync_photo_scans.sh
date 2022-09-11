#!/bin/sh

SRC_DIR="/GamenMedia/Scan/"
DEST_DIR="$HOME/GoogleDrive/Mom\ Stuff/아기앨범/"

CMD="rsync -hvrPt $SRC_DIR $DEST_DIR"

if [ ! -x "$(command -v rsync)" ]; then
	cp -rfv "$SRC_DIR" "$DEST_DIR"
else
	eval "$CMD"
fi

