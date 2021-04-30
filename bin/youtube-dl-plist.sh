#!/bin/sh

YOUTUBE_DL=$(command -v youtube-dl)

if [ ! -x $YOUTUBE_DL ]; then
    echo "Can't find youtube-dl!!"
    exit 1
fi
echo "youtube-dl has been found at $YOUTUBE_DL"


if [ $# -eq 0 ]; then
    echo "Need a YouTube Playlist URL!!"
    exit 1
fi

$YOUTUBE_DL -f bestaudio --audio-quality 0 --audio-format m4a -i -x --extract-audio -o '%(title)s.%(ext)s' $1



