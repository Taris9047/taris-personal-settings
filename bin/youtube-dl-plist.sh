#!/bin/sh

YOUTUBE_DL=$(command -v youtube-dl)

if [ $YOUTUBE_DL == "" ]; then
    echo "Can't find youtube-dl!!"
    return
fi

if [ $# -eq 0 ]; then
    echo "Need an address!!"
    return
fi

$YOUTUBE_DL -f bestaudio --audio-quality 0 --audio-format m4a -i -x --extract-audio -o '%(title)s.%(ext)s' $1



