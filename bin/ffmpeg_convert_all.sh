#!/bin/sh

FFMPEG=$(command -v ffmpeg)

if [ $FFMPEG == "" ]; then
	echo "Whooops, FFMPEG can't be found!!"
else
	echo "FFMPEG is found at $FFMPEG"
fi

TARGET_FORMAT=""
if [ $# -eq 0 ]; then
	TARGET_FORMAT="mp3"
else
	TARGET_FORMAT="${1,,}"
fi

echo "Setting up target format as $TARGET_FORMAT"

echo "Let's work!!"

TARGET_ENC_PHRASE=""
if [ "$TARGET_FORMAT" == "mp3" ]; then
	TARGET_ENC_PHRASE="-acodec libmp3lame -q:a 2"
elif [ "$TARGET_FORMAT" == "m4a" ]; then
	TARGET_ENC_PHRASE="-c:a libfdk_aac -b:a 192k"
fi

for f in $PWD/*.[^$TARGET_FORMAT]; 
do
	$FFMPEG -i "$f" $TARGET_ENC_PHRASE "${f%.*}.$TARGET_FORMAT"
done

if [ $TARGET_FORMAT == "mp3" ]; then
	echo "Normalizing..."
	"mp3gain /e /r /s r *.mp3"
fi



