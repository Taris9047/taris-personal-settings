#!/bin/sh

shopt -s extglob

FFMPEG="$(command -v ffmpeg)"

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

MAIN_OPTS="-y -map 0:a"

TARGET_ENC_PHRASE=""
if [ "$TARGET_FORMAT" == "mp3" ]; then
	TARGET_ENC_PHRASE="$MAIN_OPTS -acodec libmp3lame -b:a 192k -af dynaudnorm"
elif [ "$TARGET_FORMAT" == "m4a" ]; then
	TARGET_ENC_PHRASE="$MAIN_OPTS -c:a libfdk_aac -b:a 192k -af dynaudnorm"
elif [ "$TARGET_FORMAT" == "aac" ]; then
    TARGET_ENC_PHRASE="$MAIN_OPTS -acodec aac -b:a 192k -af dynaudnorm"
elif [ "$TARGET_FORMAT" == "ogg" ]; then
    TARGET_ENC_PHRASE="$MAIN_OPTS -acodec libvorbis -q:a 2 -af dynaudnorm"
elif [ "$TARGET_FORMAT" == "norm" ]; then
	TARGET_ENC_PHRASE="$MAIN_OPTS -af dynaudnorm -f null"
fi

if [ $TARGET_FORMAT == "norm" ]; then
    TARGETS=./*.*
else
    TARGETS=./*.!($TARGET_FORMAT)
fi

for f in $TARGETS;
do
    TARGET_OUTF="${f%.*}.$TARGET_FORMAT"
    if [ $TARGET_FORMAT == "norm" ]; then
	    TARGET_OUTF=/dev/null
    fi

	$FFMPEG -i "$f" $TARGET_ENC_PHRASE "$TARGET_OUTF"

#    if [ $TARGET_FORMAT == "norm" ]; then
#        rm -rf "$f";
#        mv "$TARGET_OUTF" "$f";
#    fi
done




