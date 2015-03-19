#!/bin/sh
echo "This patch was proven valid for version 0.21 and 0.22. We don't know what would happen if you have newer or older versions..."
echo "Based on: http://forum.kerbalspaceprogram.com/threads/24529-The-Linux-compatibility-thread!?p=705281&viewfull=1#post705281"
echo ""

if [ -z "$1" ]
  then
    KSP_PATH="."
  else
    KSP_PATH=$1
fi

KSP_64_BIN="${KSP_PATH}/KSP.x86_64"

if [ -s ${KSP_64_BIN} ]
then
    echo "7cebc7: 00" | xxd -r - KSP.x86_64
    echo "7cebcc: 00" | xxd -r - KSP.x86_64
    xxd -s +0x7cebc7 -l 1 KSP.x86_64
    xxd -s +0x7cebcc -l 1 KSP.x86_64
else
    echo "Uh oh, I can't find the 64 bit executable... Did you actually install it? OS X and Windows version doesn't have 64 bit support, yet."
    echo ${KSP_64_BIN} 
    echo "is the executable file I was looking for!" 
    echo ""
    echo "You can provide path to your KSP folder. For example,"
    echo "./ksp_x86_64_binary_patch.sh /home/jebediah/.local/Steam/steamapps/common/Kerbal\ Space\ Program/"
    echo "Just like that."
    echo 
fi



