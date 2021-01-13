#!/bin/sh
if [ ! -x $(command -v rip ]; then
	rm -rfv ./build ./src
else
	rip ./build ./src
fi

