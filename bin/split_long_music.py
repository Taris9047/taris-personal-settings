#!/usr/bin/env python

# https://gist.github.com/Ashwinning/a9677b5b3afa426667d979b36c019b04

import os
import sys
import subprocess

inputfile = sys.argv[1]
codec = '-acodec'

# Check up extension.
inputfile_name, output_extension = os.path.splitext(inputfile)
print('File extension detected: %s'%output_extension)

#ffmpeg did not like having '?' in the file name, add any other problematic symbol here.
escape_list = ['?', '#', ':', '*', '/', '\'']

def RemoveSymbols(text):
    for symbol in escape_list:
        text = text.replace(symbol, '_')
    return text

tracklist = []

class Track:
    def __init__(self, timestamp, name):
        self.timestamp = timestamp
        self.name = name

class ExtractTracks:
    def __init__(self):
        with open(sys.argv[2], "r") as values:
            for value in values:
                start_time_found = False
                name = ""
                timestamp = ""
                #split all by spaces.
                keyVal = value.replace('(', ' ').replace(')', ' ').split(' ')
                #find timestamp
                for word in keyVal:
                    if ':' in word:
                        if start_time_found == False:
                            timestamp = word
                            start_time_found = True
                    else:
                        name += word + ' '
                print timestamp, name
                tracklist.append(Track(timestamp, name))

#Initialize
ExtractTracks()


def GenerateSplitCommand(start, end, filename):
    return ['ffmpeg', '-i', inputfile, '-ss', start, '-to', end, '-c', 'copy', filename+output_extension, '-v', 'error']

def GetVideoEnd():
    ffprobeCommand = [
        'ffprobe',
        '-v',
        'error',
        '-show_entries',
        'format=duration',
        '-of',
        'default=noprint_wrappers=1:nokey=1',
        '-sexagesimal',
        inputfile
    ]
    return subprocess.check_output(ffprobeCommand).strip()

for i in range(0, len(tracklist)):
    name = tracklist[i].name.strip()
    name = RemoveSymbols(name)
    startTime = tracklist[i].timestamp.strip()
    if i != (len(tracklist) - 1):
        endTime = tracklist[i+1].timestamp.strip() #- startTime
    else:
        endTime = GetVideoEnd() #- startTime
    print('---')
    print('Generating ' + name + ' from ' + startTime + ' to ' + endTime)
    print('---')
    command = GenerateSplitCommand(str(startTime), str(endTime), name)
    output = subprocess.check_call(command)