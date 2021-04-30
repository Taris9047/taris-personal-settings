#!/usr/bin/env python2

'''
Batch converter for ffmpeg

usage:
	convert_media.py <input_directory> <input_media_format> <output_media_format> <ffmpeg_options>

'''

import os
import sys

if (len(sys.argv) < 4):
	print ""
	print "***** Not enough arguments!! *****"
	print ""
	print "Proper usage: "
	print ""
	print "onvert_media.py <input_directory> <input_media_format> <output_media_format> <optional:ffmpeg_options>"
	print ""
	exit(0)

path = sys.argv[1]

input_format = ['.' + x for x in sys.argv[2].split()]
output_format = '.' + sys.argv[3]
args = " ".join(sys.argv[4:])

files = [os.path.join(path,f) for f in os.listdir(path) if os.path.splitext(f.lower())[1] in input_format]

print "Files to convert: "
print files
print
print "Input Format: "
print input_format
print
print "Output Format: "
print output_format
print 
print "Converter options: "
print args
print

raw_input("Press any key to continue...")

for f in files:
	outf = './'+f.split('.')[len(f.split('.'))-2]+output_format
	print "Converting ", f, "to ", outf
	print "Arguments: ", args
	os.system('ffmpeg -i "%s" %s "%s"' % (f, args, outf))
	print ""

