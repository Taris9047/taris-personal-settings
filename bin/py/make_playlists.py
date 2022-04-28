#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import sys

Known_Playlist_Types = [
    'pls',
    'm3u8',
    'm3u',
    'asx',
    'xspf',
    'wpl'
]

Music_File_Types = [
    'mp3',
    'm4a',
    'flac',
    'ogg',
    'opus'
]

#
# Make Playlist class
#
# Scans a music library folder and skim through all the
# directories inside it. Place a .m3u8 file in each dir.
#
class MakePlaylists(object):

    def __init__(self, MusicLibraryDir='', Mode='m3u8'):
        if not os.path.isdir(MusicLibraryDir):
            raise ValueError("Not a valid directory!!!")

        self.MusicLibDir = os.path.realpath(MusicLibraryDir)
        all_dirs = []
        for root, dirs, files in os.walk(os.path.abspath(self.MusicLibDir)):
            all_dirs += [os.path.join(root, _) for _ in dirs]

        self.MusicDirs = all_dirs

        self.PlaylistType = Mode
        if self.PlaylistType == 'm3u8':
            self.WalkDirs(self.MakeM3U8)
        else:
            print("Playlist type {} is not yet supported!!")
            sys.exit(1)

    # Scan through directories
    #
    # Decide whether to run the playlist generation or not.
    #
    def WalkDirs(self, walk_func):
        for dir in self.MusicDirs:

            file_list = \
                [
                    f for f in os.listdir(dir) \
                        if os.path.isfile(os.path.join(dir, f))
                ]

            music_found = False
            music_file_list = []
            for mus_typ in Music_File_Types:
                for f in file_list:
                    if mus_typ in f.lower()[-1*len(mus_typ):]:
                        music_file_list.append(f)
            if len(music_file_list) > 0:
                music_found = True

            if len(music_file_list) == 0:
                print("Passing {}, no music file found!!".format(dir))
                continue

            # playlist_found = False
            # for pls in Known_Playlist_Types:
            #     for f in file_list:
            #         if pls in f.lower()[-1*len(pls):]:
            #             print(
            #                 "A {} Playlist found!! {}  Skipping!".format(pls, f))
            #             playlist_found = True
            #             break

            if music_found == True:
                walk_func(dir)

    # Actually do the job! for M3U8
    def MakeM3U8(self, dir=os.getcwd()):
        files_in_here = \
            [
                f for f in os.listdir(dir) \
                    if os.path.isfile(os.path.join(dir, f))
            ]
        files_in_here = sorted(files_in_here)

        pl_name = os.path.join(dir, os.path.basename(dir) + '.m3u8')
        with open(pl_name, 'w') as fp:
            for f in files_in_here:
                if f.split('.')[-1].lower() not in Music_File_Types:
                    continue

                #pl_fn = os.path.join('.', f)
                pl_fn = f
                pl_fn_txt = "{}".format(pl_fn) + os.linesep
                fp.write(pl_fn_txt)

        print(
            "Playlist {} has been written!"\
            .format(os.path.realpath(pl_name)))

#
# The main function
#
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: make_playlists.py <Music Directory> <Playlist Type>")
        sys.exit(0)

    music_lib_dir = os.path.realpath(sys.argv[1])

    if len(sys.argv) == 3:
        playlist_type = sys.argv[2].lower()
    else:
        playlist_type = 'm3u8'

    print("Given Music library dir: {}".format(music_lib_dir))
    print("Playlist type: {}".format(playlist_type))

    make_pl = MakePlaylists(music_lib_dir, playlist_type)
