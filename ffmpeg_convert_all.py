#!/usr/bin/env python

import os
import platform as plf
import subprocess as sbp

# getch implementation.
try:
    # Win32
    from msvcrt import getch
except ImportError:
    # UNIX
    def getch():
        import sys, tty, termios
        fd = sys.stdin.fileno()
        old = termios.tcgetattr(fd)
        try:
            tty.setraw(fd)
            return sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old)

def find_ffmpeg_posix():
    p = sbp.Popen('which ffmpeg', stdout=sbp.PIPE, shell=True).communicate[0]
    which_output = list[p]
    if not which_output:
        raise ValueError("Cannot find ffmpeg in current PATH environment!!")
        sys.exit(-1)

    else:
        ffmpeg_dir = os.path.dirname(which_output[0])
        ffmpeg_cmd = os.path.join(ffmpeg_dir, 'ffmpeg')
        return ffmpeg_cmd


"""
Behold!! the main function!!

"""
if __name__ == "__main__":
    # Setting up ffmpeg executable
    # Tailered with my system setting. One needs to fix it according to one's system.
    ffmpeg_dir = ''
    ffmpeg_cmd = ''
    if plf.system().lower() == 'windows':
        ffmpeg_dir = 'C:\\MinGW\\bin'
        ffmpeg_cmd = os.path.join(ffmpeg_dir, 'ffmpeg.exe')
    else:
        # Unix systems usually hold executables at
        #
        # /usr/bin/
        # /usr/local/bin/
        #
        ffmpeg_cmd = find_ffmpeg_posix()
        
    # converter options
    ffmpeg_options = '-c:v libx264 -preset slow -crf 20 -c:a libvo_aacenc -b:a 128k'

    # Setting up current dir
    current_file_dir = os.path.dirname(
        os.path.realpath(__file__))

    print("Current directory: {}".format(current_file_dir))

    source_list = \
        {
            "avi": [],
            "mkv": [],
            "mpeg": [],
            "wmv":[],
        }

    # Grabbing file list for avi and mkv
    for file in os.listdir(current_file_dir):
        if file.endswith(".avi"):
            source_list["avi"].append(os.path.abspath(file))
        elif file.endswith(".mkv"):
            source_list["mkv"].append(os.path.abspath(file))

    # Run the stuff
    vid_keys = source_list.keys()
    for vk in vid_keys:
        if source_list.get(vk):
            for source_file in source_list.get(vk):
                new_name = \
                    os.path.join(current_file_dir, 
                    os.path.splitext(source_file)[0]+".mp4")
                
                print("\n\033[92mConverting\n*** {} to\n>>>> {}\033[0m\n".format(source_file, new_name))
                conv_cmd = \
                    ffmpeg_cmd+" "+\
                    '-i "{}" '.format(source_file)+\
                    ffmpeg_options+" "+\
                    '"{}"'.format(new_name)
                
                if not os.path.exists(new_name):
                    p = sbp.Popen(conv_cmd, stdout=sbp.PIPE, bufsize=1024, shell=True).communicate()[0]
                else:
                    continue
                
                if os.path.exists(new_name):
                    if plf.system().lower() == 'windows':
                        p = sbp.Popen('del /F /S "{}"'.format(source_file), 
                            stdin=sbp.PIPE, bufsize=1, shell=True).communicate()[0]
                    else:
                        p = sbp.Popen('rm -rfv "{}"'.format(source_file), 
                            stdin=sbp.PIPE, bufsize=1, shell=True).communicate()[0]
                        
    print("Conversion Job Finished!!!!!")
    print("Press Any (Not the 'Any' key but literally...) Key to Continue...")
    getch()
