#!/usr/bin/env python

import os
import sys
import platform as plf
import subprocess as sbp

# converter options
ffmpeg_options = \
    '-c:v libx264 -preset slower -crf 20 -c:a libfdk_aac -b:a 128k'

# Source file list
__supported_formats = \
    [
        'avi',
        'mkv',
        'mpeg',
        'mpg',
        'm2ts',
        '3gp',
        'wmv',
        'wma',
        'mov',
        '264',
        'h264',
        'bik',
        'ts',
        'vob',
        'mjpeg',
        'mv4',
    ]

# Preparing input file data dictionary.
source_list = {}
for _sf in __supported_formats:
    source_list.update({_sf: []})

# getch implementation.
try:
    # Win32
    from msvcrt import getch
except ImportError:
    # UNIX
    def getch():
        import tty
        import termios
        fd = sys.stdin.fileno()
        old = termios.tcgetattr(fd)
        try:
            tty.setraw(fd)
            return sys.stdin.read(1)
        finally:
            termios.tcsetattr(fd, termios.TCSADRAIN, old)


def find_ffmpeg_posix():
    p = \
        sbp.Popen(
            'which ffmpeg',
            stdout=sbp.PIPE,
            shell=True).communicate[0]
    which_output = list[p]
    if not which_output:
        raise ValueError("Cannot find ffmpeg in current PATH environment!!")

    else:
        ffmpeg_dir = os.path.dirname(which_output[0])
        ffmpeg_cmd = os.path.join(ffmpeg_dir, 'ffmpeg')
        return ffmpeg_cmd


def which(program):
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(program)

    if fpath:
        if is_exe(program):
            return program
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, program)
            if is_exe(exe_file):
                return exe_file

    return None


####################################
# Behold!! the main function!!     #
####################################
def main(argv=[]):
    # Setting up ffmpeg executable
    # Tailered with my system setting.
    # One needs to fix it according to one's system.
    ffmpeg_cmd = ''
    if plf.system().lower() == 'windows':
        ffmpeg_cmd = which("ffmpeg.exe")
    else:
        # Unix systems usually hold executables at
        #
        # /usr/bin/
        # /usr/local/bin/
        #
        ffmpeg_cmd = find_ffmpeg_posix()

    if not ffmpeg_cmd:
        raise ValueError("ffmpeg command cannot be found!!")

    # Setting up current dir
    current_file_dir = os.path.dirname(
        os.path.realpath(__file__))

    print("Current directory: {}".format(current_file_dir))

    # Grabbing file list for source_list extensions
    is_empty_input = True
    sl_k_l = list(source_list.keys())
    for file in os.listdir(current_file_dir):
        for slkl in sl_k_l:
            if file.endswith('.' + slkl) or file.endswith('.' + slkl.upper()):
                source_list[slkl].append(os.path.abspath(file))
                is_empty_input = False

    if is_empty_input:
        print("No valid input video file given!!")
        sys.exit(0)

    # Run the stuff
    print("Using ffmpeg at... {}\n".format(ffmpeg_cmd))
    for vk in sl_k_l:
        if source_list.get(vk):
            for source_file in source_list.get(vk):
                new_name = \
                    os.path.join(
                        current_file_dir,
                        os.path.splitext(source_file)[0] + ".mp4")

                print(
                    "\n*** Converting ***\n>>> \
                        {} to\n>>>> {}\n"
                    .format(source_file, new_name))

                conv_cmd = \
                    ffmpeg_cmd + " " + \
                    '-i "{}" '.format(source_file) + \
                    ffmpeg_options + " " + \
                    '"{}"'.format(new_name)

                if not os.path.exists(new_name):
                    p = sbp.Popen(
                        conv_cmd,
                        stdout=sbp.PIPE,
                        bufsize=1024,
                        shell=True).communicate()[0]
                else:
                    continue

                if os.path.exists(new_name):
                    if plf.system().lower() == 'windows':
                        p = sbp.Popen(
                            'del /F /S "{}"'.format(source_file),
                            stdin=sbp.PIPE,
                            bufsize=1,
                            shell=True).communicate()[0]
                    else:
                        p = sbp.Popen(
                            'rm -rfv "{}"'.format(source_file),
                            stdin=sbp.PIPE,
                            bufsize=1,
                            shell=True).communicate()[0]

    print("Conversion Job Finished!!!!!")
    print("Press Any (Not the 'Any' key but literally...) Key to Continue...")
    getch()


# Calling out main function
if __name__ == "__main__":
    main()
