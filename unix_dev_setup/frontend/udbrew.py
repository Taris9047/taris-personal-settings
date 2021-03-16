#!/usr/bin/env python3

import os
import sys

CWD = os.path.realpath(__file__)
DATA_DIR = os.path.realpath(os.path.join(os.path.dirname(CWD), '..', 'data'))

### Get distro crom /etc/os-release ###
class GetDistro(object):
    def __init__(self, release_info='/etc/os-release'):
        if not os.path.exists(release_info):
            raise ValueError("Ouch, we need {}!!".format(release_info))

        with open(release_info, 'r') as fp:
            rinfo_str = fp.read()

        self.rel_data = {}
        for l in rinfo_str.split(os.linesep):
            if not l:
                continue
            label, item = l.split('=')
            self.rel_data[label] = item

    def __getitem__(self, info_key):
        return self.rel_data[info_key]

    def Name(self):
        return self.rel_data["NAME"]

    def Version(self):
        return self.rel_data["VERSION_ID"]

            
### The Main Function ###
def main (ARGV):

    if not ARGV:
        sys.exit(1)

    Distro = GetDistro()

### Calling main function ###
if __name__ == "__main__":
    main(sys.argv)
    
