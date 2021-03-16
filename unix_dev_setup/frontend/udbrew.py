#!/usr/bin/env python3

import os
import sys
import re

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

### Version Parsor ###
class Version(object):
    def __init__(self, ver_info):
        self.ver_info = [0]
        if isinstance(ver_info, str):
            self.init_str(ver_info)
        elif isinstance(ver_info, list):
            self.init_list(ver_info)

    def init_str(self, ver_info):
        self.ver_info = self.split_num_alpha(ver_info.split('.'))
            
    def init_list(self, ver_info):
        self.ver_info = self.split_num_alpha(ver_info)

    # Stupid but needed since some programmers use version number with
    # alphabets such as 23b, 1.11.3a, etc.
    @staticmethod
    def split_num_alpha(ary):
        insert_list = []
        for i, _ in enumerate(ary):
            # knocking out v12.13 case
            if isinstance(_, str) and _[0].lower() == 'v':
                ary[i] = _[1:]

            try:
                ary[i] = int(_)
            except ValueError:
                splitted = re.findall(r"[^\W\d_]+|\d+", _)
                for s_i, s in enumerate(splitted):
                    if s.isnumeric():
                        splitted[s_i] = int(s)
                insert_list.append((i, splitted))

        offset = 0
        while insert_list:
            sp = insert_list.pop(0)
            ary[sp[0]+offset] = sp[1][0]
            ary = ary[:sp[0]+1+offset]+sp[1][1:]+ary[sp[0]+1+offset:]
            offset += len(sp)-1
        return ary

    def __eq__(self, other):
        return self.ver_info == other.ver_info

    def __ne__(self, other):
        return self.ver_info != other.ver_info

    def __lt__(self, other):
        return self.ver_info < other.ver_info

    def __le__(self, other):
        return self.ver_info <= other.ver_info

    def __gt__(self, other):
        return self.ver_info > other.ver_info

    def __ge__(self, other):
        return self.ver_info >= other.ver_info
    
    
### The Main Function ###
def main (ARGV):

    if not ARGV:
        sys.exit(1)

    Distro = GetDistro()

### Calling main function ###
if __name__ == "__main__":
    main(sys.argv)
    
