#!/usr/bin/env python3

import os
import sys
import xml.etree.ElementTree as ET
import xml.dom.minidom as mdom

image_exts = ["jpg", "jpeg", "png"]

Version = [0, 1, 0]

def_outf_name = 'background.xml'


def display_help():
    ver_string = "{:d}.{:d}.{:d}".format(Version[0], Version[1], Version[2])
    print("Dynamic Wallpaper Creator! Version {}".format(ver_string))
    print("Usage: make_xml_bkg.py <image_folder>")
    print("provide -h or --help option to show this message.")


#
# Implementing normal ring list
#
class Node(object):
    def __init__(self, value):
        self.next = None
        self.value = value

    def Get(self):
        return self.value

    def Set(self, stuff):
        self.value = stuff


class RList(object):
    def __init__(self):
        self.head = None
        self.cursor = self.head
        self.size = 0

    def find_last(self):
        if self.head is None:
            return None

        tmp_node = self.head
        while True:
            if tmp_node.Next() == self.head:
                return tmp_node
            else:
                tmp_node = tmp_node.Next()

    def find_target(self, target):
        if self.head is None:
            raise ValueError("Oh crap, the list is empty!!")

        tmp = self.head
        sz = self.size
        while sz:
            if tmp.value == target:
                self.cursor = tmp
                return tmp
            else:
                tmp = tmp.next
            sz -= 1
        raise ValueError("Given value cannot be found!")

    def Append(self, value):
        if self.head is None:
            self.head = Node(value)
            self.head.next = self.head
            self.cursor = self.head
        else:
            tmp_node = self.find_last()
            tmp_node.next = Node(value)
            tmp_node = tmp_node.next
            tmp_node.next = self.head
        self.size += 1

    def GetByIndex(self, index):
        if index == 0 or index % self.size == 0:
            return self.head.Get()

        tmp = self.head
        if 0 <= index and index < self.size:
            ind = index
        elif index < 0:
            ind = index
            while True:
                ind = self.size + ind
                if ind >= 0:
                    break
        elif index >= self.size:
            ind = index % self.size

        while ind:
            tmp = tmp.next
            ind -= 1

        self.cursor = tmp
        return tmp.Get()

    def Next(self):
        self.cursor = self.cursor.next


#
# The XML Background maker
#
class MakeXMLBackground(object):
    def __init__(self, img_path, outf_name=def_outf_name, image_extensions=image_exts, verbose=False, duration=900.0, transition=5.0):
        self.img_path = os.path.realpath(img_path)
        self.img_exts = image_extensions
        self.outf_name = outf_name

        # Slideshow setting (duration, transition)
        self.slideshow_setting = [duration-transition, transition]

        file_list = \
            [os.path.join(self.img_path, fn)
             for fn in os.listdir(self.img_path)]
        self.img_list = []
        for f in file_list:
            self.__select_image(f)

        if len(self.img_list) == 0:
            print("Given directory does not have any usable images!")
            print("Images accepted are: {}".format(", ".join(self.img_exts)))
            sys.exit(0)
        else:
            print("Total {} images found!!".format(len(self.img_list)))

        self.xml_file_string = ""
        self.__make_xml()

        print("XML Generation Completed!!")

        if verbose:
            print(self.xml_file_string)
            exit(0)

        self.__save_to_file()

    def __select_image(self, file_path):
        for im_ext in self.img_exts:
            if file_path.endswith('.'+im_ext):
                self.img_list.append(file_path)

    def __make_xml(self):
        xml_tree = ET.Element('background')
        st_time = ET.SubElement(xml_tree, 'starttime')
        yr = ET.SubElement(st_time, 'year')
        yr.text = "2009"
        mon = ET.SubElement(st_time, 'month')
        mon.text = "08"
        day = ET.SubElement(st_time, 'day')
        day.text = "04"
        hh = ET.SubElement(st_time, 'hour')
        hh.text = "00"
        mm = ET.SubElement(st_time, 'minute')
        mm.text = "00"
        sec = ET.SubElement(st_time, 'second')
        sec.text = "00"

        # Now adding the transition crap
        for i, img in enumerate(self.img_list):
            if i == len(self.img_list)-1:
                img_next = self.img_list[0]
            else:
                img_next = self.img_list[i+1]

            static = ET.SubElement(xml_tree, 'static')
            dur = ET.SubElement(static, 'duration')
            dur.text = "{:.1f}".format(self.slideshow_setting[0])
            file = ET.SubElement(static, 'file')
            file.text = img

            tran = ET.SubElement(xml_tree, 'transition')
            tran.text = "{:.1f}".format(self.slideshow_setting[1])
            fr = ET.SubElement(tran, 'from')
            fr.text = img
            to_ = ET.SubElement(tran, 'to')
            to_.text = img_next

        self.xml_file_string = \
            mdom.parseString(ET.tostring(xml_tree)).toprettyxml()

    def __save_to_file(self):
        with open(self.outf_name, 'w') as fp:
            fp.write(self.xml_file_string)

        print("Dynamic Background XML saved at...")
        print(os.path.realpath(self.outf_name))


#
# The main function
#
if __name__ == "__main__":
    # Default image directory is where you are at.
    image_dir = os.getcwd()

    outfile_name = def_outf_name
    if len(sys.argv) > 1:
        if '-h' in sys.argv or '--help' in sys.argv:
            display_help()
            sys.exit(0)

        if '-o' in sys.argv:
            ind_o = sys.argv.index('-o')
            if sys.argv.size <= ind_o + 1:
                print("Input file is needed!")
                sys.exit(-1)
            outfile_name = sys.argv[ind_o+1]

        image_dir = os.path.realpath(sys.argv[1])

        if not os.path.exists(image_dir):
            print("It looks like the given directory for images does not exist!!")
            sys.exit(-1)

    print("Image directory has been set:")
    print(image_dir)

    mxb = MakeXMLBackground(image_dir, outf_name=outfile_name)
