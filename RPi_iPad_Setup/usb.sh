#!/bin/sh
cd /sys/kernel/config/usb_gadget/
mkdir -p pi4
cd pi4

MY_NAME='taris'
PI_NAME='Falfa'

echo 0x1d6b > idVendor
echo 0x0104 > idProduct
echo 0x0100 > bcdDevice
echo 0x0200 > bcdUSB
echo 0xEF > bDeviceClass
echo 0x02 > bDeviceSubClass
echo 0x01 > bDeviceProtocol

mkdir -p strings/0x409
echo "fedcba9876543211" > strings/0x409/serialnumber
echo "${MY_NAME}" > strings/0x409/manufacturer
echo "${PI_NAME} RPi4 USB Device" > strings/0x409/product

mkdir -p configs/c.1/strings/0x409
echo "Config 1: ECM network" > configs/c.1/strings/0x409/configuration
echo 250 > configs/c.1/MaxPower

mkdir -p functions/ecm.usb0
HOST="00:dc:c8:f7:75:14"
SELF="00:dd:dc:eb:6d:a1"
echo "$HOST" > functions/ecm.usb0/host_addr
echo "$SELF" > functions/ecm.usb0/dev_addr

ln -s functions/ecm.usb0 configs/c.1/
udevadm settle -t 5 || :
ls /sys/class/udc > UDC
ifup usb0
service dnsmasq restart
