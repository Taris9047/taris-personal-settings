# Setting up Raspberry Pi 4 as iPad attachemnt

Let's talk about how to set up a Raspberry Pi as a mobile computing attachement of an iPad which cannot run many development tools such as VSCode nor any compilers natively across applications. The only viable solution without jailbreaking is setting up another Linux PC with it.

Due to the nature of being 'mobile', Raspberry Pi seems to be the only option we can take at the moment. Good thing is, Raspberry Pi4 has USB-C power port which can also be configured as a LAN card which can be directly connected to the USB-C port of modern iPad.

## Things to prepare
1. Raspberry Pi 4 - Obviously...
2. USB-C to USB-C cable with data communication with PD charging capability.
3. or, a USB-C hub with power input with USB-A or C extension ports.

Basically, we will connect the Thunderbolt 4 port of the iPad to Raspberry Pi's USB-C power port. In other words, we will run the Raspberry Pi 4 from iPad's power pack. This set up will work without too much problem for a short session. But if we need a constant power supply, we can also bring in a USB hub that has PD charging capability. And those USB hubs usually provide USB Type A ports instead. So, for this case, you may need USB-A to C PD charging data cable. If you want to way more mobile, the PD charging port of the USB hub can be connected to a battery pack with PD charging capability. Say, 23 W of power delivery would be enough.

## Settig up Raspberry Pi 4
We will use the Raspbian OS which is an 'official' OS for Raspberry Pi. Write a SD card with Raspbian image first, then some config files needs to be edited to make sure the USB-C port to be configured as a USB LAN card. In fact, this guide almost works with RPi 5 as well. Just a few changes...


### Enabling SSHd 
Now we need to access the SD card baked with the Raspbian OS to open ```/boot/``` directlry.

At first, we need to enable `ssh` by making an empty file in the ```/boot```. Make sure you have a simple empty file like this in path:
```/boot/ssh```

### Enabling WiFi
As we will be accessing the Raspberry Pi via the USB-C, we still need the Pi conneced to WiFi for updating the OS itself and git repository cloning etc etc. So, we will also set up the WiFi as well. To do this, we have to make another file in the `/boot/` directory. 

```/boot/wpa_supplicant.conf```

This `wpa_supplicant.conf` file will contain the WiFi connection information. Here is a sample of the WiFi configuration file:

```
country=CA
ctrl_interface=DIR=/var/run/wpa_supplicant GROUP=WORKGROUP
update_config=1

network={
    ssid="WIFI_SSID"
    psk="WIFI_password"
}
```

Just make sure the `ssid` and `psk` are the exact set of your WiFi router or anywhere you have to connect.
The country code is `CA` meaning it is Canada. But it doesn't really make any difference too much. Just put it anywhere you live.

### Preparing Raspberry Pi 4
Now we set up the most basic stuffs, which can even be configured when baking the SD card with modern Raspbian OS imager, now we are ready to put this SD card into the Raspberry Pi and power up. Now let's connect to the Raspberry Pi 4 via ssh.
```
ssh -y pi@raspberrypi.local

```
Then at first, update all the packages...
```
sudo apt update && sudo apt upgrade -y
```
Wait for all the packages to be updated then reboot the Pi. Then connect to the Pi once again via SSH. Then we need to install a DHCPCD server by...
```
sudo apt install -y dnsmasq
```

### Config files
Here, list of config files to edit.
```
/boot/config.txt
/boot/cmdline.txt
/etc/modules
/etc/dhcpcd.conf
```
For RPi5, you should rather change...
```
/boot/firmware/config.txt
/boot/firmware/cmdline.txt
/etc/modules
```

#### `/boot/config.txt`
Add following to the end of file:
```
dtoverlay=dwc2
```

#### `/boot/cmdline.txt`
Add following to the end of the file. Make sure this part is separated with spaces with existing command.
```
modules-load=dwc2
```
#### `/etc/modules`
Add following to the end of the file.
```
libcomposite
```
#### `/etc/dhcdcd.conf`
Add following to the end of the file.
** This part is NOT needed for RPi5 **
```
denyiterfaces usb0
```

### Networking Settings
Now we have to make a usb network interfacing definition at `/etc/dnsmasq.d/`
```
sudo touch /etc/dnsmasq.d/usb
```
Then fill the `/etc/dnsmasq.d/usb` file with the content below:
```
interface=usb0
dhcp-range=10.55.0.2,10.55.0.6,255.255.255.248,1h
dhcp-option=3
leasefile-ro
```
Now, we will set up the Pi to have a fixed IP address of `10.55.0.1` when connected to the iPad via USB-C. First of all, open this file as superuser: `/etc/network/interfaces.d/usb0`, then add the following lines:
```
auto usb0
allow-hotplug usb0
iface usb0 inet static
    address 10.55.0.1
    netmask 255.255.255.248
```

### Start-Up script
Now we need to write a shell script that will run every boot to set up the Network interface with the USB-C port.

Write a file, possibly named as: `usb.sh`, somewhere you want, then fill out the file with the content below:
```
#!/bin/sh
OTG_Pi='display-pi'
cd /sys/kernel/config/usb_gadget/
mkdir -p "${OTG_Pi}"
cd "${OTG_Pi}"

MY_NAME="${USER}"
PI_NAME="${HOSTNAME}"

echo 0x1d6b > idVendor # Linux Foundation
echo 0x0104 > idProduct # Multifunction Composite Gadget
echo 0x0100 > bcdDevice # v1.0.0
echo 0x0200 > bcdUSB # USB2
echo 0xEF > bDeviceClass
echo 0x02 > bDeviceSubClass
echo 0x01 > bDeviceProtocol

mkdir -p strings/0x409
echo "fedcba9876543211" > strings/0x409/serialnumber
echo "${MY_NAME}" > strings/0x409/manufacturer
echo "${PI_NAME} PI4 USB Device" > strings/0x409/product
mkdir -p configs/c.1/strings/0x409
echo "Config 1: ECM network" > configs/c.1/strings/0x409/configuration
echo 250 > configs/c.1/MaxPower

# Add functions here
# see gadget configurations below
# End functions
mkdir -p functions/ecm.usb0
HOST="00:dc:c8:f7:75:14" # "HostPC"
SELF="00:dd:dc:eb:6d:a1" # "BadUSB"
echo $HOST > functions/ecm.usb0/host_addr
echo $SELF > functions/ecm.usb0/dev_addr
ln -s functions/ecm.usb0 configs/c.1/
udevadm settle -t 5 || :
ls /sys/class/udc > UDC
ifup usb0
service dnsmasq restart
```

For Raspberry Pi 5, a few bits for hardware description has changed. Thus, you can rather use this code below...
```
#!/bin/bash

OTG_Pi=display-pi
cd /sys/kernel/config/usb_gadget/
mkdir -p "${OTG_Pi}"
cd "${OTG_Pi}"
echo 0x1d6b > idVendor # Linux Foundation
echo 0x0104 > idProduct # Multifunction Composite Gadget
echo 0x0103 > bcdDevice # v1.0.3
echo 0x0320 > bcdUSB # USB2
echo 2 > bDeviceClass
mkdir -p strings/0x409
echo "fedcba9876543213" > strings/0x409/serialnumber
echo "Some User" > strings/0x409/manufacturer
echo "Display-Pi USB Device" > strings/0x409/product
mkdir -p configs/c.1/strings/0x409
echo "CDC" > configs/c.1/strings/0x409/configuration
echo 250 > configs/c.1/MaxPower
echo 0x80 > configs/c.1/bmAttributes

#ECM
mkdir -p functions/ecm.usb0
HOST="00:dc:c8:f7:75:15" # "HostPC"
SELF="00:dd:dc:eb:6d:a1" # "BadUSB"
echo $HOST > functions/ecm.usb0/host_addr
echo $SELF > functions/ecm.usb0/dev_addr
ln -s functions/ecm.usb0 configs/c.1/

#RNDIS
mkdir -p configs/c.2
echo 0x80 > configs/c.2/bmAttributes
echo 0x250 > configs/c.2/MaxPower
mkdir -p configs/c.2/strings/0x409
echo "RNDIS" > configs/c.2/strings/0x409/configuration

echo "1" > os_desc/use
echo "0xcd" > os_desc/b_vendor_code
echo "MSFT100" > os_desc/qw_sign

mkdir -p functions/rndis.usb0
HOST_R="00:dc:c8:f7:75:16"
SELF_R="00:dd:dc:eb:6d:a2"
echo $HOST_R > functions/rndis.usb0/dev_addr
echo $SELF_R > functions/rndis.usb0/host_addr
echo "RNDIS" >   functions/rndis.usb0/os_desc/interface.rndis/compatible_id
echo "5162001" > functions/rndis.usb0/os_desc/interface.rndis/sub_compatible_id

ln -s functions/rndis.usb0 configs/c.2
ln -s configs/c.2 os_desc

udevadm settle -t 5 || :
ls /sys/class/udc > UDC

sleep 5

nmcli connection up bridge-br0
nmcli connection up bridge-slave-usb0
nmcli connection up bridge-slave-usb1
sleep 5
service dnsmasq restart
```



Usually, you can put this `usb.sh` file into the `root` user's home directory.
```
sudo cp -vf ./usb.sh /root/ 
```
Or, you would rather place the file as```/usr/local/sbin/usb-gadget.sh``` for Pi 5.
On both cases, Pi 4 or 5, dont' forget to add executable privilege on this file.

Then, add the script to `/etc/rc.local` to make sure this script runs on each boot.
```
echo "sh /root/usb.sh" >> /etc/rc.local
```
** This is not needed for Pi5 **
For Pi 5, we would rather use systemd. Prepare a systemd command unit as described here:
```
[Unit]
Description=My USB gadget
After=network-online.target
Wants=network-online.target
#After=systemd-modules-load.service
  
[Service]
Type=oneshot
RemainAfterExit=yes
ExecStart=/usr/local/sbin/usb-gadget.sh
  
[Install]
WantedBy=sysinit.target
```
Place this file at ```/lib/systemd/system/usbgadget.service``` and enable the service.
```
sudo systemctl enable usbgadget.service
```

### [Pi 5 Only] Network Manager
The OTG IP address will be set here in case of RPi 5.
```
nmcli con add type bridge ifname br0
nmcli con add type bridge-slave ifname usb0 master br0
nmcli con add type bridge-slave ifname usb1 master br0
nmcli connection modify bridge-br0 ipv4.method manual ipv4.addresses 10.55.0.1/24
```
Here, we set up the Pi 5's USB-C IP address as 10.55.0.1.
You can set it up as you want, i.e. 192.168.42.1 (MilkV like), 192.168.7.1 (Beaglebone Black), etc.

### [Pi 5 Only] Also, you need to work with DNSMASQ
Now you have to use dnsmasq which is not installed on the system by default. Install it with ```sudo apt-get install -y dnsmasq```.

Then add the lines below to ```/etc/dnsmasq.d/br0```
```
dhcp-authoritative
dhcp-rapid-commit
no-ping
interface=br0
dhcp-range=10.55.0.2,10.55.0.6,255.255.255.248,1h
dhcp-option=3
leasefile-ro
```
You will have to adjust this mask address to apply different IP address as mentioned above.


### Zeroconf (MDNS) set up (optional)
Lastly, we will establish a Zeroconf functionality so that we do not need to remember the draded IP address number every time.

First, we need to define a hostname:
```
sudo vi /etc/hostname
```
Then adjust the boring `localhost` to something more joyful. Then install `Avahi` daemon.
```
sudo apt install -y avahi-daemon
sudo systemctl enable avahi-daemon
sudo systemctl start avahi-daemon
```
Then you will be able to connect to Pi with the `hostname.local` you have defined at `/etc/hostname`.

In fact, this part would have already been dealt with Pi 5's most recent Raspbian!!

### VNC Server 
Raspberry pi comes with a built-in RealVNC server which is really handy. You can enable it by accessing the `raspi-config` anytime!!



