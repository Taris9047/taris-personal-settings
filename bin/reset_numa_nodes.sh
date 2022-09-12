#!/bin/sh

# This script needs to be run as a superuser...
#for a in /sys/bus/pci/devices/*; do echo 0 | sudo tee -a $a/numa_node; done

# Corrected the script based on this page: 
# https://stackoverflow.com/questions/44232898/memoryerror-in-tensorflow-and-successful-numa-node-read-from-sysfs-had-negativ

# In case of NVIDIA
for pcidev in $(lspci -D|grep 'VGA compatible controller: NVIDIA'|sed -e 's/[[:space:]].*//'); do echo 0 > /sys/bus/pci/devices/${pcidev}/numa_node; done

# In case of AMD 
for pcidev in $(lspci -D|grep 'VGA compatible controller: Advanced Micro Devices'|sed -e 's/[[:space:]].*//'); do echo 0 > /sys/bus/pci/devices/${pcidev}/numa_node; done

