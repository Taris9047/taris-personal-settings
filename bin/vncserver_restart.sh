#!/usr/bin

# This script is supposed to be run as a root user.

systemctl stop vncserver-x11-serviced.service

sleep 1.0

systemctl start vncserver-x11-serviced.service

printf 'RealVNC server restart finished!\n'
