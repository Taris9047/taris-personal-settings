#!/bin/sh

printf  'RealVNC maintenance script.\n'

if [ ! -x "$(command -v vncserver-x11)" ]; then
  printf 'Looks like we are missing some components!\n'
  exit 1
fi

sudo systemctl stop vncserver-x11-serviced.service && sleep 1.0

sudo systemctl start vncserver-x11-serviced.service && printf 'RealVNC server successfully restarted!\n'

sudo /usr/bin/vncserver-x11 -service -reload && printf 'RealVNC server reloaded!\n'

