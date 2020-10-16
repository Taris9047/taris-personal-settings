#!/bin/sh


# Update current software!!
sudo apt-get -y update && sudo apt-get -y upgrade

# Now install all those apache stuffs..
sudo apt-get -y install apache2
echo "Apache2 installed!! check up /var/www/html to set up your web site."
ln -sfv /var/www/html $HOME/WWW
echo "Link to web site has been created at $HOME/WWW"

echo "Now installing mysql and php"
sudo apt-get -y install php
sudo service apache2 restart

sudo apt-get -y install mysql-server php-mysql
sudo service apache2 restart

echo "MAMP has been installed!! Let's work on some productive stuffs!!"
echo "Gotta install mysql database first!!"
echo "run sudo mysql_secure_installation to set up your mysql password!!"



