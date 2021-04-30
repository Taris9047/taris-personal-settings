#!/bin/sh 
if [ $# -ne 0 ]; then
	# Checking options
	if [ $1 = "test" ]; then
		echo "test option selected. Will run... make test"
		MAKE_OPTION="test"
	elif [ $1 = "clean" ]; then
		echo "clean option selected. Will run... make clean"
		MAKE_OPTION="clean"	
	elif [ $1 = "purge" ]; then
		echo "purge option selected. Will run... make distclean"
		MAKE_OPTION="distclean"
	elif [[ $1 = "install" || $1 = "update" ]]; then
		echo "Default option selected."
		MAKE_OPTION=""
	else
		echo "No options detected. use 'install' or 'update' option"
		exit 1
	fi
	
	if [ ! $2 = ]; then
		if [ -d $2 ]; then
			PHP_DIR=$2
			echo "PHP Folder: "$PHP_DIR" "
		else
			echo "Ooops. "$PHP_DIR" seem to be an Wrong Directory!!"
			exit 1
		fi
	else
		echo "Setting up default parameter: ./"
		PHP_DIR=./
	fi
else
	echo "No argument found."
	echo "List of Arguments: install, update, clean, purge, test"
	exit 1
fi

# Let's go to the destination and finish the job.
echo "Changing directory to assigned dir."
cd $PHP_DIR/

# Let's not forget current directory.
CURRENT_DIR=`pwd`

# compilation arguments here...
SYSTEM_ARGS="-O3 "
CONFIGURE_ARGS=" --prefix=/usr/local/php5  --mandir=/usr/share/man  --infodir=/usr/share/info  --sysconfdir=/etc  --with-config-file-path=/etc/php.ini  --with-curl  --with-zlib  --with-zlib-dir=/usr  --with-openssl --enable-exif  --enable-ftp  --enable-mbstring  --enable-mbregex  --enable-sockets  --with-gd  --with-jpeg-dir=/usr/local/lib  --with-png-dir=/usr/X11R6  --with-freetype-dir=/usr/X11R6 --with-xpm-dir=/usr/X11R6  --with-mysql=mysqlnd  --with-pdo-mysql=mysqlnd  --with-mysqli=mysqlnd  --with-apxs2=/usr/local/apache2/bin/apxs --with-mcrypt  --with-iconv"
MAKE_ARGS="-j 6"

# Setting up make tool
MAKE=`which make`
MAKEFILE=$PHP_DIR/Makefile

# Cleaning up 
if [ ! -f $MAKEFILE ]; then
	echo "Looks like a new tarball... or missing Makefile."
else
	echo "Makefile found at "$MAKEFILE" ..."
	if [[ "$MAKE_OPTION" = "clean" || "$MAKE_OPTION" = "distclean" ]]; then
		echo "Cleaning previous binaries..."
		$MAKE $MAKE_OPTION
	fi
	if [ "$MAKE_OPTION" = "test" ]; then
		$MAKE distclean
	fi
fi

# Compiling PHP
if [ ! -f $MAKEFILE ]; then
	echo "Configuring PHP"
	CFLAGS=$SYSTEM_ARGS CXXFLAGS=$SYSTEM_ARGS $PHP_DIR/configure $CONFIGURE_ARGS
fi
if [ -f $MAKEFILE ]; then
	echo "Compiling PHP"
	$MAKE $MAKE_ARGS
	
	# Checking make test
	if [ "$MAKE_OPTION" = "test" ]; then
		echo "Running Test script."
		$MAKE $MAKE_OPTION
	fi

	echo "To install, run 'sudo make install'"
fi

cd $CURRENT_DIR
