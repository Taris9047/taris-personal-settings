#!/bin/sh 
if [ $# -ne 0 ]; then
	# Checking options
	if [ $1 = "clean" ]; then
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
	
	if [ -n $2 ]; then
		if [ -d $2 ]; then
			APACHE2_DIR=$2
			echo "Apache2 Source Folder: "$APACHE2_DIR" "
		else
			echo "Ooops. "$APACHE2_DIR" seems to be a Wrong thing.. not directory!!"
			exit 1
		fi
	else
		echo "Setting up default parameter: ./"
		APACHE2_DIR=./
	fi
	
	if [ -n $3 ]; then
		if [ -d $3 ]; then
			APR_DIR=$3
			echo "apr Folder: "$APR_DIR" "
		else
			"Ooops, "$APR_DIR" seems not a directory."
			exit 1
		fi
	else
		echo "Setting up default parameter: ../apr"
		APR_DIR='../apr'
	
	fi
	
	if [ -n $4 ]; then
		if [ -d $4 ]; then
			APR_UTIL_DIR=$4
			echo "apr Folder: "$APR_UTIL_DIR" "
		else
			"Ooops, "$APR_UTIL_DIR" seems not a directory."
			exit 1
		fi
	else
		echo "Setting up default parameter: ../apr-util"
		APR_UTIL_DIR='../apr-util'
	fi
	
else
	echo "No argument found."
	echo "List of Arguments: install, update, clean, purge"
	echo "Examples ..."
	echo "1) Apr, apr-util directories fixed at default locations."
	echo "   compile_apache.sh update"
	echo "2) Fully assigning parameters for clean install."
	echo "   compile_apache.sh clean /somewhere/apache-2.4.X /somewhere/apr /somewhere/else/apr-util"
	exit 1
fi

# Let's go to the destination and finish the job.
echo "Changing directory to assigned Apache source dir."
cd $APACHE2_DIR/

# Let's not forget the source directory root.
CURRENT_DIR=`pwd`

# compilation arguments here...
SYSTEM_ARGS="-O3 "
CONFIGURE_ARGS="-enable-layout=DIYMacServer -enable-mods-shared=all -with-ssl=/usr -with-mpm=prefork -disable-unique-id -enable-ssl -enable-dav -enable-cache -enable-proxy -enable-logio -enable-deflate -with-included-apr -enable-cgi -enable-cgid -enable-suexec"
MAKE_ARGS="-j 6"

# Setting up make tool
MAKE=`which make`
MAKEFILE=$APACHE2_DIR/Makefile
if [ $1 = "purge" || $1="clean" ]; then
	rm -rfv ./srclib/apr ./srclib/apr-util
fi

# Cleaning up 
if [ ! -f $MAKEFILE ]; then
	echo "Looks like a new tarball... or missing Makefile."
else
	echo "Makefile found at "$MAKEFILE" ..."
	if [[ "$MAKE_OPTION" = "clean" || "$MAKE_OPTION" = "distclean" ]]; then
		echo "Cleaning previous binaries..."
		$MAKE $MAKE_OPTION
	fi
fi

# Copying in apr library.
APACHE2_APR_DIR=$APACHE2_DIR/srclib/apr
APACHE2_APR_UTIL_DIR=$APACHE2_DIR/srclib/apr-util
if [ ! -d $APACHE2_APR_DIR ]; then
	cp -Rvf $APR_DIR $APACHE2_DIR/srclib/apr
else
	echo "Found apr directory at ./srclib/apr"
fi
if [ ! -d $APACHE2_APR_UTIL_DIR ]; then
	cp -Rvf $APR_UTIL_DIR $APACHE2_DIR/srclib/apr-util
else
	echo "Found apr-util directory at ./srclib/apr-util"
fi

# Appending Profiles to configs
PROFILE_CHECK=`cat ./config.layout | grep DIYMacServer`
if [ "$PROFILE_CHECK" = "" ]; then
	echo "
	
	<Layout DIYMacServer>
	  prefix: /usr/local/apache2
	  exec_prefix: ${prefix}
	  bindir: ${exec_prefix}/bin
	  sbindir: ${exec_prefix}/bin
	  libdir: ${exec_prefix}/lib
	  libexecdir: ${exec_prefix}/modules
	  mandir: ${prefix}/man
	  sysconfdir: /etc/httpd
	  datadir: /Library/Webserver
	  installbuilddir: ${datadir}/build
	  errordir: ${datadir}/error
	  iconsdir: ${datadir}/icons
	  htdocsdir: ${datadir}/Documents
	  manualdir: ${datadir}/manual
	  cgidir: ${datadir}/CGI-Executables
	  includedir: ${prefix}/include
	  localstatedir: /var
	  runtimedir: ${localstatedir}/run
	  logfiledir: ${localstatedir}/log/httpd
	  proxycachedir: ${runtimedir}/proxy
	</Layout>
	
	" >> ./config.layout
else 
	echo "Found profile setup in the ./config.layout"
fi

PROFILE_CHECK_APR=`cat ./srclib/apr/config.layout | grep DIYMacServer`
if [ "$PROFILE_CHECK_APR" = "" ]; then
	echo "
	
	<Layout DIYMacServer>
	  prefix: /usr/local/apache2
	  exec_prefix: ${prefix}
	  bindir: ${exec_prefix}/bin
	  sbindir: ${exec_prefix}/bin
	  libdir: ${exec_prefix}/lib
	  libexecdir: ${exec_prefix}/libexec+
	  mandir: ${prefix}/share/man
	  sysconfdir: /etc/httpd
	  datadir: /Library/Webserver
	  installbuilddir: ${datadir}/build
	  includedir: ${prefix}/include
	  localstatedir: /var
	  runtimedir: ${localstatedir}/run
	</Layout>
	
	" >> ./srclib/apr/config.layout
else 
	echo "Found profile setup in ./srclib/apr/config.layout"
fi

PROFILE_CHECK_APR_UTIL=`cat ./srclib/apr-util/config.layout | grep DIYMacServer`
if [ "$PROFILE_CHECK_APR_UTIL" = "" ]; then
	echo "
	
	<Layout DIYMacServer>
	  prefix: /usr/local/apache2
	  exec_prefix: ${prefix}
	  bindir: ${exec_prefix}/bin
	  sbindir: ${exec_prefix}/bin
	  libdir: ${exec_prefix}/lib
	  libexecdir: ${exec_prefix}/libexec+
	  mandir: ${prefix}/share/man
	  sysconfdir: /etc/httpd
	  datadir: /Library/Webserver
	  installbuilddir: ${datadir}/build
	  includedir: ${prefix}/include
	  localstatedir: /var
	  runtimedir: ${localstatedir}/run
	</Layout>
	
	" >> ./srclib/apr-util/config.layout
else 
	echo "Found profile setup in ./srclib/apr-util/config.layout"
fi

# Compiling Apache2
if [ ! -f $MAKEFILE ]; then
	echo "Configuring Apache2..."
	CFLAGS=$SYSTEM_ARGS CXXFLAGS=$SYSTEM_ARGS $APACHE2_DIR/configure $CONFIGURE_ARGS
fi
if [ -f $MAKEFILE ]; then
	echo "Compiling Apache2..."
	$MAKE $MAKE_ARGS
	
	echo "To install, run 'sudo make install'"
fi

cd $CURRENT_DIR
