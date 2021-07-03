#!/bin/sh

XOW_WORKDIR="$HOME/.xow_workspace"
XOW_SRCDIR="$XOW_WORKDIR/xow"
LIBUSB_SRCDIR="$XOW_WORKDIR/libusb"

DEFAULT_PREFIX="/usr/local"

die() {
    printf 'ERROR: %s\n' "$1"
    exit 1
}

[ ! -x "$(command -v git)" ] && die 'We need git in the system!!'
[ ! -x "$(command -v curl)" ] && die 'We need curl in the system!!'
[ ! -x "$(command -v cabextract)" ] && die 'We need cabextract in the system!!'
[ ! -x "$(command -v systemctl)" ] && die 'Systemd is required!! yeah... I know, it sucks...'

[ ! -d "$XOW_WORKDIR" ] && mkdir -p $XOW_WORKDIR

# Let's install libusb first. We need git version to ensure correct operation.
printf '>> Installing git version of libusb-1.0\n\n\n'
cd "$XOW_WORKDIR" &&
    rm -rf "$LIBUSB_SRCDIR" &&
    git clone https://github.com/libusb/libusb.git ./libusb &&
    cd "$LIBUSB_SRCDIR" &&
    ./autogen.sh &&
    CFLAGS="\"-O3 -fomit-frame-pointer -pipe -march=native\"" \
        ./configure --prefix="$DEFAULT_PREFIX" &&
    make && sudo make install

printf '\n\n\n'

# Then install xow using the libusb from git.
printf '>> Installing xow!!\n\n'
cd "$XOW_WORKDIR" &&
    rm -rf "$XOW_SRCDIR" &&
    git clone https://github.com/medusalix/xow.git ./xow &&
    cd "$XOW_SRCDIR" &&
    make BUILD=RELEASE LDFLAGS="$DEFAULT_PREFIX" && sudo make install &&
    sudo systemctl enable xow &&
    sudo systemctl start xow

printf '\n\n\nxow has been installed and service activated!\n'
printf 'You should be able to pair the controller now.\n'
printf 'But if something goes wrong, consult with xow_reset.sh!\n'
printf '\n\n\n\n'
