    sudo apt install software-properties-common

    sudo add-apt-repository ppa:philip.scott/elementary-tweaks
    sudo apt update
    sudo apt install elementary-tweaks

    sudo apt install ubuntu-restricted-extras libavccodec-extra libdvd-pkg

    sudo ubuntu-drivers autoinstall

    sudo -H sed -i 's/OnlyShowIn\=Unity\;GNOME\;/OnlyShowIn\=Unity\;GNOME\;Pantheon\;/' /etc/xdg/autostart/indicator-application.desktop

    sudo apt install synaptic

    sudo apt install firefox
    sudo apt remove epiphany-browser

    sudo apt install libreoffice libreoffice-gtk3 libreoffice-style-elementary

    sudo apt install com.github.devidmhewitt.clipped

    sudo apt install gdebi

    sudo apt install tlp tlp-rdw
