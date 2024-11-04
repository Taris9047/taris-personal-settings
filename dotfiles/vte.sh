if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
	if [ -f '/etc/profile.d/vte.sh' ]; then
		source /etc/profile.d/vte.sh
	fi

	if [ -f '/etc/profile.d/vte-2.96.sh' ]; then
		source /etc/profile.d/vte.sh
	fi

fi
