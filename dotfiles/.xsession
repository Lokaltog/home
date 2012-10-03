#!/bin/zsh --login

set -u

while :; do
	for SCRIPT in `run-parts --list $HOME/.xsession.d`; do
		echo -n "Sourcing ${SCRIPT}... "
		. $SCRIPT
		echo "done!"
	done

	[[ -e $HOME/.xsession-exit ]] && break

	echo "Restarting X session (touch $HOME/.xsession-exit to exit)..."
done

rm -f $HOME/.xsession-exit

exit 0
