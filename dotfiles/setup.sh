#!/bin/sh
# Script to automatically create dotfile symlinks

FROM="`dirname \`which $0\``"

if [[ -d "$1" ]]; then
	TO="$1"
else
	TO="$PWD"
fi

cd $FROM

for FILE in `find -L . -maxdepth 1 -wholename './.*' -printf '%P\n'`; do
	TO_FILE="$TO/$FILE"

	if [[ -e $TO_FILE ]] || [[ -h $TO_$FILE ]]; then
		read -p "File $TO_FILE exists, overwrite? [Y/n] " -n 1 -r

		if [[ $REPLY =~ ^[Nn]$ ]]; then
			echo
			echo "Skipping $FILE"
			echo

			continue
		fi

		rm $TO_FILE
	fi

	echo "Creating symlink to $FILE"
	echo

	ln -sT "$FROM/$FILE" $TO_FILE
done
