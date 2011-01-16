#!/bin/sh
# Script to automatically create dotfile symlinks

FROM="`dirname \`which $0\``"

if [ -d "$1" ]; then
	TO="$1"
else
	TO="$PWD"
fi

cd $FROM

for FILE in dot.*; do
	ln -s "$FROM/$FILE" "$TO/${FILE:3}"
done
