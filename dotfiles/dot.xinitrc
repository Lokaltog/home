# vim:set ft=sh:

# set keymap to dvorak
setxkbmap us dvp \
	-option terminate:ctrl_alt_bksp \
	-option keypad:atm \
	-option lv3:ralt_switch

# custom key mappings
xmodmap \
	-e 'remove Lock = Caps_Lock' \
	-e 'add Lock = Escape' \
	-e 'keysym Caps_Lock = Escape' \
	-e 'keysym Escape = Caps_Lock' \
	-e 'keycode 10 = ampersand     1' \
	-e 'keycode 11 = bracketleft   2 currency' \
	-e 'keycode 12 = braceleft     3 cent' \
	-e 'keycode 13 = braceright    4 yen' \
	-e 'keycode 14 = parenleft     5 EuroSign' \
	-e 'keycode 15 = equal         6 sterling' \
	-e 'keycode 16 = asterisk      7' \
	-e 'keycode 17 = parenright    8 onehalf' \
	-e 'keycode 18 = plus          9' \
	-e 'keycode 19 = bracketright  0' \
	-e 'keycode 20 = exclam        percent exclamdown'

# set background
xsetroot -bg "#121212" -mod 3 3 &
xrdb -merge ~/.Xresources &
feh --bg-center ~/.sync/eos.png &

# set mouse/keyboard speed
xset m 2 0 &
xset r rate 300 30 &

# start statnot
statnot ~/sync/conf/statnot.py &

# start default apps
urxvtd -q -o -f
~/sync/bin/chromium &

# disable bluetooth
echo disable | sudo tee -a /proc/acpi/ibm/bluetooth

# start dwm
exec dwm
