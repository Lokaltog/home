export GTK2_RC_FILES="$HOME/.config/gtk-2.0/gtkrc"

# Colorize less
export LESS_TERMCAP_mb=$(printf "\e[1;31m") \
LESS_TERMCAP_md=$(printf "\e[1;31m") \
LESS_TERMCAP_me=$(printf "\e[0m") \
LESS_TERMCAP_se=$(printf "\e[0m") \
LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
LESS_TERMCAP_ue=$(printf "\e[0m") \
LESS_TERMCAP_us=$(printf "\e[1;32m")

# Load keyboard layout
XKBDIR="$HOME/.config/.xkb"
XKBMAPFILE="$XKBDIR/keymap/`hostname`"

[[ -r "$XKBMAPFILE" ]] || XKBMAPFILE="${XKBMAPFILE%/*}/custom"

xkbcomp -I"$XKBDIR" "$XKBMAPFILE" "${DISPLAY%%.*}"

unset XKBDIR XKBMAPFILE

# Load X resources
if [[ -f $HOME/.Xresources ]]; then
	xrdb -merge $HOME/.Xresources
fi

# Disable bell
xset b 0 &

# Set keyboard repeat rate
xset r rate 200 50 &

# Set mouse acceleration
xinput --set-prop 'Razer Razer DeathAdder' 'Device Accel Profile' -1
xinput --set-prop 'Contour Design RollerMouse Pro' 'Device Accel Profile' 0
xinput --set-ptr-feedback 'Contour Design RollerMouse Pro' 3 10 -1
xinput --set-prop 'TPPS/2 IBM TrackPoint' 'Device Accel Profile' 1
xinput --set-ptr-feedback 'TPPS/2 IBM TrackPoint' 1 5 1

# Set font paths
xset +fp ~/.fonts
xset fp rehash

# Set default cursor
xsetroot -cursor_name left_ptr

# Start WM
sxhkd &
exec bspwm
