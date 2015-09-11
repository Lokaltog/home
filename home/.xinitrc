# Update RC file locations for apps that don't support XDG
export GTK2_RC_FILES="$HOME/.config/gtk-2.0/gtkrc"
export HGRCPATH="$HOME/.config/hg/hgrc"
export PASSWORD_STORE_DIR="$HOME/.config/password-store"
export GNUPGHOME="$HOME/.config/gnupg"

# Colorize less
export LESS_TERMCAP_mb=$(printf "\e[1;31m") \
LESS_TERMCAP_md=$(printf "\e[1;31m") \
LESS_TERMCAP_me=$(printf "\e[0m") \
LESS_TERMCAP_se=$(printf "\e[0m") \
LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
LESS_TERMCAP_ue=$(printf "\e[0m") \
LESS_TERMCAP_us=$(printf "\e[1;32m")

# Load keyboard layout
XKBDIR="$HOME/.config/xkb"
XKBMAPFILE="$XKBDIR/keymap/custom"
xkbcomp -I"$XKBDIR" "$XKBMAPFILE" "${DISPLAY%%.*}"

# Load X resources
cat <<EOF | xrdb -merge
Xcursor.theme: Vanilla-DMZ
Xcursor.size: 16

Xft.antialias: 1
Xft.autohint : 0
Xft.hinting  : 1
Xft.hintstyle: hintslight
Xft.lcdfilter: lcddefault
Xft.rgba     : rgb
EOF

# Disable DPMS and screen saver blanking
xset -dpms
xset s off

# Disable bell
xset b 0 &

# Set keyboard repeat rate
xset r rate 200 50 &

# Set font paths
xset +fp ~/.fonts
xset fp rehash

# Set default cursor
xsetroot -cursor_name left_ptr

# Host-specific settings
HOSTNAME="`hostname`"
if [[ "$HOSTNAME" = "lokaltog" ]]; then
    # Workstation
    # Set mouse acceleration
    xinput --set-prop 'Razer Razer DeathAdder' 'Device Accel Profile' -1
    xinput --set-prop 'Contour Design RollerMouse Pro' 'Device Accel Profile' 0
    xinput --set-ptr-feedback 'Contour Design RollerMouse Pro' 3 10 -1
fi
if [[ "$HOSTNAME" = "regiontog" ]]; then
    # Laptop
    # Set mouse acceleration
    xinput --set-prop 'TPPS/2 IBM TrackPoint' 'Device Accel Profile' 1
    xinput --set-ptr-feedback 'TPPS/2 IBM TrackPoint' 1 5 1

    # Set monitor DPI
    cat <<EOF | xrdb -merge
Xft.dpi: 96
EOF
fi

# Start gpg-agent daemon
if [[ -x /usr/bin/gpg-agent ]]; then
    kill $(ps ux | awk '/gpg-agent/ && !/awk/ {print $2}') >/dev/null 2>&1
fi
if [[ -x /usr/bin/gpg-agent ]]; then
    eval "$(/usr/bin/gpg-agent --daemon)"
fi

# Start WM
sxhkd -m -1 &
candybar &
exec bspwm
