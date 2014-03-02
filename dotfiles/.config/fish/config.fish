alias + "sudo -E"
alias l "ls -ABFhovX --color=auto --group-directories-first --time-style=long-iso"
alias p "+ pacman"
alias pr "packer --noedit"
alias y "yaourt"
alias sd "+ shutdown -h now"
alias rb "+ reboot"
alias sshfs "sshfs -o reconnect,nosuid,nodev,allow_other,uid=1000,gid=100"
alias df "df -h"
alias du "du -h"
alias rmr "rm -rf"
alias mkdir "mkdir -vp"
alias sc "sudo systemctl"
alias em 'emacsclient -nw -a "" -c'

set fish_greeting

function fish_prompt
	powerline shell left -r fish_prompt
end
function fish_right_prompt
	powerline shell right -r fish_prompt
end

if test -f "/usr/lib/stderred.so"
	set -x LD_PRELOAD "/usr/lib/stderred.so:$LD_PRELOAD"
end

function smartdot
	commandline -i (commandline -b | awk '{print $0 ~ /\.\.$/ ? "/.." : "."}')
end
function smartsudo
	commandline -r (commandline -b | awk '{print $0 ~ /^(sudo|\+)\ / ? "" : "+ "}')(commandline -b)
end

function fish_user_key_bindings
	bind . 'smartdot'
	bind \es 'smartsudo'
end

set -x EDITOR "emt"

set -x LANG "en_US.utf8"
set -x LC_CTYPE "nb_NO.utf8"
set -x LC_NUMERIC "nb_NO.utf8"
set -x LC_TIME "nb_NO.utf8"
set -x LC_COLLATE "nb_NO.utf8"
set -x LC_MONETARY "nb_NO.utf8"
set -x LC_MESSAGES "en_US.utf8"
set -x LC_PAPER "nb_NO.utf8"
set -x LC_NAME "nb_NO.utf8"
set -x LC_ADDRESS "nb_NO.utf8"
set -x LC_TELEPHONE "nb_NO.utf8"
set -x LC_MEASUREMENT "nb_NO.utf8"
set -x LC_IDENTIFICATION "nb_NO.utf8"
set -x LC_ALL ""

set -x MAKEFLAGS "-j6"

set -x PATH $PATH $HOME/.local/bin $HOME/.gem/ruby/1.9.1/bin $HOME/sync/bin /usr/bin/vendor_perl
set -x NODE_PATH /usr/lib/node_modules $NODE_PATH

eval (dircolors -c ~/sync/modules/dircolors/LS_COLORS)

# virtualenv support
. ~/sync/modules/virtualfish/virtual.fish
. ~/sync/modules/virtualfish/auto_activation.fish
