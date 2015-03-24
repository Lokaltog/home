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
alias scu "systemctl --user"

set fish_greeting

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

set -x EDITOR "emacs"

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
set -x PATH $PATH $HOME/.local/bin ./node_modules/.bin /usr/bin/vendor_perl/

set -x TERM xterm-256color

source $HOME/.homesick/repos/homeshick/homeshick.fish
