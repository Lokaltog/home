# Lokaltog's .zshrc
# -----------------
# vim:set ft=zsh:

# show dopefish {{{
	cat ~/sync/etc/dopefish
# }}}
# environment variables {{{
	# command history {{{
		export HISTFILE=~/.zshhist
		export HISTSIZE=10000
		export SAVEHIST=$HISTSIZE
	# }}}
	# colors {{{
		export GREP_COLORS="38;5;230:sl=38;5;240:cs=38;5;100:mt=38;5;161:fn=38;5;197:ln=38;5;212:bn=38;5;44:se=38;5;166"
		export LS_COLORS="*.tar.bz2=38;5;226:*.tar.xz=38;5;130:*PKGBUILD=48;5;233;38;5;160:*.html=38;5;213:*.htm=38;5;213:*.vim=38;5;142:*.css=38;5;209:*.screenrc=38;5;120:*.procmailrc=38;5;120:*.zshrc=38;5;120:*.bashrc=38;5;120:*.xinitrc=38;5;120:*.vimrc=38;5;120:*.htoprc=38;5;120:*.muttrc=38;5;120:*.gtkrc-2.0=38;5;120:*.fehrc=38;5;120:*.rc=38;5;120:*.md=38;5;130:*.markdown=38;5;130:*.conf=38;5;148:*.h=38;5;81:*.rb=38;5;192:*.c=38;5;110:*.diff=38;5;31:*.yml=38;5;208:*.pl=38;5;178:*.csv=38;5;136:tw=38;5;003:*.chm=38;5;144:*.bin=38;5;249:*.pdf=38;5;203:*.mpg=38;5;38:*.ts=38;5;39:*.sfv=38;5;191:*.m3u=38;5;172:*.txt=38;5;192:*.log=38;5;190:*.swp=38;5;241:*.swo=38;5;240:*.theme=38;5;109:*.zsh=38;5;173:*.nfo=38;5;113:mi=38;5;124:or=38;5;160:ex=38;5;197:ln=target:pi=38;5;130:ow=38;5;208:fi=38;5;007:so=38;5;167:di=38;5;30:*.pm=38;5;197:*.pl=38;5;166:*.sh=38;5;243:*.patch=38;5;37:*.tar=38;5;118:*.tar.gz=38;5;172:*.zip=38;5;11::*.rar=38;5;11:*.tgz=38;5;11:*.7z=38;5;11:*.mp3=38;5;173:*.flac=38;5;166:*.mkv=38;5;115:*.avi=38;5;114:*.wmv=38;5;113:*.jpg=38;5;66:*.jpeg=38;5;67:*.png=38;5;68:*.pacnew=38;5;33"
	# }}}
	# locale {{{
		export LANG="en_US.utf8"
		export LC_CTYPE="nb_NO.utf8"
		export LC_NUMERIC="nb_NO.utf8"
		export LC_TIME="nb_NO.utf8"
		export LC_COLLATE="nb_NO.utf8"
		export LC_MONETARY="nb_NO.utf8"
		export LC_MESSAGES="en_US.utf8"
		export LC_PAPER="nb_NO.utf8"
		export LC_NAME="nb_NO.utf8"
		export LC_ADDRESS="nb_NO.utf8"
		export LC_TELEPHONE="nb_NO.utf8"
		export LC_MEASUREMENT="nb_NO.utf8"
		export LC_IDENTIFICATION="nb_NO.utf8"
		export LC_ALL=""
	# }}}
	# build settings {{{
		export CFLAGS="-march=native -mtune=native -O3 -pipe"
		export CXXFLAGS="$CFLAGS"
		export LDFLAGS="-Wl,--hash-style=gnu -Wl,--as-needed"
		export MAKEFLAGS="-j6"
	# }}}
	# application preferences {{{
		export LESS_VIM="vim -R \
			-c 'let no_plugin_maps = 1' \
			-c 'set foldlevel=999 scrolloff=999 mouse=h nolist nonumber laststatus=0 foldcolumn=0' \
			-c 'runtime! macros/less.vim' \
			-c 'map <space> <c-d>' \
			-c 'hi RedundantSpaces none'"
		export PAGER="$LESS_VIM -"
		export MANPAGER="sh -c \"unset PAGER; col -b -x | $LESS_VIM \
			-c 'set ft=man' \
			-c 'map K :Man <c-r>=expand(\\\"<cword>\\\")<cr><cr>' \
			-\""
		export EDITOR="vim"
		export VISUAL="vim"
		export BROWSER="chromium"
		export WINEARCH="win32"
	# }}}
# }}}
# zsh options {{{
	typeset -g -A key
	typeset -U path cdpath fpath manpath

	setopt auto_cd # change dir without cd
	setopt extended_glob # regex globbing
	setopt print_exit_value # print exit value of programs with non-zero exit status
	setopt notify # report the status if background jobs immediately
	setopt complete_in_word # not just at the end
	setopt always_to_end # when complete from middle, move cursor
	setopt no_match # show error if pattern has no matches
	setopt no_beep # disable beeps
	setopt list_packed # compact completion lists
	setopt list_types # show types in completion
	setopt rec_exact # recognize exact, ambiguous matches
	setopt hist_verify # when using ! cmds, confirm first
	setopt hist_ignore_all_dups # ignore dups in command history
	setopt hist_ignore_space # don't add commands prepended by whitespace to history
	setopt append_history # allow multiple sessions to append to the history file
	setopt extended_history # save additional info to history file
	setopt inc_append_history # append commands to history immediately
	setopt prompt_subst # enable variable substitution in prompt
	setopt correct # command correction
	setopt dvorak # correkt dvorak typing mistakes
	setopt short_loops # allow short loops
# }}}
# keybindings {{{
	bindkey -v

	bindkey '^?' backward-delete-char
	bindkey '^[[1~' beginning-of-line # Home
	bindkey '^[[4~' end-of-line # End
	bindkey '^[[3~' delete-char # Del
	bindkey '^[[5~' up-line-or-history  # Page Up
	bindkey '^[[6~' down-line-or-history # Page Down
	bindkey "^[[7~" beginning-of-line # Home
	bindkey "^[[8~" end-of-line # End
	bindkey '^[[A' up-line-or-search # Up
	bindkey '^[[D' backward-char # Left
	bindkey '^[[B' down-line-or-search # Down
	bindkey '^[[C' forward-char # Right
	bindkey "^[OH" beginning-of-line
	bindkey "^[OF" end-of-line
# }}}
# aliases {{{
	# general aliases {{{
		alias sudo="sudo -E"
		alias -- +="sudo"
		alias sv="+ vim"
		alias regiontog="ssh -t regiontog 'screen -UODRa -p 0'"
		alias ls="ls --color=auto"
		alias lsa="ls -AahXBFov --color=auto --indicator-style=file-type --group-directories-first"
		alias ss="+ -s"
		alias grep="grep --color=auto"
		alias sd="+ shutdown -h now"
		alias rb="+ reboot"
		alias p="+ pacman"
		alias pr="packer --noedit"
		alias sshfs="sshfs -o reconnect,nosuid,nodev,allow_other,uid=1000,gid=100"
		alias sy="p -Syu"
		alias df="df -h"
		alias du="du -h"
		alias rmr="rm -rf"
		alias mv="nocorrect mv -iv"
		alias cp="nocorrect cp -iv"
		alias mkdir="nocorrect mkdir -vp"
		alias chmod="chmod -v"
		alias chown="chown -v"
	# }}}
	# default sudo commands {{{
		for cmd in mount ifconfig pacman chmod chown; do
			alias $cmd="+ $cmd"
		done
	# }}}
	# multitail aliases {{{
		alias tsys="+ multitail \
			-n 200 -t Daemons /var/log/daemon.log \
			-n 200 -t Kernel /var/log/kernel.log \
			-n 200 -t Errors -wh 10 /var/log/errors.log"

		alias tserv="+ multitail \
			-n 200 -t \"NginX Access\" /var/log/nginx/access.log \
			-n 200 -t \"NginX Error\" /var/log/nginx/error.log \
			-n 200 -t \"Postgres\" -wh 10 /var/log/postgresql.log"

		alias tsquid="+ multitail \
			-n 200 -t \"Squid Access\" /var/log/squid/access.log \
			-n 200 -t \"Squid Cache\" -wh 20 /var/log/squid/cache.log"
	# }}}
	# suffix aliases {{{
		alias -s html=$BROWSER
		alias -s {php,tpl,txt,PKGBUILD}=$EDITOR
		alias -s {jpg,JPG,jpeg,JPEG,png,PNG,gif,GIF}="feh -FZd"
		alias -s {mpg,mpeg,avi,ogm,wmv,m4v,mp4,mov}="mplayer -idx"
	# }}}
# }}}
# completion {{{
	autoload -Uz compinit && compinit

	# ssh hosts completion {{{
		[ -f ~/.ssh/config ] && : ${(A)ssh_config_hosts:=${${${${(@M)${(f)"$(<~/.ssh/config)"}:#Host *}#Host }:#*\**}:#*\?*}}
		[ -f ~/.ssh/known_hosts ] && : ${(A)ssh_known_hosts:=${${${(f)"$(<$HOME/.ssh/known_hosts)"}%%\ *}%%,*}}
		zstyle ':completion:*:*:*' hosts $ssh_config_hosts $ssh_known_hosts
	# }}}
	# general rules {{{
		# enable completion cache
		zstyle ':completion:*' completer _complete _ignored
		zstyle ':completion:*' expand prefix suffix
		zstyle ':completion:*' group-name ''
		zstyle ':completion:*' ignore-parents parent pwd .. directory
		zstyle ':completion:*' insert-unambiguous true
		zstyle ':completion:*' matcher-list ''
		zstyle ':completion:*' menu select=long
		zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
		zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
		zstyle ':completion:*' squeeze-slashes true
		zstyle ':completion::complete:*' use-cache 1
		zstyle ':completion::complete:*' cache-path ~/.zshcache
		zstyle ':completion:*:*:kill:*' menu yes select
		zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
		zstyle ':completion:*:*:killall:*:processes' command 'ps --forest -A -o pid,user,cmd'
		zstyle ':completion:*:processes-names' command 'ps axho command'
		zstyle ':completion:*:processes' command 'ps -au$USER -o pid,time,cmd|grep -v "ps -au$USER -o pid,time,cmd"'
		zstyle ':completion:*:cd:*' ignored-patterns '(*/)#lost+found'
		zstyle ':completion:*:matches' group yes
		zstyle ':completion:*:options' description yes
		zstyle ':completion:*:options' auto-description '%d'
		zstyle ':completion:*:descriptions' format $'\e[01;33m-- %d --\e[0m'
		zstyle ':completion:*:messages' format $'\e[01;35m-- %d --\e[0m'
		zstyle ':completion:*:warnings' format $'\e[01;31m-- no matches found --\e[0m'
		zstyle ':completion:*:manuals' separate-sections true
		zstyle ':completion:*:manuals.*' insert-sections   true
		zstyle ':completion:*:man:*' menu yes select
		zstyle ':completion:*:rm:*' ignore-line yes
		zstyle ':completion:*:cp:*' ignore-line yes
		zstyle ':completion:*:mv:*' ignore-line yes
	# }}}

	# generic gnu completions for apps that understang long options (--option)
	compdef _gnu_generic slrnpull make df du mv cp makepkg packer
# }}}
# root settings {{{
	if [[ $UID == 0 ]]; then
		# close root shell after 180 seconds
		export TMOUT=180
	fi
# }}}
# vcs info {{{
	autoload -Uz vcs_info

	zstyle ':vcs_info:*' enable git svn hg
	zstyle ':vcs_info:*' formats '%F{5}%s%f %F{7}%BÍ %b'

	precmd(){
		vcs_info
	}
# }}}
# prompt {{{
	if [[ "$TERM" == "linux" ]]; then
		PR_SSH='SSH'
		PR_START='%%'
	else
		PR_SSH='ê'
		PR_START='ý'
	fi
	# regular prompt {{{
		PROMPTDIR='%b%F{6}%3~%f'

		if [[ $UID == 0 ]]; then
			PROMPT='%B%F{1}%n%f '$PROMPTDIR' %B%F{1}${PR_START}%f%b '
		else
			PROMPT='%B%F{4}%n%f '$PROMPTDIR' %B%F{7}${PR_START}%f%b '
		fi

		# check if user is logged in via ssh
		[[ ! -z "$SSH_CLIENT" ]] && PROMPT='%F{3}${PR_SSH} %M%f %F{0}%B│%b%f '$PROMPT
	# }}}
	# right prompt {{{
		RPROMPT='${vcs_info_msg_0_}'
	# }}}
	# list prompt - don't ask 'do you want to see all ...' in menu selection {{{
		LISTPROMPT=''
	# }}}
	# spelling prompt {{{
		SPROMPT='zsh: correct '%R' to '%r'? ([Y]es/[N]o/[E]dit/[A]bort) '
	# }}}
# }}}
# zle stuff {{{
	# smart dot (e.g. enter ..../dir) {{{
		smartdot(){
			if [[ $LBUFFER = *.. ]]; then
				LBUFFER+=/..
			else
				LBUFFER+=.
			fi
		}
		zle -N smartdot smartdot
		bindkey . smartdot
	# }}}
	# quick add sudo {{{
		insert_sudo(){
			if [[ $LBUFFER != "sudo "* && $LBUFFER != '+ '* ]]; then
				LBUFFER="+ $LBUFFER"
			fi
		}
		zle -N insert-sudo insert_sudo
		bindkey "^[r" insert-sudo
	# }}}
	# integrate ranger {{{
		integrate_ranger(){
			local before="$(pwd)"
			ranger $before <$TTY
			local after="$(grep \^\' ~/.ranger/bookmarks)"
			after[1,2]=
			if [[ $before != $after ]]; then
				cd $after
				print "ranger: $before -> $after"
			fi
			zle redisplay
			precmd
		}
		zle -N integrated-ranger integrate_ranger
		bindkey "^F" integrated-ranger
	# }}}
# }}}
# functions {{{
	# service control {{{
		service(){
			for s in ${*[2,-1]}; do
				if [[ -n $1 ]]; then
					[[ ! -r /etc/rc.d/$s ]] && echo "Cannot control service '$s'" && continue
					sudo /etc/rc.d/$s $1
				else
					/etc/rc.d/$s
				fi
			done
		}
		start()  { service start $*   }
		stop()   { service stop $*    }
		restart(){ service restart $* }
	# }}}
# }}}
