if &t_Co == 256
	colo distinguished
endif

if &term =~ "rxvt-256color"
	" Instantly leave insert mode when pressing <Esc> {{{
		" This works by disabling the mapping timeout completely in normal mode,
		" and enabling it in insert mode with a very low timeout length.
		augroup FastEscape
			autocmd!

			set notimeout
			set ttimeout
			set timeoutlen=10

			au InsertEnter * set timeout
			au InsertLeave * set notimeout
		augroup END
	" }}}
	" Change cursor color in insert mode {{{
		let &t_SI="]12;\#89b6e2\x7"
		let &t_EI="]12;\#dd4010\x7"
	" }}}
	" Use custom fillchars/listchars/showbreak icons {{{
		set list
		set fillchars=vert:┇,fold:┄,diff:╱
		set listchars=tab:⋮\ ,trail:⌴,eol:·,precedes:◂,extends:▸
		set showbreak=↪
	" }}}
	" Disable list/listchars on selected filetypes {{{
		augroup ListFiletypes
			autocmd!

			au FileType text,tex,latex setl nolist
			au FileType diff setl listchars+=trail:\ " Disable trailing space chars in diff files
		augroup END
	" }}}
endif
