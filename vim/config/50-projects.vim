try
	let s:projectpath = readfile($XDG_CONFIG_HOME .'/projectpath')[0]
catch
	let s:projectpath = ''
endtry

if ! empty(s:projectpath) && isdirectory(s:projectpath)
	let s:projectsessionfile = s:projectpath . "/vim/session.vim"

	let &path = s:projectpath . "/**," . &path
	let &tags = s:projectpath . "/vim/tags," . &tags

	" CScope preferences {{{
		set nocsverb
		silent! exec "cd " . s:projectpath
		silent! exec "cscope add " . s:projectpath . "/vim/cscope.out"
		set csverb

		set cscopetag
		set cscopetagorder=0
	" }}}
	augroup Project " {{{
		autocmd!
		" Highlight project tags {{{
			au BufReadPost,CursorHold *.php silent! exec "source " . s:projectpath . "/vim/tags.php-hl.vim"
		" }}}
		" Highlight project tags {{{
			au BufReadPost,CursorHold *.py  silent! exec "source " . s:projectpath . "/vim/tags.python-hl.vim"
		" }}}
	augroup END " }}}
endif
