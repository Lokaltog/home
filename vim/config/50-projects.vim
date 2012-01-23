let s:projectpath = system("cat $XDG_CONFIG_HOME/projectpath 2&>/dev/null")

if ! empty(s:projectpath) && isdirectory(s:projectpath)
	let s:projectsessionfile = s:projectpath . "/vim/session.vim"

	let &path = s:projectpath . "/**," . &path
	let &tags = s:projectpath . "/vim/tags," . &tags
	let g:NERDTreeBookmarksFile = s:projectpath . "/vim/bookmarks"

	" CScope preferences {{{
		set nocsverb
		silent! exec "cd " . s:projectpath
		silent! exec "cscope add " . s:projectpath . "/vim/cscope.out"
		set csverb

		set cscopetag
		set cscopetagorder=0
	" }}}
	" Session options {{{
		set sessionoptions=blank,buffers,folds
		" Session handling functions {{{
			function! s:SessionException()
				" Vim called with arguments
				if argc() != 0
					return 1
				endif

				" Git commit messages
				if match(expand("%"), "\.git\/COMMIT") >= 0
					return 1
				endif

				return 0
			endfunction

			function! s:MakeSession()
				if s:SessionException() == 1
					return
				endif

				exec "mksession! " . s:projectsessionfile

				call writefile(
					\ [
						\ 'colorscheme ' . g:colors_name
					\ ],
					\ fnamemodify(s:projectsessionfile, ':p:r') . 'x.vim'
				\ )
			endfunction

			function! s:LoadSession()
				if s:SessionException()
					return
				endif

				" Check that file is readable
				if ! filereadable(s:projectsessionfile)
					return
				endif

				exec "source " . s:projectsessionfile
			endfunction
		" }}}
	" }}}
	augroup Project " {{{
		autocmd!
		" Highlight project tags {{{
			au BufReadPost,CursorHold *.php silent! exec "source " . s:projectpath . "/vim/tags.php-hl.vim"
		" }}}
		" Highlight project tags {{{
			au BufReadPost,CursorHold *.py  silent! exec "source " . s:projectpath . "/vim/tags.python-hl.vim"
		" }}}
		" Write/source session file {{{
			"au VimEnter * nested call s:LoadSession()
			"au VimLeave * call s:MakeSession()
		" }}}
	augroup END " }}}
endif
