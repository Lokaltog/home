" Universal FoldText function {{{
	function! FoldText(...)
		" This function uses code from doy's vim-foldtext: https://github.com/doy/vim-foldtext
		" Prepare fold variables {{{
			" Use function argument as line text if provided
			let l:line = a:0 > 0 ? a:1 : getline(v:foldstart)

			let l:line_count = v:foldend - v:foldstart + 1
			let l:indent = repeat(' ', indent(v:foldstart))

			let l:w_win = winwidth(0)
			let l:w_num = getwinvar(0, '&number') * getwinvar(0, '&numberwidth')
			let l:w_fold = getwinvar(0, '&foldcolumn')
		" }}}
		" Handle diff foldmethod {{{
			if &fdm == 'diff'
				let l:text = printf('ǒ %s matching lines Ǔ', l:line_count)

				" Center-align the foldtext
				return repeat('Ć', (l:w_win - strchars(l:text) - l:w_num - l:w_fold) / 2) . l:text
			endif
		" }}}
		" Handle other foldmethods {{{
			let l:text = l:line
			" Remove foldmarkers {{{
				let l:foldmarkers = split(&foldmarker, ',')
				let l:text = substitute(l:text, '\V' . l:foldmarkers[0] . '\%(\d\+\)\?\s\*', '', '')
			" }}}
			" Remove comments {{{
				let l:comment = split(&commentstring, '%s')

				if l:comment[0] != ''
					let l:comment_begin = l:comment[0]
					let l:comment_end = ''

					if len(l:comment) > 1
						let l:comment_end = l:comment[1]
					endif

					let l:pattern = '\V' . l:comment_begin . '\s\*' . l:comment_end . '\s\*\$'

					if l:text =~ l:pattern
						let l:text = substitute(l:text, l:pattern, ' ', '')
					else
						let l:text = substitute(l:text, '.*\V' . l:comment_begin, ' ', '')

						if l:comment_end != ''
							let l:text = substitute(l:text, '\V' . l:comment_end, ' ', '')
						endif
					endif
				endif
			" }}}
			" Remove preceding non-word characters {{{
				let l:text = substitute(l:text, '^\W*', '', '')
			" }}}
			" Remove surrounding whitespace {{{
				let l:text = substitute(l:text, '^\s*\(.\{-}\)\s*$', '\1', '')
			" }}}
			" Make unmatched block delimiters prettier {{{
				let l:text = substitute(l:text, '([^)]*$',   'Ę ĵ ę', '')
				let l:text = substitute(l:text, '{[^}]*$',   'Ę ĵ ę', '')
				let l:text = substitute(l:text, '\[[^\]]*$', 'Ę ĵ ę', '')
			" }}}
			" Add arrows when indent level > 2 spaces {{{
				if indent(v:foldstart) > 2
					let l:cline = substitute(l:line, '^\s*\(.\{-}\)\s*$', '\1', '')
					let l:clen = strlen(matchstr(l:cline, '^\W*'))

					let l:indent = repeat(' ', indent(v:foldstart) - 2)
					let l:text = 'ģ ' . l:text
				endif
			" }}}
			" Prepare fold text {{{
				let l:fnum = printf(' %s đ ', l:line_count)
				let l:ftext = printf('%s%s ', l:indent, l:text)
			" }}}
			return l:ftext . repeat('Ķ', l:w_win - strchars(l:fnum) - strchars(l:ftext) - l:w_num - l:w_fold) . l:fnum
		" }}}
	endfunction
" }}}
" PHP FoldText function {{{
	function! FoldText_PHP()
		" This function uses code from phpfolding.vim
		let l:curline = v:foldstart
		let l:line = getline(l:curline)
		" Did we fold a DocBlock? {{{
			if strridx(l:line, '#@+') != -1
				if (matchstr(l:line, '^.*#@+..*$') == l:line)
					let l:line = substitute(l:line, '^.*#@+', '', 'g') . ' ' . g:phpDocBlockIncludedPostfix
				else
					let l:line = getline(l:curline + 1) . ' ' . g:phpDocBlockIncludedPostfix
				endif
		" }}}
		" Did we fold an API comment block? {{{
			elseif strridx(l:line, "\/\*\*") != -1
				let s:state = 0

				while l:curline < v:foldend
					let l:loopline = getline(l:curline)

					if s:state == 0 && strridx(l:loopline, "\*\/") != -1
						let s:state = 1
					elseif s:state == 1 && (matchstr(l:loopline, '^\s*$') != l:loopline)
						break
					endif

					let l:curline = l:curline + 1
				endwhile

				let l:line = getline(l:curline)
			endif
		" }}}
		" Cleanup {{{
			let l:line = substitute(l:line, '/\*\|\*/\d\=', '', 'g')
			let l:line = substitute(l:line, '^\s*\*\?\s*', '', 'g')
			let l:line = substitute(l:line, '{$', '', 'g')
			let l:line = substitute(l:line, '($', '(...)', 'g')
		" }}}
		" Append postfix if there is PhpDoc in the fold {{{
			if l:curline != v:foldstart
				let l:line = l:line . " " . g:phpDocIncludedPostfix . " "
			endif
		" }}}
		return FoldText(l:line)
	endfunction
" }}}
" Enable PHP FoldText function {{{
	let g:DisableAutoPHPFolding = 1

	au FileType php EnableFastPHPFolds
	au FileType php set foldtext=FoldText() | setl foldtext=FoldText_PHP()
" }}}

