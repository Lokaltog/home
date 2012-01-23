" Highlight problematic whitespace (spaces before tabs) {{{
	hi ProblematicSpaces ctermfg=214 ctermbg=160 cterm=bold

	match ProblematicSpaces / \+\ze\t/
" }}}
" Highlight conflict markers {{{
	match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
" }}}
