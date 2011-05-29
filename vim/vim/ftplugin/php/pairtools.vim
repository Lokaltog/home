" Enable PairTools modules {{{
	let g:pairtools_php_pairclamp = 1
	let g:pairtools_php_tagwrench = 1
	let g:pairtools_php_jigsaw = 1
" }}}
" Configure PairClamp {{{
	let g:pairtools_php_autoclose = 1
	let g:pairtools_php_forcepairs = 1
	let g:pairtools_php_closepairs = "(:),[:],{:},\":\",':',`:`"
	let g:pairtools_php_smartclose = 1
	let g:pairtools_php_smartcloserules = '\w'
	let g:pairtools_php_apostrophe = 0
	let g:pairtools_php_antimagic = 1
	let g:pairtools_php_antimagicfield = "Comment,String"
	let g:pairtools_php_pcexpander = 1
	let g:pairtools_php_pceraser = 1
" }}}
" Configure TagWrench {{{
	let g:pairtools_php_tagwrenchhook = 'tagwrench#BuiltinNoHook'
	let g:pairtools_php_twexpander = 1
	let g:pairtools_php_tweraser = 1
" }}}
