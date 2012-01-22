augroup General " {{{
	autocmd!
	" Custom psql highlighting {{{
		au BufNewFile,BufRead *.sql set ft=pgsql
	" }}}
	" Nginx highlighting {{{
		au BufNewFile,BufRead /etc/nginx/conf/* set ft=nginx
	" }}}
	" Script templates {{{
		au BufNewFile *.sh  so ~/.vim/templates/tpl.sh
		au BufNewFile *.py  so ~/.vim/templates/tpl.py
		au BufNewFile *.php so ~/.vim/templates/tpl.php
	" }}}
	" Load .Xdefaults/.Xresources on save {{{
		au BufWritePost,FileWritePost ~/.Xdefaults,~/.Xresources silent! !xrdb -load % >/dev/null 2>&1
	" }}}
	" Support Genshi templates {{{
		au FileType html set ft=genshi
	" }}}
augroup END " }}}
augroup Formatting " {{{
	autocmd!
	" Format plain text and e-mails correctly {{{
		au BufNewFile,BufRead *.txt setl ft=text
		au FileType mail,text,tex setl formatoptions+=t formatoptions-=l textwidth=72 colorcolumn=72
	" }}}
	" Use foldmarkers for specific filetypes {{{
		au FileType sass,javascript,psql,vim setl foldmethod=marker foldlevel=0
	" }}}
augroup END" }}}
augroup Whitespace " {{{
	autocmd!
	" Remove trailing whitespace from selected filetypes {{{
		function! s:StripTrailingWhitespace()
			normal mZ

			%s/\s\+$//e

			normal `Z
		endfunction

		au FileType html,css,sass,javascript,php,python,ruby,psql,vim au BufWritePre <buffer> :silent! call <SID>StripTrailingWhitespace()
	" }}}
augroup END " }}}
