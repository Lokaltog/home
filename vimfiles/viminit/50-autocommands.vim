augroup General " {{{
	autocmd!
	" Custom psql highlighting {{{
		au BufNewFile,BufRead *.sql set ft=psql
	" }}}
	" Nginx highlighting {{{
		au BufNewFile,BufRead /etc/nginx/conf/* set ft=nginx
	" }}}
	" Script templates {{{
		au BufNewFile *.sh  so ~/.vim/templates/tpl.sh
		au BufNewFile *.py  so ~/.vim/templates/tpl.py
		au BufNewFile *.php so ~/.vim/templates/tpl.php
	" }}}
	" Help file settings {{{
		function! s:SetupHelpWindow()
			wincmd L
			vertical resize 80
			setl nonumber winfixwidth colorcolumn=

			let b:stl = "#[Branch] HELP#[BranchS] [>] #[FileName]%<%t #[FileNameS][>>]%* %=#[LinePercentS][<<]#[LinePercent] %p%% " " Set custom statusline

			nnoremap <buffer> <Space> <C-]> " Space selects subject
			nnoremap <buffer> <BS>    <C-T> " Backspace to go back
		endfunction

		au FileType help au BufEnter,BufWinEnter <buffer> call <SID>SetupHelpWindow()
	" }}}
	" Override SASS defaults {{{
		au FileType sass exec 'set noexpandtab shiftwidth=' . s:tabwidth . ' tabstop=' . s:tabwidth . ' softtabstop=' . s:tabwidth
	" }}}
	" Disable Syntastic for selected filetypes {{{
		au FileType sass SyntasticDisable
	" }}}
	" Fix space highlighting in diff files {{{
		au FileType diff hi clear RedundantSpaces
			\ | hi DiffCol ctermbg=238 cterm=bold
			\ | match DiffCol /^[ +-]\([+-]\)\@!/
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
	" Fix gitcommit formatting {{{
		au FileType gitcommit setl formatoptions+=t formatoptions-=l textwidth=72 colorcolumn=72
	" }}}
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
augroup VimFiles " {{{
	autocmd!

	au Filetype vim noremap <buffer> <F1> <Esc>:help <C-r><C-w><CR>
augroup END " }}}
