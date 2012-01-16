nnoremap <buffer> q :bd<CR>

function! s:SetupHelpWindow()
	wincmd L
	vertical resize 80
	setl nonumber winfixwidth colorcolumn=

	nnoremap <buffer> <Space> <C-]> " Space selects subject
	nnoremap <buffer> <BS>    <C-T> " Backspace to go back
endfunction

au BufEnter,BufWinEnter <buffer> call <SID>SetupHelpWindow()
