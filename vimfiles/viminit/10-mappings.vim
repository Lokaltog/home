let mapleader = ','

" F key mappings {{{
	nnoremap <silent> <F6>  :set number!<CR>
	nnoremap <silent> <F7>  :LustyJuggler<CR>
	nnoremap <silent> <F8>  :TagbarToggle<CR>
	nnoremap <silent> <F9>  :LustyBufferExplorer<CR>
	nnoremap <silent> <F10> :NERDTreeToggle<CR>
	nnoremap <silent> <F11> :GundoToggle<CR>
" }}}
" Tabularize mappings {{{
	vnoremap <silent> <Leader>t=> :Tabularize /=>/l0l1<CR>
	vnoremap <silent> <Leader>t=  :Tabularize /=/l0l1<CR>
	vnoremap <silent> <Leader>t,  :Tabularize /,/l0l1<CR>
	vnoremap <silent> <Leader>t:  :Tabularize /:/l0l1<CR>
" }}}
" Buffer mappings {{{
	nnoremap <silent> <Leader>d :bd<CR>
" }}}
" Quick edit .vimrc {{{
	nnoremap <silent> <Leader>ev :edit   $MYVIMRC<CR>
	nnoremap <silent> <Leader>sv :source $MYVIMRC<CR>
" }}}
" Enter command mode quickly {{{
	nnoremap ; :
" }}}
" Navigate by visual lines {{{
	noremap k gk
	noremap j gj
" }}}
" Clear search highlighting {{{
	nnoremap <silent> <Leader>/ :nohlsearch<CR>
" }}}
" Sudo write {{{
	command! -bar -nargs=0 W  silent! exec "write !sudo tee % >/dev/null"  | silent! edit!
" }}}
" Write and make file executable {{{
	command! -bar -nargs=0 WX silent! exec "write !chmod a+x % >/dev/null" | silent! edit!
" }}}
" Fix broken vim regexes when searching {{{
	" http://stevelosh.com/blog/2010/09/coming-home-to-vim/#important-vimrc-lines
	nnoremap / /\v
	vnoremap / /\v
	cnoremap s/ s/\v
" }}}
" Easier bracket matching {{{
	nnoremap <Tab> %
" }}}
" Split window mappings {{{
	nnoremap K <C-w>k
	nnoremap J <C-w>j
	nnoremap H <C-w>h
	nnoremap L <C-w>l

	nmap <C-k> <C-w>s
	nmap <C-j> <C-w>sJ
	nmap <C-h> <C-w>v
	nmap <C-l> <C-w>vL
" }}}
" Repurpose arrow keys to move lines {{{
	" Inspired by http://jeetworks.com/node/89
	function! s:MoveLineUp()
		call <SID>MoveLineOrVisualUp(".", "")
	endfunction

	function! s:MoveLineDown()
		call <SID>MoveLineOrVisualDown(".", "")
	endfunction

	function! s:MoveVisualUp()
		call <SID>MoveLineOrVisualUp("'<", "'<,'>")
		normal gv
	endfunction

	function! s:MoveVisualDown()
		call <SID>MoveLineOrVisualDown("'>", "'<,'>")
		normal gv
	endfunction

	function! s:MoveLineOrVisualUp(line_getter, range)
		let l_num = line(a:line_getter)
		if l_num - v:count1 - 1 < 0
			let move_arg = "0"
		else
			let move_arg = a:line_getter." -".(v:count1 + 1)
		endif
		call <SID>MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
	endfunction

	function! s:MoveLineOrVisualDown(line_getter, range)
		let l_num = line(a:line_getter)
		if l_num + v:count1 > line("$")
			let move_arg = "$"
		else
			let move_arg = a:line_getter." +".v:count1
		endif
		call <SID>MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
	endfunction

	function! s:MoveLineOrVisualUpOrDown(move_arg)
		let col_num = virtcol(".")
		execute "silent! ".a:move_arg
		execute "normal! ".col_num."|"
	endfunction

	" Arrow key remapping:
	" Up/Dn = move line up/dn
	" Left/Right = indent/unindent
	function! SetArrowKeysAsTextShifters()
		" Normal mode
		nnoremap <silent> <Left>   <<
		nnoremap <silent> <Right>  >>
		nnoremap <silent> <Up>     <Esc>:call <SID>MoveLineUp()<CR>
		nnoremap <silent> <Down>   <Esc>:call <SID>MoveLineDown()<CR>

		" Visual mode
		vnoremap <silent> <Left>   <gv
		vnoremap <silent> <Right>  >gv
		vnoremap <silent> <Up>     <Esc>:call <SID>MoveVisualUp()<CR>
		vnoremap <silent> <Down>   <Esc>:call <SID>MoveVisualDown()<CR>

		" Insert mode
		inoremap <silent> <Left>   <C-D>
		inoremap <silent> <Right>  <C-T>
		inoremap <silent> <Up>     <C-O>:call <SID>MoveLineUp()<CR>
		inoremap <silent> <Down>   <C-O>:call <SID>MoveLineDown()<CR>
	endfunction

	call SetArrowKeysAsTextShifters()
" }}}
" Disable normal mode movement keys {{{
	noremap  <Home> <NOP>
	noremap  <End>  <NOP>
" }}}
" Paste mode toggle {{{
	nnoremap <silent> <F12> :set paste!<CR>
" }}}
