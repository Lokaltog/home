" Basic configuration {{{
	syntax on
	filetype plugin indent on

	set nocompatible
	set tags=tags;.vimtags;
	set nobackup
	set noswapfile
	set nomodeline
	set undofile
	set undodir=~/.vim/tmp
	set autochdir
	set backspace=indent,eol,start
	set fileformats=unix,dos
	set fileformat=unix
	set matchpairs+=<:>
	set shortmess=atToOI
	set iskeyword+=_,$,@,%,#
	set whichwrap=b,s,[,]
	set hidden
	set encoding=utf-8
	set termencoding=utf-8
	" Instantly leave insert mode when pressing <Esc> {{{
		" This works by disabling the mapping timeout completely in normal mode,
		" and enabling it in insert mode with a very low timeout length.
		augroup fastescape
			autocmd!

			set notimeout
			set ttimeout
			set timeoutlen=50

			au InsertEnter * set timeout
			au InsertLeave * set notimeout
		augroup END
	" }}}
	" Define , as map leader {{{
		let mapleader = ','
	" }}}
	" Disable all bells {{{
		set noerrorbells visualbell t_vb=
	" }}}
	" Wild menu {{{
		set wildmenu
		set wildignore=.svn,CVS,.git,.hg,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif
		set wildmode=full
	" }}}
" }}}
" UI options {{{
	" Set colorscheme {{{
		if &t_Co == 256
			colo lokaltog
		else
			colo pablo
		endif
	" }}}
	" URxvt-specific settings {{{
		if &term =~ "rxvt"
			" Change cursor color in insert mode {{{
				silent !echo -ne "]12;\#dd4010\x7"

				let &t_SI="]12;\#89b6e2\x7"
				let &t_EI="]12;\#dd4010\x7"

				au VimLeave * silent !echo -ne "]12;\#dd4010\x7"
			" }}}
			" Use custom fillchars/listchars/showbreak icons {{{
				set fillchars=vert:║,fold:Û,diff:░
				set listchars=tab:Ë\ ,trail:í,eol:î
				set showbreak=Ì
			" }}}
		endif
	" }}}
	set laststatus=2
	set hlsearch
	set incsearch
	set number
	set numberwidth=4
	set ruler
	set scrolloff=10
	set sidescrolloff=20
	set scrolljump=10
	set showcmd
	set noshowmode
	set virtualedit=all
	set confirm
	set updatetime=1500
	set history=1000
	set undolevels=1000
	set pumheight=10
" }}}
" Statusline {{{
	" Statusline update function {{{
		" Inspired by StatusLineHighlight by Ingo Karkat
		function! s:StatusLine(new_stl, type, current)
			let current = (a:current ? "" : "NC")
			let type = a:type
			let new_stl = a:new_stl

			" Prepare current buffer specific text
			" Syntax: #CUR# ... #/CUR#
			let new_stl = substitute(new_stl, '#CUR#\(.\{-,}\)#/CUR#', (a:current ? '\1' : ''), 'g')

			" Prepare statusline colors
			" Syntax: #[ ... ]
			let new_stl = substitute(new_stl, '#\[\(\w\+\)\]', '%#StatusLine'.type.'\1'.current.'#', 'g')

			if &l:stl ==# new_stl
				" Statusline already set, nothing to do
				return
			endif

			if empty(&l:stl)
				" No statusline is set, use my_stl
				let &l:stl = new_stl
			else
				" Check if a custom statusline is set
				let plain_stl = substitute(&l:stl, '%#StatusLine\w\+#', '', 'g')

				if &l:stl ==# plain_stl
					" A custom statusline is set, don't modify
					return
				endif

				" No custom statusline is set, use my_stl
				let &l:stl = new_stl
			endif
		endfunction
	" }}}
	" Set default statusline {{{
		let g:default_stl  = ""
		let g:default_stl .= "#CUR##[Mode] %{substitute(mode(), '', '^V', 'g')} #[ModeS]õ#/CUR#"
		let g:default_stl .= "#[Branch] %(%{substitute(fugitive#statusline(), 'GIT(\\([a-z0-9\\-_\\.]\\+\\))', 'Í \\1', 'gi')}#[BranchS] ó %)" " Git branch
		let g:default_stl .= "%<" " Truncate right
		let g:default_stl .= "#[FileName]%t " " File name
		let g:default_stl .= "#[Error]%(%{SyntasticStatuslineFlag()} %)" " Syntastic error flag
		let g:default_stl .= "#[ModFlag]%(%M %)" " Modified flag
		let g:default_stl .= "#[BufFlag]%([%R%H%W] %)" " RO,HLP,PRV flags
		let g:default_stl .= "#[FileNameS]õ" " Separator
		let g:default_stl .= "#[FunctionName] %(%{cfi#format('%s', '')} %)" " Function name
		let g:default_stl .= "%=" " Right align
		let g:default_stl .= "#[FileFormat] %{&fileformat}" " File format
		let g:default_stl .= "#[FileEncoding] %{(&fenc == '' ? &enc : &fenc)}" " File encoding
		let g:default_stl .= "#[LineNumber]%( %l:%c%V%) " " Line/column/virtual column
		let g:default_stl .= "#[Separator]ò ð #[FileType]%{strlen(&ft) ? toupper(&ft) : 'N/A'} " " File type
		let g:default_stl .= "#[LinePercentS]ô#[LinePercent] %p%% " " Line percentage
		"let g:default_stl .= " %{synIDattr(synID(line('.'),col('.'),1),'name')}" " Current syntax group
	" }}}
	" Set statusline colors {{{
		" Statusline color dict parser {{{
			function! s:StatusLineColors(colors)
				for type in keys(a:colors)
					for name in keys(a:colors[type])
						let colors = {'c': a:colors[type][name][0], 'nc': a:colors[type][name][1]}
						let type = (type == 'NONE' ? '' : type)
						let name = (name == 'NONE' ? '' : name)

						if exists("colors['c'][0]")
							exec 'hi StatusLine'.type.name.' ctermbg='.colors['c'][0].' ctermfg='.colors['c'][1].' cterm='.colors['c'][2]
						endif

						if exists("colors['nc'][0]")
							exec 'hi StatusLine'.type.name.'NC ctermbg='.colors['nc'][0].' ctermfg='.colors['nc'][1].' cterm='.colors['nc'][2]
						endif
					endfor
				endfor
			endfunction
		" }}}
		call <SID>StatusLineColors({
			\   'NONE': {
				\   'NONE'         : [[ 236, 231, 'bold'], [ 232, 244, 'none']]
			\ }
			\ , 'Normal': {
				\   'Mode'         : [[ 214, 235, 'bold'], [                 ]]
				\ , 'ModeS'        : [[ 214, 240, 'bold'], [                 ]]
				\ , 'Branch'       : [[ 240, 250, 'none'], [ 234, 239, 'none']]
				\ , 'BranchS'      : [[ 240, 246, 'none'], [ 234, 239, 'none']]
				\ , 'FileName'     : [[ 240, 231, 'bold'], [ 234, 244, 'bold']]
				\ , 'FileNameS'    : [[ 240, 236, 'bold'], [ 234, 232, 'bold']]
				\ , 'Error'        : [[ 240, 196, 'bold'], [ 234, 239, 'none']]
				\ , 'ModFlag'      : [[ 240, 196, 'bold'], [ 234, 239, 'none']]
				\ , 'BufFlag'      : [[ 240, 244, 'bold'], [ 234, 239, 'none']]
				\ , 'FunctionName' : [[ 236, 247, 'none'], [ 232, 239, 'none']]
				\ , 'FileFormat'   : [[ 236, 244, 'none'], [ 232, 239, 'none']]
				\ , 'FileEncoding' : [[ 236, 244, 'none'], [ 232, 239, 'none']]
				\ , 'LineNumber'   : [[ 236, 248, 'bold'], [ 232, 239, 'none']]
				\ , 'Separator'    : [[ 236, 244, 'none'], [ 232, 239, 'none']]
				\ , 'FileType'     : [[ 236, 248, 'bold'], [ 232, 239, 'none']]
				\ , 'LinePercentS' : [[ 252, 236, 'bold'], [ 234, 232, 'none']]
				\ , 'LinePercent'  : [[ 252, 236, 'bold'], [ 234, 239, 'none']]
			\ }
			\ , 'Insert': {
				\   'Mode'         : [[ 153,  23, 'bold'], [                 ]]
				\ , 'ModeS'        : [[ 153,  31, 'bold'], [                 ]]
				\ , 'Branch'       : [[  31, 117, 'none'], [                 ]]
				\ , 'BranchS'      : [[  31, 117, 'none'], [                 ]]
				\ , 'FileName'     : [[  31, 231, 'bold'], [                 ]]
				\ , 'FileNameS'    : [[  31,  24, 'bold'], [                 ]]
				\ , 'Error'        : [[  31, 196, 'bold'], [                 ]]
				\ , 'ModFlag'      : [[  31, 196, 'bold'], [                 ]]
				\ , 'BufFlag'      : [[  31,  75, 'bold'], [                 ]]
				\ , 'FunctionName' : [[  24, 117, 'none'], [                 ]]
				\ , 'FileFormat'   : [[  24,  75, 'none'], [                 ]]
				\ , 'FileEncoding' : [[  24,  75, 'none'], [                 ]]
				\ , 'LineNumber'   : [[  24,  75, 'bold'], [                 ]]
				\ , 'Separator'    : [[  24,  37, 'none'], [                 ]]
				\ , 'FileType'     : [[  24,  81, 'bold'], [                 ]]
				\ , 'LinePercentS' : [[ 117,  24, 'bold'], [                 ]]
				\ , 'LinePercent'  : [[ 117,  23, 'bold'], [                 ]]
			\ }
		\ })
	" }}}
	augroup StatusLineHighlight " {{{
		autocmd!

		au BufWinEnter,WinEnter,CmdwinEnter,CursorHold,BufWritePost,InsertLeave * call <SID>StatusLine((exists('b:stl') ? b:stl : g:default_stl), 'Normal', 1)
		au WinLeave * call <SID>StatusLine((exists('b:stl') ? b:stl : g:default_stl), 'Normal', 0)
		au InsertEnter,CursorHoldI * call <SID>StatusLine((exists('b:stl') ? b:stl : g:default_stl), 'Insert', 1)
	augroup END " }}}
" }}}
" Layout / Text formatting {{{
	set autoindent
	set wrap
	set textwidth=72
	set formatoptions=croqwanl1
	set ignorecase
	set linebreak
	set shiftwidth=6
	set tabstop=6
	set smartcase
	set smartindent
	set smarttab
	set startofline
	set completeopt=menu,menuone,longest,preview
" }}}
" Folding {{{
	set foldenable
	set foldmethod=marker
	set foldlevel=1
	set foldcolumn=0

	function! VimFold()
		let t = getline(v:foldstart)
		let w = strlen(matchstr(t, '^\s*'))
		let t = substitute(t, '^\W*', '', '')
		let t = substitute(t, '\W*$', '', '')
		let n = v:foldend - v:foldstart + 1

		return repeat(repeat(' ', &tabstop), w).t.' Î'.n.' ÝÏ '
	endfunction

	au FileType vim set foldlevel=0 foldtext=VimFold()
" }}}
" Mappings {{{
	" F key mappings {{{
		nnoremap <silent><F7> :LustyJuggler<CR>
		nnoremap <silent><F8> :TlistToggle<CR>
		nnoremap <silent><F9> :LustyBufferExplorer<CR>
		nnoremap <silent><F10> :NERDTreeToggle<CR>
		nnoremap <silent><F11> :GundoToggle<CR>
	" }}}
	" Quick edit .vimrc {{{
		nnoremap <Leader>ev :e $MYVIMRC<CR>
		nnoremap <Leader>sv :so $MYVIMRC<CR>
	" }}}
	" Enter command mode quickly {{{
		nnoremap ; :
	" }}}
	" Navigate by visual lines {{{
		noremap k gk
		noremap j gj
	" }}}
	" Tab indenting in visual mode {{{
		vnoremap <Tab> >gv
		vnoremap <S-Tab> <gv
	" }}}
	" Clear search highlighting {{{
		nnoremap <silent> <Leader>/ :silent noh<cr>
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
	" }}}
	" Easier bracket matching {{{
		nnoremap <Tab> %
	" }}}
	" Vertically split window and select it  {{{
		nnoremap <Leader>w <C-w>v<C-w>l
	" }}}
	" Easier split window navigation {{{
		nnoremap H <C-w>h
		nnoremap J <C-w>j
		nnoremap K <C-w>k
		nnoremap L <C-w>l
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
			nnoremap <silent> <Left> <<
			nnoremap <silent> <Right> >>
			nnoremap <silent> <Up> <Esc>:call <SID>MoveLineUp()<CR>
			nnoremap <silent> <Down> <Esc>:call <SID>MoveLineDown()<CR>

			" Visual mode
			vnoremap <silent> <Left> <gv
			vnoremap <silent> <Right> >gv
			vnoremap <silent> <S-Up> <Esc>:call <SID>MoveVisualUp()<CR>
			vnoremap <silent> <S-Down> <Esc>:call <SID>MoveVisualDown()<CR>

			" Insert mode
			inoremap <silent> <Left> <C-D>
			inoremap <silent> <Right> <C-T>
			inoremap <silent> <Up> <C-O>:call <SID>MoveLineUp()<CR>
			inoremap <silent> <Down> <C-O>:call <SID>MoveLineDown()<CR>
		endfunction

		call SetArrowKeysAsTextShifters()
	" }}}
	" Disable insert mode movement keys {{{
		noremap <Home> <NOP>
		noremap <End>  <NOP>

		inoremap <Home> <NOP>
		inoremap <End>  <NOP>
	" }}}
	" Mouse toggle {{{
		function! s:ToggleMouse()
			if !exists("old_mouse")
				let old_mouse = "ar"
			endif

			if &mouse == ""
				let &mouse = old_mouse
				echo "Mouse is for Vim (" . &mouse . ")"
			else
				let old_mouse = &mouse
				let &mouse = ""
				echo "Mouse is for terminal"
			endif
		endfunction

		nnoremap <F12> :call <SID>ToggleMouse()<CR>
	" }}}
" }}}
" Autocommands {{{
	augroup general " {{{
		autocmd!
		" Custom psql highlighting {{{
			au BufNewFile,BufRead *.sql set ft=psql foldmethod=marker
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
			au FileType help wincmd L
			au FileType help vertical resize 80
			au FileType help setl nonumber winfixwidth
			au FileType help let b:stl = "#[Branch] HELP#[BranchS] ó #[FileName]%<%t #[FileNameS]õ%* %=#[LinePercentS]ô#[LinePercent] %p%% " " Set custom statusline
			au FileType help nnoremap <buffer><space> <c-]> " Space selects subject
			au FileType help nnoremap <buffer><bs> <c-T> " Backspace to go back
		" }}}
		" Override SASS defaults {{{
			au FileType sass set sw=6 ts=6 noet
		" }}}
		" Disable whitespace trimming on patch files {{{
			au FileType diff au! whitespace
		" }}}
		" Enable Syntastic for selected filetypes {{{
			au BufNewFile,BufRead php,html,javascript,python,ruby,sh SyntasticEnable
		" }}}
	augroup END " }}}
	augroup formatting " {{{
		autocmd!
		" Fix gitcommit formatting {{{
			au FileType gitcommit setl formatoptions+=t formatoptions-=l textwidth=72 colorcolumn=72
		" }}}
		" Format plain text and e-mails correctly {{{
			au BufNewFile,BufRead *.txt setl ft=text
			au FileType mail,text setl formatoptions+=t formatoptions-=l textwidth=72 colorcolumn=72
		" }}}
	augroup END" }}}
	augroup list " {{{
		autocmd!
		" Set list on selected filetypes {{{
			au filetype vim setl list
			au filetype html,css,javascript,php,python,ruby setl list
		" }}}
	augroup END " }}}
	augroup whitespace " {{{
		autocmd!
		" Remove trailing whitespace on write {{{
			au BufWritePre * :call setline(1, map(getline(1, "$"), 'substitute(v:val, "\\s\\+$", "","")'))
		" }}}
	augroup END " }}}
	augroup mail " {{{
		autocmd!
		" Fix mutt mail formatting {{{
			fun! MuttMail()
				set tw=72 wrap fo+=tcqan1
				au! whitespace

				if getline(1) == ""
					" Write new mail
					norm O
					startinsert
				else
					" Reply to mail
					silent %!~/sync/bin/mail-filter reply
					norm jgqapO
					norm o
					startinsert
				endif
			endfun

			au BufRead /tmp/mutt-* call MuttMail()
		" }}}
	augroup END " }}}
" }}}
" Plugin settings {{{
	" EasyTags settings {{{
		let g:easytags_cmd = '/usr/bin/ctags'
		let g:easytags_resolve_links = 1
		let g:easytags_on_cursorhold = 0
		let g:easytags_always_enabled = 1
	" }}}
	" PHP highlighting settings {{{
		let g:php_folding = 0
		let g:php_html_in_strings = 1
		let g:php_parent_error_close = 1
		let g:php_parent_error_open = 1
		let g:php_no_shorttags = 1
	" }}}
	" Python highlighting settings {{{
		let g:python_highlight_all = 1
		let g:python_show_sync = 1
		let g:python_print_as_function = 1
	" }}}
	" delimitMate settings {{{
		let g:delimitMate_unbalanced_parens = 1
		let g:delimitMate_balance_matchpairs = 1

		au FileType mail,text,vim let b:delimitMate_autoclose = 0
	" }}}
	" Disable annoying sql bindings {{{
		let g:ftplugin_sql_omni_key_right = "<C-S-M-Right>"
		let g:ftplugin_sql_omni_key_left = "<C-S-M-Left>"
	" }}}
	" NERD tree settings {{{
		let g:NERDTreeChristmasTree = 1
		let g:NERDTreeCaseSensitiveSort = 1
		let g:NERDTreeQuitOnOpen = 1
		let g:NERDTreeWinPos = 'right'
		let g:NERDTreeWinSize = 50
		let g:NERDTreeShowBookmarks = 1
	" }}}
	" Taglist settings {{{
		let g:Tlist_WinWidth = 50
		let g:Tlist_Close_On_Select = 1
		let g:Tlist_Compact_Format = 1
		let g:Tlist_Display_Tag_Scope = 1
		let g:Tlist_Exit_OnlyWindow = 1
		let g:Tlist_File_Fold_Auto_Close = 1
		let g:Tlist_GainFocus_On_ToggleOpen = 1
		let g:Tlist_Use_Right_Window = 1
		let g:Tlist_Process_File_Always = 1

		au FileType taglist if bufname(bufnr("$")) == "__Tag_list__" | let b:stl = "#[FileName]%< Tag list #[FileNameS]õ%* %=" | endif " Set custom statusline
	" }}}
	" Syntastic settings {{{
		let g:syntastic_enable_signs = 1
		let g:syntastic_auto_loc_list = 1
	" }}}
	" Gundo settings {{{
		let g:gundo_right = 1
		let g:gundo_width = 50

		au FileType gundo if bufname(bufnr("$")) == "__Gundo__" | let b:stl = "#[Branch] GUNDO#[BranchS] ó #[FileName]%<Undo tree #[FileNameS]õ%* %=" | endif " Set custom statusline
		au FileType diff if bufname(bufnr("$")) == "__Gundo_Preview__" | let b:stl = "#[Branch] GUNDO#[BranchS] ó #[FileName]%<Diff preview #[FileNameS]õ%* %=" | endif " Set custom statusline
	" }}}
	" current-func-info settings {{{
		let g:cfi_php_show_params = 1
	" }}}
" }}}
