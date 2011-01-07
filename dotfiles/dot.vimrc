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
	set title
	" Instantly leave insert mode when pressing <Esc> {{{
		" This works by disabling the mapping timeout completely in normal mode,
		" and enabling it in insert mode with a very low timeout length.
		augroup fastescape
			autocmd!

			set notimeout
			set ttimeout
			set timeoutlen=10

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
				set fillchars=vert:┆,fold:Ĕ,diff:Đ
				set listchars=tab:Ā\ ,trail:Ć,eol:Ą
				set showbreak=Ģ
			" }}}
		endif
	" }}}
	set laststatus=2
	set hlsearch
	set incsearch
	set nonumber
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

			" Prepare statusline arrows
			" Syntax: [>] [>>] [<] [<<]
			let new_stl = substitute(new_stl, '\[>\]',  'č', 'g')
			let new_stl = substitute(new_stl, '\[>>\]', 'ď', 'g')
			let new_stl = substitute(new_stl, '\[<\]',  'Č', 'g')
			let new_stl = substitute(new_stl, '\[<<\]', 'Ď', 'g')

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
		let g:default_stl .= "#CUR##[Mode] %{substitute(mode(), '', '^V', 'g')} #[ModeS][>>]#/CUR#"
		let g:default_stl .= "#[Branch] %(%{substitute(fugitive#statusline(), 'GIT(\\([a-z0-9\\-_\\.]\\+\\))', 'ģ \\1', 'gi')}#[BranchS] [>] %)" " Git branch
		let g:default_stl .= "#[ModFlag]%{&readonly ? 'Ġ ' : ''}" " RO flag
		let g:default_stl .= "#[FileName]%t " " File name
		let g:default_stl .= "#CUR##[Error]%(%{substitute(SyntasticStatuslineFlag(), '\\[syntax:\\(\\d\\+\\)\\((\\(\\d\\+\\))\\)\\?\\]', '[>][>][>] SYNTAX Ĥ \\1\\2 [>][>][>]', 'i')} %)#/CUR#" " Syntastic error flag
		let g:default_stl .= "#[ModFlag]%(%M %)" " Modified flag
		let g:default_stl .= "#[BufFlag]%(%H%W %)" " HLP,PRV flags
		let g:default_stl .= "#[FileNameS][>>]" " Separator
		let g:default_stl .= "#[FunctionName] " " Padding/HL group
		let g:default_stl .= "%<" " Truncate right
		let g:default_stl .= "#CUR#%(%{cfi#format('%s', '')} %)#/CUR#" " Function name
		let g:default_stl .= "%= " " Right align
		let g:default_stl .= "#CUR##[FileFormat]%{&fileformat} #/CUR#" " File format
		let g:default_stl .= "#CUR##[FileEncoding]%{(&fenc == '' ? &enc : &fenc)} #/CUR#" " File encoding
		let g:default_stl .= "#CUR##[Separator][<] Ĩĩ #[FileType]%{strlen(&ft) ? &ft : 'n/a'} #/CUR#" " File type
		let g:default_stl .= "#[LinePercentS][<<]#[LinePercent] %p%% " " Line/column/virtual column, Line percentage
		let g:default_stl .= "#[LineNumberS][<<]#[LineNumber] ĥ %l#[LineColumn]:%c%V " " Line/column/virtual column, Line percentage
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
				\ , 'FileName'     : [[ 240, 231, 'bold'], [ 234, 244, 'none']]
				\ , 'FileNameS'    : [[ 240, 236, 'bold'], [ 234, 232, 'none']]
				\ , 'Error'        : [[ 240, 202, 'bold'], [ 234, 239, 'none']]
				\ , 'ModFlag'      : [[ 240, 196, 'bold'], [ 234, 239, 'none']]
				\ , 'BufFlag'      : [[ 240, 250, 'none'], [ 234, 239, 'none']]
				\ , 'FunctionName' : [[ 236, 247, 'none'], [ 232, 239, 'none']]
				\ , 'FileFormat'   : [[ 236, 244, 'none'], [ 232, 239, 'none']]
				\ , 'FileEncoding' : [[ 236, 244, 'none'], [ 232, 239, 'none']]
				\ , 'Separator'    : [[ 236, 244, 'none'], [ 232, 239, 'none']]
				\ , 'FileType'     : [[ 236, 248, 'none'], [ 232, 239, 'none']]
				\ , 'LinePercentS' : [[ 240, 236, 'none'], [ 234, 232, 'none']]
				\ , 'LinePercent'  : [[ 240, 250, 'none'], [ 234, 239, 'none']]
				\ , 'LineNumberS'  : [[ 252, 240, 'bold'], [ 234, 234, 'none']]
				\ , 'LineNumber'   : [[ 252, 236, 'bold'], [ 234, 244, 'none']]
				\ , 'LineColumn'   : [[ 252, 240, 'none'], [ 234, 239, 'none']]
			\ }
			\ , 'Insert': {
				\   'Mode'         : [[ 153,  23, 'bold'], [                 ]]
				\ , 'ModeS'        : [[ 153,  31, 'bold'], [                 ]]
				\ , 'Branch'       : [[  31, 117, 'none'], [                 ]]
				\ , 'BranchS'      : [[  31, 117, 'none'], [                 ]]
				\ , 'FileName'     : [[  31, 231, 'bold'], [                 ]]
				\ , 'FileNameS'    : [[  31,  24, 'bold'], [                 ]]
				\ , 'Error'        : [[  31, 202, 'bold'], [                 ]]
				\ , 'ModFlag'      : [[  31, 196, 'bold'], [                 ]]
				\ , 'BufFlag'      : [[  31,  75, 'none'], [                 ]]
				\ , 'FunctionName' : [[  24, 117, 'none'], [                 ]]
				\ , 'FileFormat'   : [[  24,  75, 'none'], [                 ]]
				\ , 'FileEncoding' : [[  24,  75, 'none'], [                 ]]
				\ , 'Separator'    : [[  24,  37, 'none'], [                 ]]
				\ , 'FileType'     : [[  24,  81, 'none'], [                 ]]
				\ , 'LinePercentS' : [[  31,  24, 'none'], [                 ]]
				\ , 'LinePercent'  : [[  31, 117, 'none'], [                 ]]
				\ , 'LineNumberS'  : [[ 117,  31, 'bold'], [                 ]]
				\ , 'LineNumber'   : [[ 117,  23, 'bold'], [                 ]]
				\ , 'LineColumn'   : [[ 117,  31, 'none'], [                 ]]
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
	" Highlight problematic whitespace (spaces before tabs) {{{
		hi RedundantSpaces ctermfg=214 ctermbg=160 cterm=bold
		match RedundantSpaces / \+\ze\t/
	" }}}
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

		return repeat(repeat(' ', &tabstop), w).t.' Ĉ '.n.' Ĥ ĉ'
	endfunction

	au FileType vim set foldlevel=0 foldtext=VimFold()
" }}}
" Mappings {{{
	" F key mappings {{{
		nnoremap <silent><F6> :set number!<CR>
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
			function! s:SetupHelpWindow()
				wincmd L
				vertical resize 80
				setl nonumber winfixwidth colorcolumn=
				let b:stl = "#[Branch] HELP#[BranchS] [>] #[FileName]%<%t #[FileNameS][>>]%* %=#[LinePercentS][<<]#[LinePercent] %p%% " " Set custom statusline
				nnoremap <buffer><space> <c-]> " Space selects subject
				nnoremap <buffer><bs> <c-T> " Backspace to go back
			endfunction

			au FileType help au BufEnter,BufWinEnter <buffer> call <SID>SetupHelpWindow()
		" }}}
		" Override SASS defaults {{{
			au FileType sass set sw=6 ts=6 noet
		" }}}
		" Disable Syntastic for selected filetypes {{{
			au FileType sass SyntasticDisable
		" }}}
		" Fix space highlighting in diff files {{{
			au FileType diff hi clear RedundantSpaces
				\ | hi DiffCol ctermbg=238 cterm=bold
				\ | match DiffCol /^[ +-]\([+-]\)\@!/
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
			au FileType vim setl list
			au FileType diff setl list listchars+=trail:\ " Disable trailing space chars
			au FileType sh,zsh,bash setl list
			au FileType html,css,sass,javascript,php,python,ruby,psql setl list
		" }}}
	augroup END " }}}
	augroup whitespace " {{{
		autocmd!
		" Remove trailing whitespace from selected filetypes {{{
			function! s:StripTrailingWhitespace()
				normal mZ

				%s/\s\+$//e

				if line("'Z") != line(".")
					echom "Stripped whitespace"
				endif

				normal `Z
			endfunction

			au FileType html,css,sass,javascript,php,python,ruby,psql au BufWritePre <buffer> :call <SID>StripTrailingWhitespace()
		" }}}
	augroup END " }}}
	augroup statuslines " {{{
		autocmd!
		" Lusty buffer list {{{
			au BufEnter * if bufname("%") == "[LustyExplorer-Buffers]" | let b:stl = "#[Branch] LustyExplorer#[BranchS] [>] #[FileName]%<Buffer List #[FileNameS][>>]%* %=" | endif " Set custom statusline
		" }}}
		" Tag list {{{
			au BufEnter * if bufname("%") == "__Tag_list__"
				\ | let b:stl = "#[FileName]%< Tag list #[FileNameS][>>]%* %="
				\ | endif
		" }}}
		" Gundo {{{
			au BufEnter * if bufname("%") == "__Gundo__"
				\ | let b:stl = "#[Branch] GUNDO#[BranchS] [>] #[FileName]%<Undo tree #[FileNameS][>>]%* %="
				\ | endif

			au BufEnter * if bufname("%") == "__Gundo_Preview__"
				\ | let b:stl = "#[Branch] GUNDO#[BranchS] [>] #[FileName]%<Diff preview #[FileNameS][>>]%* %="
				\ | endif
		" }}}
		" Syntastic location list {{{
			au BufEnter * if bufname("%") == "[Location List]"
				\ | let b:stl = "#[FileName]%< Location List #[FileNameS][>>]%* %="
				\ | endif
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
	" }}}
	" Syntastic settings {{{
		let g:syntastic_enable_signs = 1
		let g:syntastic_auto_loc_list = 0
	" }}}
	" Gundo settings {{{
		let g:gundo_right = 1
		let g:gundo_width = 50
	" }}}
" }}}
