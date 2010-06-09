" Basic configuration {{{
	set nocompatible
	syntax on
	set mouse=ar

	set tags+=tags;$HOME

	" General options {{{
		filetype plugin indent on
		set nobackup
		set swapfile
		set uc=200
		set directory=~/.vim/tmp

		set autochdir
		set backspace=2
		set fileformats=unix,dos
		set fileformat=unix

		let mapleader=','
		set whichwrap=b,s
		set matchpairs+=<:>
		set shortmess=atToOI
		set iskeyword+=_,$,@,%,#
		set whichwrap=b,s,[,]
		set clipboard+=unnamed
		set hidden
	"	set margincolumn=100
	" }}}
	" Wild menu {{{
		set wildmenu
		set wildignore=*.bak,*~,*.jpg,*.gif,*.png
		set wildmode=list:longest
	" }}}
" }}}
" UI options {{{
	set laststatus=2
	set fillchars=vert:ÿ,fold:Û
	set hlsearch
	set incsearch
	set lazyredraw
	set linespace=0
	set number
	set numberwidth=5
	set ruler
	set scrolloff=999
	set sidescrolloff=20
	set showcmd
	set showbreak=Ì
	set virtualedit=all
	set textwidth=0
	set confirm
	set updatetime=500
	set list listchars=tab:Ë\ ,trail:í,eol:î

	colo unrealized
	" Change cursor color in insert mode {{{
		if &term =~ "xterm\\|rxvt"
			silent !echo -ne "]12;\#dd4010\x7"
			let &t_SI = "]12;\#89b6e2\x7"
			let &t_EI = "]12;\#dd4010\x7"
			au VimLeave * silent !echo -ne "]12;\#dd4010\x7"
		endif
	" }}}
" }}}
" Status line {{{
	" Get syntax item {{{
		function! SyntaxItem()
			return synIDattr(synID(line("."),col("."),1),"name")
		endfunction
	" }}}

	" Git branch wrapper {{{
		function! GitBranch()
			if !exists("b:branch")
				let b:branch = fugitive#statusline()

				if empty(b:branch)
					return ''
				endif

				let b:branch = substitute(b:branch, 'GIT(\(\w\+\))', ' Í \1', 'g')
			endif

			return b:branch
		endfunction
	" }}}

	set statusline=
    set statusline+=%1*%{GitBranch()}%* " Git branch (User1)
	set statusline+=\ %< " Separator, truncate
	set statusline+=Î " Group start
	set statusline+=\ %f " File (relative path)
	set statusline+=%(\ %2*%M%*%) " Modified (+, -) (User2)
	set statusline+=%(\ %3*[%R%H%W]%*%) " RO,HLP,PRV (User3)
	set statusline+=\ %= " Separator, left/right
	set statusline+=\ %6*%{&fileformat}%* " File format
	set statusline+=\ %6*%{(&fenc==\"\"?&enc:&fenc)}%* " File encoding
	set statusline+=\ %(%l:%c%V%)
	set statusline+=\ Ï " Group end
	set statusline+=\ %4*%{strlen(&ft)?toupper(&ft):'NONE'}%* " File type (User4)
	set statusline+=\ %5*%P\ %* " Percentage (User5) (always 3 in length)
    "set statusline+=%{SyntaxItem()}
" }}}
" Layout / Text formatting {{{
	set autoindent
	set formatoptions=tcroqn1
	set ignorecase
	set linebreak
	set shiftwidth=4
	set tabstop=4
	set smartcase
	set smartindent
	set smarttab
	set startofline
" }}}
" Folding {{{
	set foldenable
	set foldmethod=marker
	set foldlevel=1
	set foldopen-=search
	set foldcolumn=2

	function! VimFold()
		let t = getline(v:foldstart)
		let t = substitute(t, '^\W*', '', '')
		let t = substitute(t, '\W*$', '', '')
		let n = v:foldend - v:foldstart + 1

		return t.' Î'.n.' ÝÏ '
	endfunction
	au FileType vim set foldlevel=0 foldtext=VimFold()
" }}}
" Mappings - Dvorak based {{{
	nmap <Leader>b <Esc>:Bp<CR>
	nmap <F7> v
	nmap <F8> s
	nmap <F9> :w<CR>
	nmap <silent><F10> :NERDTreeToggle<CR>

	" q: sucks
	nmap q: :q

	" Override PageUp/PageDown
	nmap <PageUp> <C-U>
	nmap <PageDown> <C-D>
	imap <PageUp> <C-O><C-U>
	imap <PageDown> <C-O><C-D>

	" Folding/unfolding
	nnoremap <S-Left> zc
	nnoremap <Leader>h zc
	inoremap <S-Left> <C-O>zc
	nnoremap <S-Right> zo
	nnoremap <Leader>l zo
	inoremap <S-Right> <C-O>zo

	" Fix Shift-up/down in case shift is held while browsing folds
	nmap <S-Up> <Up>
	imap <S-Up> <Up>
	nmap <S-Down> <Down>
	imap <S-Down> <Down>

	" Quick edit .vimrc
	nmap ,v <Esc>:tabnew $MYVIMRC<CR>

	" Navigate by visual lines
	noremap k gk
	noremap j gj

	" Tab indenting in visual mode
	vmap <Tab> >gv
	vmap <S-Tab> <gv

	" Tab controls
	nmap <Leader><Tab> :tabnew<CR>
	nmap <Leader>o :tabnext<CR>
	nmap <Leader>a :tabprevious<CR>
	nmap <Leader>; :tabclose!<CR>

	" ,n clears highlighting
	nmap <silent> <Leader>n :silent noh<cr>

	" Sudo write
	command! -bar -nargs=0 SudoW :silent exe "write !sudo tee % >/dev/null"|silent edit!

	" Home toggles start of line/start of text {{{
		imap <khome> <home>
		nmap <khome> <home>
		inoremap <silent> <home> <C-O>:call Home()<CR>
		nnoremap <silent> <home> :call Home()<CR>
		function! Home()
			let curcol=wincol()
			normal ^
			let newcol=wincol()
			if newcol==curcol
				normal 0
			endif
		endfunction
	" }}}
	" Line moving {{{
		function! MoveLineUp()
			call MoveLineOrVisualUp(".", "")
		endfunction

		function! MoveLineDown()
			call MoveLineOrVisualDown(".", "")
		endfunction

		function! MoveVisualUp()
			call MoveLineOrVisualUp("'<", "'<,'>")
			normal gv
		endfunction

		function! MoveVisualDown()
			call MoveLineOrVisualDown("'>", "'<,'>")
			normal gv
		endfunction

		function! MoveLineOrVisualUp(line_getter, range)
			let l_num = line(a:line_getter)
			if l_num - v:count1 - 1 < 0
				let move_arg = "0"
			else
				let move_arg = a:line_getter." -".(v:count1 + 1)
			endif
			call MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
		endfunction

		function! MoveLineOrVisualDown(line_getter, range)
			let l_num = line(a:line_getter)
			if l_num + v:count1 > line("$")
				let move_arg = "$"
			else
				let move_arg = a:line_getter." +".v:count1
			endif
			call MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
		endfunction

		function! MoveLineOrVisualUpOrDown(move_arg)
			let col_num = virtcol(".")
			execute "silent! ".a:move_arg
			execute "normal! ".col_num."|"
		endfunction

		nnoremap <silent> <Leader><Up> :<C-u>call MoveLineUp()<CR>
		nnoremap <silent> <Leader><Down> :<C-u>call MoveLineDown()<CR>
		vnoremap <silent> <Leader><Up> :<C-u>call MoveVisualUp()<CR>
		vnoremap <silent> <Leader><Down> :<C-u>call MoveVisualDown()<CR>
	" }}}
" }}}
" Autocommands {{{
	augroup general
		autocmd!

		" Custom psql highlighting
		au BufNewFile,BufRead *.sql set ft=psql foldmethod=marker

		" Remove trailing whitespace on write
		au BufWritePre * :call setline(1, map(getline(1, "$"), 'substitute(v:val, "\\s\\+$", "","")'))

		" Always do a full syntax refresh
		au BufEnter * syntax sync fromstart

		" Source .vimrc on write
		au BufWritePost .vimrc source %

		" Make .sh/.py files executable on write
		au BufWritePost *.sh silent !chmod a+x %
		au BufWritePost *.py silent !chmod a+x %

		" Script templates
		au BufNewFile *.sh  so ~/.vim/templates/tpl.sh
		au BufNewFile *.py  so ~/.vim/templates/tpl.py
		au BufNewFile *.php so ~/.vim/templates/tpl.php

		" Help file settings
		au FileType help set nonumber " No line numbers when viewing help
		au FileType help nnoremap <buffer><cr> <c-]> " Enter selects subject
		au FileType help nnoremap <buffer><bs> <c-T> " Backspace to go back
		au FileType help wincmd L
		au FileType help vertical resize 80

		" Override SASS defaults
		au FileType sass set sw=4 ts=4 noet

		" Set margincolumn for git commit files
		au FileType gitcommit set margincolumn=50 | hi MarginColumn ctermbg=52

		" Highlight vim modeline in all files {{{
			au Syntax *
				\ syn match VimModelineLine /^.\{-1,}vim:[^:]\{-1,}:.*/ contains=VimModeline |
				\ syn match VimModeline contained /vim:[^:]\{-1,}:/
			hi def link VimModelineLine comment
			hi def link VimModeline     special
		" }}}
	augroup END
" }}}
" Plugin settings {{{
	" Disable matchparens (slow and useless)
	let loaded_matchparen = 1

	" PHP highlighting settings {{{
		let g:php_smart_members = 1
		let g:php_alt_properties = 1
		let g:php_show_semicolon = 1
		let g:php_smart_semicolon = 1
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
		let g:delimitMate_excluded_ft = "mail,txt,vim"
		let g:delimitMate_matchpairs = "(:),[:],{:},\":\",':',`:`"
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
		let g:NERDTreeWinSize = 40
		let g:NERDTreeShowBookmarks = 1
	" }}}
" }}}
