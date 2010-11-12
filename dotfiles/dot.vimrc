" Basic configuration {{{
	syntax on
	filetype plugin indent on

	" Define , as map leader
	let mapleader=','

	" Disable all bells
	set vb t_vb=

	set nocompatible
	set mouse=ar
	set tags=tags;.vimtags;
	" General options {{{
		set nobackup
		set swapfile
		set updatecount=200
		set directory=~/.vim/tmp
		set autochdir
		set backspace=indent,eol,start
		set fileformats=unix,dos
		set fileformat=unix
		set matchpairs+=<:>
		set shortmess=atToOI
		set iskeyword+=_,$,@,%,#
		set whichwrap=b,s,[,]
		set hidden
	" }}}
	" Wild menu {{{
		set wildmenu
		set wildignore=*.bak,*~,*.jpg,*.gif,*.png
		set wildmode=list:longest
	" }}}
" }}}
" UI options {{{
	set laststatus=2
	set fillchars=vert:║,fold:Û
	set hlsearch
	set incsearch
	set number
	set numberwidth=4
	set ruler
	set scrolloff=10
	set sidescrolloff=20
	set showcmd
	set showbreak=Ì
	set virtualedit=all
	set textwidth=0
	set confirm
	set updatetime=1500
	set history=100
	set list listchars=tab:Ë\ ,trail:í,eol:î

	colo lokaltog
	" Change cursor color in insert mode {{{
		if &term =~ "xterm\\|rxvt"
			silent !echo -ne "]12;\#dd4010\x7"
			let &t_SI="]12;\#89b6e2\x7"
			let &t_EI="]12;\#dd4010\x7"
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
			return substitute(fugitive#statusline(), 'GIT(\([a-z0-9\-_\.]\+\))', ' Í \1 │', 'gi')
		endfunction
	" }}}
	set statusline=
	set statusline+=%(%1*%{GitBranch()}%*%) " Git branch (User1)
	set statusline+=%< " Separator, truncate
	set statusline+=\ %f " File (relative path)
	set statusline+=%6*%(\ @\ %{Tlist_Get_Tagname_By_Line()}%)%*
	set statusline+=%2*%(\ %{SyntasticStatuslineFlag()}%)%*
	set statusline+=%2*%(\ %M%)%* " Modified (+, -) (User2)
	set statusline+=%3*%(\ [%R%H%W]%)%* " RO,HLP,PRV (User3)
	set statusline+=\ %= " Separator, left/right
	set statusline+=%6*%(\ %{&fileformat}%)%* " File format
	set statusline+=%6*%(\ %{(&fenc==''?&enc:&fenc)}%)%* " File encoding
	set statusline+=\ %(%l:%c%V%)
	set statusline+=\ %1*│%* " Group end
	set statusline+=%4*%(\ %{strlen(&ft)?&ft:''}\ %1*│%)%* " File type (User4)
	set statusline+=\ %5*%P\ %* " Percentage (User5) (always 3 in length)
"	set statusline+=\ %{SyntaxItem()}
" }}}
" Layout / Text formatting {{{
	set autoindent
	set wrap
	set formatoptions=tcroqn1
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
		let t=getline(v:foldstart)
		let w=strlen(matchstr(t, '^\s*'))
		let t=substitute(t, '^\W*', '', '')
		let t=substitute(t, '\W*$', '', '')
		let n=v:foldend - v:foldstart + 1

		return repeat(repeat(' ', &tabstop), w).t.' Î'.n.' ÝÏ '
	endfunction

	au FileType vim set foldlevel=0 foldtext=VimFold()
" }}}
" Mappings {{{
	nmap <silent><F7> :LustyJuggler<CR>
	nmap <silent><F8> :TlistToggle<CR>
	nmap <silent><F9> :LustyBufferExplorer<CR>
	nmap <silent><F10> :NERDTreeToggle<CR>

	" Buffer swithing
	nnoremap <silent> <Tab> :bnext<CR>
	nnoremap <silent> <S-Tab> :bprevious<CR>

	" Folding/unfolding
	nnoremap <S-Left> zc
	nnoremap <Leader>h zc
	inoremap <S-Left> <C-O>zc
	nnoremap <S-Right> zo
	nnoremap <Leader>l zo
	inoremap <S-Right> <C-O>zo

	" Quick edit .vimrc
	nmap <Leader>v <Esc>:e $MYVIMRC<CR>

	" Navigate by visual lines
	noremap k gk
	noremap j gj

	" Tab indenting in visual mode
	vmap <Tab> >gv
	vmap <S-Tab> <gv

	" Clear search highlighting
	nmap <silent> <Leader>n :silent noh<cr>

	" Sudo write (:W)
	command! -bar -nargs=0 W :silent exec "write !sudo tee % >/dev/null" | silent edit!

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
			let l_num=line(a:line_getter)
			if l_num - v:count1 - 1 < 0
				let move_arg="0"
			else
				let move_arg=a:line_getter." -".(v:count1 + 1)
			endif
			call MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
		endfunction

		function! MoveLineOrVisualDown(line_getter, range)
			let l_num=line(a:line_getter)
			if l_num + v:count1 > line("$")
				let move_arg="$"
			else
				let move_arg=a:line_getter." +".v:count1
			endif
			call MoveLineOrVisualUpOrDown(a:range."move ".move_arg)
		endfunction

		function! MoveLineOrVisualUpOrDown(move_arg)
			let col_num=virtcol(".")
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

		" Nginx highlighting
		au BufNewFile,BufRead /etc/nginx/conf/* set ft=nginx

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
		au FileType help setl nonumber statusline=%f%<
		au FileType help nnoremap <buffer><space> <c-]> " Space selects subject
		au FileType help nnoremap <buffer><bs> <c-T> " Backspace to go back
		au FileType help wincmd L
		au FileType help vertical resize 80

		" Override SASS defaults
		au FileType sass set sw=6 ts=6 noet

		" Disable whitespace trimming on patch files
		au FileType diff au! whitespace

		" Enable Syntastic {{{
			au BufNewFile,BufRead * SyntasticEnable
		" }}}
		" Highlight vim modeline in all files {{{
			au Syntax *
				\ syn match VimModelineLine /^.\{-1,}vim:[^:]\{-1,}:.*/ contains=VimModeline |
				\ syn match VimModeline contained /vim:[^:]\{-1,}:/

			hi def link VimModelineLine comment
			hi def link VimModeline     special
		" }}}
	augroup END
	augroup whitespace
		autocmd!

		" Remove trailing whitespace on write
		au BufWritePre * :call setline(1, map(getline(1, "$"), 'substitute(v:val, "\\s\\+$", "","")'))
	augroup END
	augroup mail
		autocmd!

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
	augroup END
" }}}
" Plugin settings {{{
	" Disable matchparens (slow and useless) {{{
		let loaded_matchparen=1
	" }}}
	" EasyTags settings {{{
		let g:easytags_cmd='/usr/bin/ctags'
		let g:easytags_resolve_links=1
		let g:easytags_on_cursorhold=0
		let g:easytags_always_enabled=1
	" }}}
	" Supertab settings {{{
		let g:SuperTabLongestEnhanced=1
		let g:SuperTabContextDefaultCompletionType='<C-x><C-u>'
	" }}}
	" PHP highlighting settings {{{
		let g:php_folding=0
		let g:php_html_in_strings=1
		let g:php_parent_error_close=1
		let g:php_parent_error_open=1
		let g:php_no_shorttags=1
	" }}}
	" Python highlighting settings {{{
		let g:python_highlight_all=1
		let g:python_show_sync=1
		let g:python_print_as_function=1
	" }}}
	" delimitMate settings {{{
		let g:delimitMate_unbalanced_parens=1
		let g:delimitMate_balance_matchpairs=1

		au FileType mail,text let b:delimitMate_autoclose=0
	" }}}
	" Disable annoying sql bindings {{{
		let g:ftplugin_sql_omni_key_right="<C-S-M-Right>"
		let g:ftplugin_sql_omni_key_left="<C-S-M-Left>"
	" }}}
	" NERD tree settings {{{
		let g:NERDTreeChristmasTree=1
		let g:NERDTreeCaseSensitiveSort=1
		let g:NERDTreeQuitOnOpen=1
		let g:NERDTreeWinPos='right'
		let g:NERDTreeWinSize=50
		let g:NERDTreeShowBookmarks=1
	" }}}
	" Taglist settings {{{
		let g:Tlist_Close_On_Select=1
		let g:Tlist_Compact_Format=1
		let g:Tlist_Display_Tag_Scope=1
		let g:Tlist_Exit_OnlyWindow=1
		let g:Tlist_File_Fold_Auto_Close=1
		let g:Tlist_GainFocus_On_ToggleOpen=1
		let g:Tlist_Use_Right_Window=1
		let g:Tlist_Process_File_Always=1
	" }}}
	" Syntastic settings {{{
		let g:syntastic_enable_signs=1
		let g:syntastic_auto_loc_list=1
	" }}}
" }}}
