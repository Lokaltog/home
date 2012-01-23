" Author: Kim Silkebækken <kim.silkebaekken+vim@gmail.com>
" Source: http://git.io/LS

" Load Unbundle {{{
	runtime bundle/unbundle/unbundle.vim
" }}}
" Source viminit files {{{
	runtime! config/**/*.vim
" }}}
" Basic configuration {{{
	filetype plugin indent on
	syntax on

	set backspace=indent,eol,start
	set nobackup
	set clipboard=unnamed,unnamedplus,autoselect
	set colorcolumn+=76
	set nocompatible
	set completeopt=menu,menuone,longest
	set conceallevel=2
	set confirm
	set copyindent
	set diffopt+=context:3
	set encoding=utf-8
	set noerrorbells visualbell t_vb= " Disable all bells
	set formatoptions=roqwanl1
	set hidden
	set history=1000
	set hlsearch
	set ignorecase
	set incsearch
	set laststatus=2
	set lazyredraw
	set linebreak
	set nomodeline
	set nonumber
	set numberwidth=4
	set preserveindent
	set pumheight=10
	set report=0
	set ruler
	set scrolljump=10
	set scrolloff=10
	set shortmess=atToOI
	set showcmd
	set noshowmode
	set smartcase
	set nostartofline
	set noswapfile
	set switchbuf=useopen,usetab
	set termencoding=utf-8
	set textwidth=10000
	set undodir=~/.vim/tmp
	set undofile
	set undolevels=1000
	set updatetime=1500
	set virtualedit=block,onemore
	set nowrap
	set nowritebackup
	" Set tab width {{{
		let g:tabwidth = 4

		exec 'set shiftwidth='  . g:tabwidth
		exec 'set softtabstop=' . g:tabwidth
		exec 'set tabstop='     . g:tabwidth
	" }}}
	" Folding settings {{{
		set foldcolumn=0
		set foldenable
		set foldlevel=0
		set foldmethod=marker
		set foldtext=FoldText()
	" }}}
	" Wild menu {{{
		set wildignore=.svn,CVS,.git,.hg,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpe?g,*.png,*.xpm,*.gif
		set wildmenu
		set wildmode=full
	" }}}
" }}}
