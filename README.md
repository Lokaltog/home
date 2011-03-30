# Lokaltog's sync repo

- Author: Kim Silkebækken (kim.silkebaekken+github@gmail.com)
- Source: <https://github.com/Lokaltog/sync>
- Version: **1.0.4**
- Updated: **2011-03-29**.

## Vim statusbar

Quite a few people have contacted me about the vim statusbar. The
statusbar is a custom statusbar that requires the `lokaltog-12` fonts to
work correctly. It also requires the following vim plugins:

- [Fugitive](https://github.com/tpope/vim-fugitive)
- [Syntastic](https://github.com/scrooloose/syntastic)
- [current-func-info](http://www.vim.org/scripts/script.php?script_id=3197)

I'm planning to make the statusbar more portable in the future. In the
meantime, feel free to play around with the code, and please let me know
if you have any comments on how I can make the statusbar more portable.

## Other important notes

Most of the stuff in this repo depends on the fonts in `fonts/bdf` to
look right. DWM requires `cureextra` for statusline icons while vim and
zsh require `lokaltog-12` for icons and various symbols. Everything will
work without these fonts, but it will look weird because the icons and
symbols replace some Unicode glyphs.

Some binaries are targeted toward Arch Linux users (they use `pacman` to
install missing packages).

If you just want to check out how everything looks without messing with
your own home folder, just create a test user account and install
everything from that account.

## Installation guide

### Step 1: Clone the repo and fetch submodules

	git clone git://github.com/Lokaltog/sync.git
	cd sync
	git submodule init
	git submodule update

### Step 2: Create symlinks to the dotfiles

There's a shell script in `sync/dotfiles` which will automatically
create symlinks for all dotfiles in this repo.

	/bin/sh ~/sync/dotfiles/setup.sh ~

### Step 3 (optional, Arch Linux only): Compile and install dwm

	cd ~/sync/pkg/dwm-git
	makepkg -si

The `.xinitrc` in this repo executes `dwm`. Restart X, dwm should start
without any problems.
