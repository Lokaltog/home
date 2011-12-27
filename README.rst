====================
Lokaltog's sync repo
====================

:Author: Kim Silkebækken (kim.silkebaekken+github@gmail.com)
:Source: http://git.io/LS
:Version: 1.0.9
:Updated: 2011-12-27

Vim statusbar
-------------

Please see the Powerline_ project.

.. _Powerline: http://git.io/Powerline

Custom fonts
------------

Some of the stuff in this repo requires the fonts in ``fonts/bdf`` to work 
properly. Dwm requires ``cureextra`` for statusline icons, while vim and zsh 
require ``lokaltog-12`` or ``lokaltog-10`` for icons and various symbols.  
These symbols are stored in the extended Latin-1 range, which may cause my 
configuration to appear weird unless you use compatible fonts.

==================
Installation guide
==================

1. Clone the repo and fetch all submodules.

   ::

        git clone git://github.com/Lokaltog/sync.git
        cd sync
        git submodule init
        git submodule update

2. Create symlinks to the dotfiles.

   There's a shell script in ``dotfiles`` which will automatically create 
   all the necessary dotfile symlinks.

   ::

        ~/sync/dotfiles/setup.sh

3. *Optional.* Compile and install dwm.

   ::

        cd ~/sync/pkg/dwm-git
        makepkg -si

   The ``.xinitrc`` and the files in ``.xsession.d`` will execute dwm on 
   X startup. Restart X and dwm should start without any problems.
