====================
Lokaltog's sync repo
====================

:Author: Kim Silkebækken (kim.silkebaekken+github@gmail.com)
:Source: http://git.io/LS
:Version: 1.0.9
:Updated: 2011-12-27

==================
Installation guide
==================

1. Clone the repo and fetch all submodules.

   ::

        git clone git://github.com/Lokaltog/sync.git
        cd sync
        git submodule update --init

2. Create symlinks to the dotfiles.

   There's a shell script in ``dotfiles`` that will automatically create 
   all necessary dotfile symlinks.

   ::

        ~/sync/dotfiles/setup.sh
