" Paste code
nnoremap sp p>`].
nnoremap sP P>`].

if exists('g:loaded_fakeclip')
  nmap +sp <Plug>(fakeclip-p)>`].
  nmap +sP <Plug>(fakeclip-p)>`].

  nmap &sp <Plug>(fakeclip-screen-p)>`].
  nmap &sP <Plug>(fakeclip-screen-p)>`].
endif
