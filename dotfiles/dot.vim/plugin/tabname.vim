" File: tab_name.vim
" Author: Nickolay Golubev
" Email: golubev.nikolay@gmail.com
" Description: script allow set names for tab pages ( "Call stack", "Help tab", "Broswser" for example ). Useful for console tab mode
" Usage:
"   :TName 'tabname' - set name for current tab page
"   :TNoName - remove name (page name = buffer name)

if exists('tab_name_plugin')
    finish
endif

let tab_name_plugin = 1

function! s:SetTabName(name)
    let t:tab_name = a:name

    for win_number in range(1, winnr('$'))
        call setwinvar(win_number, "tab_win_name", a:name)
    endfor

    call s:RefreshTab()
endfunction

function! s:RemoveTabName()
    for win_number in range(1, winnr('$'))
        call setwinvar(win_number, "tab_win_name", '')
    endfor
    unlet t:tab_name

    call s:RefreshTab()
endfunction

function! s:RefreshTab()
    set tabline=%!TabCaptionLineFunction()
	set guitablabel=%{TabGuiCaptionLabel()}
endfunction

function! TabCaptionLabel(number)
    let caption = ' '
    let tab_name = gettabwinvar(a:number, 1, 'tab_win_name') 

    let buflist = tabpagebuflist(a:number)
    let winnr = tabpagewinnr(a:number)
	let buf_name = bufname(buflist[winnr - 1])

    if tab_name == ''
        let caption .= pathshorten(buf_name)
    else
        let caption .= tab_name
    endif
    return caption.' '
endfunction


function! TabCaptionLineFunction()
    let line = ''
    for i in range(tabpagenr('$'))

        let modified_part = ''
        let bufnrlist = tabpagebuflist(i+1)
        for bufnr in bufnrlist
            if getbufvar(bufnr, "&modified")
                let modified_part = '+'
                break
            endif
        endfor

        "let caption = '['.(i+1).modified_part.']'
        let caption = ''.(i+1).modified_part.''
        let line .= '%#TabLineNumber#'.caption
        " select the highlighting
        if i + 1 == tabpagenr()
            let line .= '%#TabLineSel#'
        else
            if i % 2 == 0
                let line .= '%#TabLine#'
            else
                let line .= '%#TabLine#'
            endif
        endif

        let line .= '%' . (i + 1) . 'T'

        let line .= TabCaptionLabel(i + 1)
    endfor

    let line .= '%#TabLineFill#%T'

    if tabpagenr('$') > 1
        let line .= '%=%#TabLine#%999X%#TabLineClose#[X]'
    endif

    return line
endfunction

function! TabGuiCaptionLabel()
    "let caption = '['
    let tab_number = v:lnum
    let bufnrlist = tabpagebuflist(tab_number)
    let tab_name = gettabwinvar(tab_number, 1, 'tab_win_name') 

    let caption .= tab_number

    for bufnr in bufnrlist
        if getbufvar(bufnr, "&modified")
            let caption .= '+'
            break
        endif
    endfor

    "let caption .= '] '

    let winnr = tabpagewinnr(tab_number)
	let buf_name = bufname(bufnrlist[winnr - 1])
    if tab_name == ''
        let caption .= pathshorten(buf_name)
    else
        let caption .= tab_name
    endif

    return caption
endfunction


function! s:TabWinEnter()
    if exists('t:tab_name')
        call setwinvar(winnr(), "tab_win_name", t:tab_name)
    endif
endfunction
    
augroup TabLabelNameAU
    au!
    au WinEnter * call s:TabWinEnter()
augroup END

call s:RefreshTab()

command! -nargs=1 TName call s:SetTabName(<args>)
command! TNoName call s:RemoveTabName()


