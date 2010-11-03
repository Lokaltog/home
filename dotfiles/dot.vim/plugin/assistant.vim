" == "acomment" == {{{
"
"          File:  assistant.vim
"          Path:  ~/.vim/plugin
"        Author:  A-yu
"      Modifier:  A-yu
"      Modified:  2010-08-19 17:33:09  
"       License:  Public Domain
"   Description:  Show the prototype of function
"
" --}}}

" Exit if already loaded
if exists("g:loaded_assistant")
    finish
endif
let g:loaded_assistant = "Version 1.35"

" Exit if user doesn't need this plugin
if exists("g:DisableAcomplete") && g:DisableAcomplete != 0
    finish
endif

" ================================== Config ===========================================
" Defined user complete function, default is <C-x-u>
"
" Mapping for Eclipse user
" imap <silent> <unique> <A-/> <C-x><C-u>
" nmap <silent> <unique> <A-/> :call <SID>Help()<Cr>
"
" Mapping for Netbeans user
" imap <silent> <unique> <C-\> <C-x><C-u>
" nmap <silent> <unique> <C-\> :call <SID>Help()<Cr>

nmap <silent> <unique> <C-h> :call <SID>Help()<Cr>

" Defined complete file types
let s:aType = {
            \'php':'php', 'phps':'php', 'phtml':'php',
            \'vim':'vim', 'vimrc':'vim'
            \}
" ================================== Config End =======================================

" Keyword regular expression.
let s:aChar = '[a-zA-Z0-9_]'

let s:aUrl = {}
let s:aDict = {}

function s:GetFileType()
    return tolower((strridx(expand("%"),".") == -1) ?
                \"" :
                \strpart(expand("%"),(strridx(expand("%"),".") + 1)))
endf

function s:Init(fileType)
    if !has_key(s:aType, a:fileType)
        return 0
    endif

    if !has_key(s:aUrl, s:aType[a:fileType])
        " Thanks for Caglar Toklu
        let s:aUrl[s:aType[a:fileType]] = expand(substitute(globpath(&rtp, 'plugin/assistant/'), "\n", ',', 'g').s:aType[a:fileType].'.txt')
    endif

    if !has_key(s:aDict, s:aType[a:fileType])
        let s:aDict[s:aType[a:fileType]] = {}
        for line in readfile(s:aUrl[s:aType[a:fileType]])
            let mList = matchlist(substitute(line, '\s*=\s*>\s*', '=>', ''), '^\(.\+\)=\s*>\(.\+\)$')
            if len(mList) >= 3
                let s:aDict[s:aType[a:fileType]][mList[1]] = mList[2]
            endif
        endfor
    endif

    return 1
endf

function s:Help()
    let fileType = s:GetFileType()
    if !s:Init(fileType)
        echo 'assistant.MISS : Does not support the file type "'.fileType.'"'
        return
    endif

    let str = getline(".")
    let col = col(".")
    let end = col("$")

    let num = col - 1
    while num >= 0
        if strpart(str, num, 1) !~ s:aChar
            break
        endif
        let lcol = num
        let num -= 1
    endw
    if !exists("lcol")
        echo 'assistant.ERR : The current contents of the cursor is not a keyword'
        return
    endif

    let num = col - 1
    while num <= end
        if strpart(str, num, 1) !~ s:aChar
            break
        endif
        let rcol = num
        let num += 1
    endw

    let key = strpart(str, lcol, rcol-lcol+1)
    let len = len(s:aDict[s:aType[fileType]]) - 1
    let keys = keys(s:aDict[s:aType[fileType]])
    let vals = values(s:aDict[s:aType[fileType]])

    let results = []
    while len >= 0
        if keys[len] ==? key || keys[len] =~? '^.\+:\{2}'.key.'$'
            call add(results, keys[len] . vals[len])
        endif
        let len -= 1
    endw

    if len(results) > 0
        echo join(sort(results), "\n")
    else
        echo 'assistant.MISS : Can not find the information on "'.key.'"'
    endif
endf

function Acomplete(start, base)
    if a:start
        let line = getline('.')
        let start = col('.') - 1
        while start > 0 && line[start - 1] =~ s:aChar
            let start -= 1
        endwhile
        return start
    else
        let fileType = s:GetFileType()
        if !s:Init(fileType) || a:base =~ '^\s*$'
            return []
        endif

        let key = a:base
        let len = len(s:aDict[s:aType[fileType]]) - 1
        let keys = keys(s:aDict[s:aType[fileType]])
        let vals = values(s:aDict[s:aType[fileType]])

        let results = []
        while len >= 0
            if keys[len] =~ '^'.key.s:aChar.'*$'
                call add(results, {'word':keys[len], 'menu':vals[len]})
            endif
            let len -= 1
        endw

        return sort(results)
    endif
endf

" Set user complete function
function s:SetCompletefunc()
    let type = s:GetFileType()
    if !has_key(s:aType, type)
        if filereadable(expand(substitute(globpath(&rtp, 'plugin/assistant/'), "\n", ',', 'g').type.'.txt'))
            let s:aType[type] = type
        endif
    endif

    if has_key(s:aType, type)
        set completefunc=Acomplete
    endif
endf
autocmd BufEnter,BufRead * :call s:SetCompletefunc()
autocmd Filetype * :call s:SetCompletefunc()

" vim:ft=vim:ff=unix:tabstop=4:shiftwidth=4:softtabstop=4:expandtab
" End of file : assistant.vim
