" PHP tags highlighting
syn region phpRegion matchgroup=phpDelimiter start="<?\(php\)\=" end="?>" contains=@phpClTop keepend

" Dim semicolons
syn match phpEOL ";\s*$" contained containedin=phpRegion

" Dim string delimiters
syn region phpStringDouble matchgroup=phpStringDelim start=+"+ skip=+\\\\\|\\"+ end=+"+  contains=@phpAddStrings,phpIdentifier,phpSpecialChar,phpIdentifierSimply,phpIdentifierComplex contained keepend
syn region phpBacktick matchgroup=phpStringDelim start=+`+ skip=+\\\\\|\\"+ end=+`+  contains=@phpAddStrings,phpIdentifier,phpSpecialChar,phpIdentifierSimply,phpIdentifierComplex contained keepend
syn region phpStringSingle matchgroup=phpStringDelim start=+'+ skip=+\\\\\|\\'+ end=+'+  contains=@phpAddStrings contained keepend

if !exists("php_ignore_phpdoc")
	syn case ignore

	syn match phpCommentStar contained "^\s*\*[^/]"me=e-1
	syn match phpCommentStar contained "^\s*\*$"

	syn region phpDocComment   start="/\*\*" end="\*/" keepend contains=phpCommentTitle,phpDocTags,phpTodo
	syn region phpCommentTitle contained matchgroup=phpDocComment start="/\*\*" matchgroup=phpCommmentTitle keepend end="\.$" end="\.[ \t\r<&]"me=e-1 end="[^{]@"me=s-2,he=s-1 end="\*/"me=s-1,he=s-1 contains=phpCommentStar,phpTodo,phpDocTags containedin=phpComment

	syn region phpDocTags  start="{@\(example\|id\|internal\|inheritdoc\|link\|source\|toc\|tutorial\)" end="}" containedin=phpComment
	syn match  phpDocTags  "@\(abstract\|access\|author\|category\|copyright\|deprecated\|example\|final\|global\|ignore\|internal\|license\|link\|method\|name\|package\|param\|property\|return\|see\|since\|static\|staticvar\|subpackage\|todo\|tutorial\|uses\|var\|version\)\s\+\S\+" contains=phpDocParam containedin=phpComment
	syn match  phpDocParam contained "\(\s\S\+\)\+"
	syn match  phpDocTags  "@\(filesource\|static\)" containedin=phpComment

	syn case match

	command! -nargs=+ PhpHiLink hi def link <args>

	PhpHiLink phpCommentTitle SpecialComment
	PhpHiLink phpDocComment   Comment
	PhpHiLink phpDocTags      Special
	PhpHiLink phpDocParam     Function
	PhpHiLink phpCommentStar  Comment

	delcommand PhpHiLink
endif
