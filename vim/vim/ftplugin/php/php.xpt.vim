XPTemplate priority=personal

XPTvar $TRUE          TRUE
XPTvar $FALSE         FALSE
XPTvar $NULL          NULL
XPTvar $UNDEFINED     NULL

XPTvar $CURSOR_PH     // cursor
XPTvar $VOID_LINE     // void

XPTvar $BRel          \n
XPTvar $BRfun         \n
XPTvar $BRif          \n
XPTvar $BRloop        \n
XPTvar $BRstc         \n

XPTvar $SParg         ''
XPTvar $SPcmd         ' '
XPTvar $SPfun         ' '
XPTvar $SPop          ' '

XPT fun synonym=function
`public ^function `function_name^`$SPfun^(`params^)`$BRfun^{
	`cursor^
}

XPT ca synonym=case  wrap
case `constant^:
	`cursor^
break;

XPT sw
`Include:switch^

XPT fe
`Include:foreach^

XPT wh
`Include:while^

XPT eli synonym=elif|elseif wrap
else`Include:if^

XPT els synonym=else wrap
else`$BRif^{
    `cursor^
}

XPT ar
array(`cursor^)

XPT arr
array(
	`cursor^
)

XPT ak
'`key^' => `value^,`cursor^

XPT try wrap
try`$BRfun^{
	`cursor^
}
catch`$SPfun^(`Exception^ `evar^$e^)`$BRfun^{
}
