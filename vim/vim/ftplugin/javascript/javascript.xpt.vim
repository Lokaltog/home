XPTemplate priority=personal

XPTvar $TRUE          true
XPTvar $FALSE         false
XPTvar $NULL          null
XPTvar $UNDEFINED     null

XPTvar $CURSOR_PH     /* cursor */
XPTvar $VOID_LINE     /* void */;

XPTvar $BRel          ' '
XPTvar $BRfun         ' '
XPTvar $BRif          ' '
XPTvar $BRloop        ' '
XPTvar $BRstc         ' '

XPTvar $SParg         ''
XPTvar $SPcmd         ' '
XPTvar $SPfun         ' '
XPTvar $SPop          ' '

XPT cb wrap " // comment block {{{ ... }}}
// `comment^ {{{
	`cursor^
// }}}
