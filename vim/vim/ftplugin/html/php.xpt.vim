XPTemplate priority=personal

XPTvar $TRUE          TRUE
XPTvar $FALSE         FALSE
XPTvar $NULL          NULL
XPTvar $UNDEFINED     NULL

XPTvar $CURSOR_PH     /* cursor */
XPTvar $VOID_LINE     /* void */;

XPTvar $BRel          \n
XPTvar $BRfun         \n
XPTvar $BRif          \n
XPTvar $BRloop        \n
XPTvar $BRstc         \n

XPTvar $SParg         ''
XPTvar $SPcmd         ' '
XPTvar $SPfun         ' '
XPTvar $SPop          ' '

XPT fe
<?php foreach ($`var^ as `container^): ?>
`cursor^
<?php endforeach; ?>

XPT if wrap
<?php if`$SPcmd^(`$SParg^`condition^`$SParg^): ?>
`cursor^
<?php endif; ?>

XPT els synonym=else
<?php else: ?>
`cursor^

XPT de
<?php echo Debug::vars($`variable^); `die;^ ?>

XPT e
<?php echo `cursor^; ?>

XPT ev
<?php echo $`variable^; ?>

XPT ph wrap
<?php

`cursor^

?>

XPT php wrap
<?php `cursor^ ?>
