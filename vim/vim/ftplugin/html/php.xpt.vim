XPTemplate priority=personal

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
