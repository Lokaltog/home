XPTemplate priority=personal

XPT de
echo Debug::vars($`variable^); `die;^`cursor^

XPT fi
'`field^' => Jelly::field('`type^'`^),`cursor^

XPT url
Route::url('`default^', array('controller' => '`controller^', 'action' => '`action^'))

XPT _redir hidden
`req^->redirect(`Include:url^);

XPT redir alias=_redir
XSET req=\$this->request

XPT iredir alias=_redir
XSET req=Request::instance\()

XPT jq
Jelly::query('`model^')
