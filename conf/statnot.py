import os

STATUS_UPDATE_INTERVAL = 15.0
STATUS_COMMAND = ['/usr/bin/python', '%s/sync/bin/dwm-status' % os.getenv('HOME')]
