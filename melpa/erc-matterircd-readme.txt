Helpers to format *italic* and [link](url) plus removal of redundant GIF
bits for easy display via erc-image (although this is not required, it
is recommended), also will automatically connect to mattermost when
connecting to a matterircd server, and finally ensures @ is prepended
when completing nicknames in when connected to matterircd.

(require 'erc-matterircd)
(setq erc-matterircd-server "mattermost.server")
(setq erc-matterircd-team "mytest")
(setq erc-matterircd-password "password")
(add-to-list 'erc-modules 'matterircd)
(erc-update-modules)

Then connect to matterircd as a normal erc server:
(erc :server "localhost" :port "6667" :nick "mynick")
