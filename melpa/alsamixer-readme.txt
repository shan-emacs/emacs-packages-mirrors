Very basic interface to amixer commandline tool to control audio
volume.

(global-set-key (kbd "<XF86AudioRaiseVolume>") #'alsamixer-up-volume)
(global-set-key (kbd "<XF86AudioLowerVolume>") #'alsamixer-down-volume)
(global-set-key (kbd "<XF86AudioMute>") #'alsamixer-toggle-mute)
