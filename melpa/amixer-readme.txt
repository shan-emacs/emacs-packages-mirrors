Very basic interface to amixer commandline tool to control audio
volume.

(global-set-key (kbd "<XF86AudioRaiseVolume>") #'amixer-up-volume)
(global-set-key (kbd "<XF86AudioLowerVolume>") #'amixer-down-volume)
(global-set-key (kbd "<XF86AudioMute>") #'amixer-toggle-mute)
