========================================
Remove default holidays, then append NZ calendar:

(setq holiday-general-holidays nil)
(setq holiday-christian-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)
(setq holiday-bahai-holidays nil)
(setq holiday-oriental-holidays nil)

(setq calendar-holidays (append holiday-local-holidays holiday-nz-holidays))
