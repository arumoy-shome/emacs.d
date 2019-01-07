;; do not pollute my init file Custom!
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (require 'custom))

;; unleash the beast!
;; taken from howardabrams/dot-files
(setq gc-cons-threshold 50000000)
(setq gnutls-min-prime-bits 4096)

;; quicker confirmations
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-processes nil)

;; save session, cursor location & window arrangement
(desktop-save-mode t)
(save-place-mode t)
(winner-mode 1)

;; secure network
(setq gnutls-verify-error t
      tls-checktrust t)

;; prevent emacs backups from littering my system
(auto-save-visited-mode 1)		;auto save in the same file
(setq auto-save-visited-interval 600	;auto save every 10 minutes
      backup-directory-alist '(("." . "~/.emacs.d/backups/"))) ;centralized location for backups

;; disable disabled commands
(setq disabled-command-function nil)

(require 'core-ui)
(require 'core-packages)

(provide 'core)
