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

;; secure network
(setq gnutls-verify-error t
      tls-checktrust t)

;; prevent emacs backups from littering my system
(setq auto-save-visited-interval 6000	;auto save every 10 minutes
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "backups"))))

;; disable disabled commands
(setq disabled-command-function nil)

;; strip trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'core-ui)
(require 'core-packages)

(provide 'core)
