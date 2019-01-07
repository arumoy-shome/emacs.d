;; do not pollute my init file Custom!
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (require 'custom))

;; unleash the beast!
;; taken from howardabrams/dot-files
(setq gc-cons-threshold 50000000)
(setq gnutls-min-prime-bits 4096)

;; disable bells and confirmation dialog boxes
(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil)

;; reclaim display real estate
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode t)

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

;; init straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; straight.el config
(setq straight-use-package-by-default t)

;; init use-package we have to use straight-use-package since
;; use-package is not loaded yet
(straight-use-package 'use-package)

(provide 'core)
