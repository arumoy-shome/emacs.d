;;; core
(setq gnutls-min-prime-bits 4096)
(defalias 'yes-or-no-p 'y-or-n-p)       ; quicker confirmations
(setq gnutls-verify-error t)             ; secure network
(setq tls-checktrust t)
(setq create-lockfiles nil)
(setq inhibit-splash-screen t)
(setq initial-scratch-message (concat ";; Welcome, " (system-name)))
(setq initial-major-mode 'fundamental-mode)
(setenv "TERM" "xterm")
(setq shell-file-name "/usr/local/bin/bash")          ; used by (async-)shell-command
(setq explicit-shell-file-name "/usr/local/bin/bash") ; used by shell & term
(setq user-full-name "Arumoy Shome")
(setq user-mail-address "contact@arumoy.me")

;;; ui
(setq ring-bell-function 'ignore) ;; disable bells and confirmation dialog boxes
(setq x-gtk-use-system-tooltips nil)
(setq use-dialog-box nil)
(setq echo-keystrokes 0.02)
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))
(setq custom-safe-themes t)
(setq max-mini-window-height 0.15)
(setq x-underline-at-descent-line t)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; default font
(set-face-attribute 'default nil
		    :family "Source Code Pro" :height 130 :weight 'normal)
(set-face-attribute 'fixed-pitch nil
                    :family "Iosevka" :height 1.0)
(set-face-attribute 'variable-pitch nil
                    :family "IBM Plex Serif" :height 1.0)

(add-hook 'window-setup-hook #'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;;; editor
(setq sentence-end-double-space nil)
(setq tab-width 2)			; length of tab
(setq tab-always-indent 'complete)      ; first indent, then complete
(setq-default indent-tabs-mode nil)	; use spaces instead of tabs

(provide 'aru-core)
