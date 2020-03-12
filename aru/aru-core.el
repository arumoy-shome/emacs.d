;;; core
(setq gnutls-min-prime-bits 4096)
(defalias 'yes-or-no-p 'y-or-n-p)       ; quicker confirmations
(setq gnutls-verify-error t)             ; secure network
(setq tls-checktrust t)
(setq create-lockfiles nil)
(setq inhibit-splash-screen t)
(setq initial-scratch-message (concat ";; Welcome, " (system-name)))
(setq initial-major-mode 'fundamental-mode)

;;; ui
(setq ring-bell-function 'ignore) ;; disable bells and confirmation dialog boxes
(setq x-gtk-use-system-tooltips nil)
(setq use-dialog-box nil)
(setq echo-keystrokes 0.02)
(setq mode-line-percent-position nil)
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))
(setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  " "
                  mode-line-position
                  " "
                  mode-line-modes
                  " "
                  mode-line-misc-info
                  mode-line-end-spaces))
(setq custom-safe-themes t)

;; default font
(set-face-attribute 'default nil
		    :family "Source Code Pro" :weight 'light)

(add-hook 'window-setup-hook #'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

;;; editor
(setq sentence-end-double-space nil)
(setq tab-width 2)			; length of tab
(setq-default indent-tabs-mode nil)	; use spaces instead of tabs

(provide 'aru-core)
