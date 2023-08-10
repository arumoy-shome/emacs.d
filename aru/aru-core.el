;; aru-core.el  -*- lexical-binding: t; -*-

(setq use-short-answers t)		; quicker confirmations
(setq create-lockfiles nil)

(setq initial-scratch-message (concat ";; Welcome, " (system-name)))
(setq initial-major-mode 'fundamental-mode)
(setq user-full-name "Arumoy Shome")
(setq user-mail-address "contact@arumoy.me")
(setq shell-file-name "/usr/local/bin/bash")

;;; ui
(setq ring-bell-function 'ignore) ;; disable bells and confirmation dialog boxes
(setq echo-keystrokes 0.02)
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))
(setq max-mini-window-height 0.15)
(setq x-underline-at-descent-line t)

(set-face-attribute 'default nil
		    :family "Source Code Pro" :height 140 :weight 'normal)

;;; editor
(setq sentence-end-double-space nil)
(setq tab-always-indent 'complete)      ; first indent, then complete
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))

(provide 'aru-core)
