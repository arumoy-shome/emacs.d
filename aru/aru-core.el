;; aru-core.el  -*- lexical-binding: t; -*-

(setq use-short-answers t)		; quicker confirmations
(setq create-lockfiles nil)

(setq initial-scratch-message (concat ";; Welcome, " (system-name)))
(setq initial-major-mode 'fundamental-mode)
(setq user-full-name "Arumoy Shome")
(setq user-mail-address "contact@arumoy.me")

;;; ui
(setq ring-bell-function 'ignore) ;; disable bells and confirmation dialog boxes
(setq echo-keystrokes 0.02)
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))
(setq max-mini-window-height 0.15)
(setq x-underline-at-descent-line t)

(set-face-attribute 'default nil
		    :family "Source Code Pro" :height 130)

;;; editor
(setq sentence-end-double-space nil)
(setq tab-always-indent 'complete)      ; first indent, then complete

(provide 'aru-core)
