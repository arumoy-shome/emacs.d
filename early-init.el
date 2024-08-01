;; early-init.el --- Early init file -*- lexical-binding: t; -*-

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq use-dialog-box t)
(setq use-file-dialog nil)
(setq inhibit-startup-echo-area-message user-login-name)
(setq use-short-answers t)		; quicker confirmations
(setq create-lockfiles nil)

;; (add-to-list 'default-frame-alist '(undecorated-round . t)) ; no titlebar

(setq initial-scratch-message (concat ";; Welcome, " (system-name)))
(setq initial-major-mode 'fundamental-mode)
(setq user-full-name "Arumoy Shome")
(setq user-mail-address "contact@arumoy.me")
(setq ring-bell-function 'ignore) ;; disable bells and confirmation dialog boxes
(setq echo-keystrokes 0.02)
(setq mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))
(setq max-mini-window-height 0.15)
(setq x-underline-at-descent-line t)

(set-face-attribute 'default nil :family "SauceCodePro Nerd Font" :height 130)
;; (set-face-attribute 'variable-pitch nil :family "Zed Sans Extended" :height 140)

;;; editor
(setq sentence-end-double-space nil)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar prot-emacs--file-name-handler-alist file-name-handler-alist)
(defvar prot-emacs--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist prot-emacs--file-name-handler-alist
                  vc-handled-backends prot-emacs--vc-handled-backends)))

;; Initialise installed packages at this early stage, by using the
;; available cache.  I had tried a setup with this set to nil in the
;; early-init.el, but (i) it ended up being slower and (ii) various
;; package commands, like `describe-package', did not have an index of
;; packages to work with, requiring a `package-refresh-contents'.
(setq package-enable-at-startup t)

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))
