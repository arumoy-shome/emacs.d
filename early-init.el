;; early-init.el -*- lexical-binding: t; -*-

;; startup optimization: set `gc-cons-threshold' to a high value now
;; and reset it later
(defvar aru/orig-gc-cons-threshold gc-cons-threshold
  "Original value of `gc-cons-threshold.'")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold aru/orig-gc-cons-threshold)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq use-dialog-box t)
(setq use-file-dialog nil)
(setq inhibit-startup-echo-area-message user-login-name)
