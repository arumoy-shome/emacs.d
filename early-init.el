;; early-init.el -*- lexical-binding: t; -*-

;; startup optimization: set `gc-cons-threshold' to a high value now
;; and reset it later
(defvar aru/orig-gc-cons-threshold gc-cons-threshold
  "Original value of `gc-cons-threshold.'")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold aru/orig-gc-cons-threshold)))

(menu-bar-mode -1)			; disable menu-bar
(tool-bar-mode -1)			; disable tool-bar
(tooltip-mode -1)			; disable tooltip
(scroll-bar-mode -1)			; disable scroll-bars
(horizontal-scroll-bar-mode -1)
(setq package-enable-at-startup nil)    ; do not initialize package manager
(setq frame-inhibit-implied-resize t)   ; do not resize frame this early
