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
