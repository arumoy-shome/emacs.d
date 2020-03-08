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

;; default font
(set-face-attribute 'default nil
		    :family "Source Code Pro" :weight 'light)

(add-hook 'window-setup-hook #'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(defun aru/colors-dark ()
  "switch to the dark colorscheme"
  (interactive)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (load-theme 'doom-tomorrow-night t))

(defun aru/colors-light ()
  "switch to the light colorscheme"
  (interactive)
  ;; FIXME titlebar text is not visible
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (load-theme 'doom-solarized-light t))

;;; editor
(setq sentence-end-double-space nil)
(setq tab-width 2)			; length of tab
(setq-default indent-tabs-mode nil)	; use spaces instead of tabs

(provide 'aru-core)