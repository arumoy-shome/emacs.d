;; disable bells and confirmation dialog boxes
(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil)

;; reclaim display real estate
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(blink-cursor-mode 0)
(global-hl-line-mode nil)

;; show scratch instead of splash screen
(setq inhibit-splash-screen t
      initial-scratch-message (concat ";; Welcome, " (system-name)))

;; default font
(set-face-attribute 'default nil
		    :family "Source Code Pro" :weight 'light)

(add-hook 'window-setup-hook #'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(defun aru/colors-dark ()
  "switch to the dark colorscheme"
  (interactive)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (load-theme 'doom-solarized-dark t))

(defun aru/colors-light ()
  "switch to the light colorscheme"
  (interactive)
  ;; FIXME titlebar text is not visible
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (load-theme 'doom-solarized-light t))

(provide 'core-ui)
