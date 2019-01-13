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
		    :font "Source Code Pro-14")

(add-to-list 'default-frame-alist '(ns-transparent-titlebar	.	t))
(add-to-list 'default-frame-alist '(ns-appearance		.	dark))
(add-hook 'window-setup-hook #'toggle-frame-maximized)

(defun aru/load-theme ()
  "Load the theme of my choice. This function is run with the after-init-hook"
  (load-theme 'spacemacs-light t))

(provide 'core-ui)
