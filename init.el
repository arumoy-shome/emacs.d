;; disable bells and confirmation dialog boxes
(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil)

;; reclaim display real estate
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(show-paren-mode t)

;; quicker confirmations
(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-processes nil)

;; save session, cursor location & window arrangement
(desktop-save-mode t)
(save-place-mode t)
(winner-mode 1)

;; secure network
(setq gnutls-verify-error t
      tls-checktrust t)

;; init straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; straight.el config
(setq straight-use-package-by-default t)

;; init use-package we have to use straight-use-package since
;; use-package is not loaded yet
(straight-use-package 'use-package)

(use-package all-the-icons)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t "turn on bold universally")
  (doom-themes-enable-italic t "turn on italics is universally")
  :config
  (load-theme 'doom-nord-light t)
  (doom-themes-visual-bell-config))

(use-package which-key
  :config
  (which-key-mode))

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)))

(use-package telephone-line
  :config
  (telephone-line-mode 1))

;; TODO move this into it's own file
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
