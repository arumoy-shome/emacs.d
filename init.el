;; all config files under elisp dir
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

(require 'core)

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
  (("M-x"	.	helm-M-x)
   ("C-x C-f"	.	helm-find-files)
   ("C-x b"	.	helm-mini)
   ("C-h a"	.	helm-apropos)))

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-init))

(use-package magit
  :commands
  (magit-status)
  :bind
  ("C-x g"	.	magit-status))
