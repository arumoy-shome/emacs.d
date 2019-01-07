;; all config files under elisp dir
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

(require 'core)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t "turn on bold universally")
  (doom-themes-enable-italic t "turn on italics is universally")
  :config
  (load-theme 'doom-nord-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

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
  (after-init	.	doom-modeline-init))

(use-package magit
  :bind
  ("C-x g"	.	magit-status))

(use-package markdown-mode
  :mode
  ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)) ;taken from doom-emacs/modules/lang/markdown

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook (prog-mode	.	smartparens-mode))

(use-package restart-emacs
  :commands (restart-emacs))

(use-package hippie-exp
  :bind
  ([remap dabbrev-expand] . hippie-expand))

;; taken from jwiegley/dot-emacs
(use-package winner
  :hook (after-init	.	winner-mode)
  :bind
  (("M-n"		.	winner-redo)
   ("M-p"		.	winner-undo)))

(use-package paren
  :hook (after-init	.	show-paren-mode))

(use-package whole-line-or-region
  :hook (after-init	.	whole-line-or-region-mode))

(use-package whitespace
  :commands
  (whitespace-buffer
   whitespace-cleanup
   whitespace-mode
   whitespace-turn-off))
