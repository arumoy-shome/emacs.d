;; all config files under elisp dir
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

(require 'core)

(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-bold t "turn on bold universally")
  (doom-themes-enable-italic t "turn on italics is universally")
  :config
  (load-theme 'doom-nord-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package helm
  :straight t
  :bind
  (("M-x"	.	helm-M-x)
   ("C-x C-f"	.	helm-find-files)
   ("C-x b"	.	helm-mini)
   ("C-h a"	.	helm-apropos)
   ("C-c h m"	.	helm-man-woman)
   ("C-c h i"	.	helm-info)
   ("C-c h r"	.	helm-info-emacs)))

(use-package doom-modeline
  :straight t
  :hook
  (after-init	.	doom-modeline-init))

(use-package magit
  :straight t
  :bind
  ("C-x g"	.	magit-status))

(use-package markdown-mode
  :straight t
  :mode
  ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)) ;taken from doom-emacs/modules/lang/markdown

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  :hook ((prog-mode	.	smartparens-mode)
	 (text-mode	.	smartparens-mode)))

(use-package restart-emacs
  :straight t
  :commands (restart-emacs))

(use-package hippie-exp
  :straight t
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
  :straight t
  :hook (after-init	.	whole-line-or-region-mode))

(use-package whitespace
  :commands
  (whitespace-buffer
   whitespace-cleanup
   whitespace-mode
   whitespace-turn-off))

(use-package org
  :init
  (setq org-ellipsis " ▼ "
	org-archive-location "::* Archive")
  :config
  (require 'package-org)
  (aru/setup-org-capture)
  (aru/setup-org-ui)
  :bind
  (("C-c l"		.	org-store-link)
   ("C-c a"		.	org-agenda)
   ("C-c c"		.	org-capture))
  :hook
  ((org-agenda-finalize	.	aru/setup-org-agenda)
   (org-mode		.	org-indent-mode)))

(use-package text-mode
  :hook (text-mode	.	auto-fill-mode))
