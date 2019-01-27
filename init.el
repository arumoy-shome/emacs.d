;; all config files under elisp dir
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

(require 'core)

(use-package doom-themes
  :disabled
  :straight t
  :custom
  (doom-themes-enable-bold t "turn on bold universally")
  (doom-themes-enable-italic t "turn on italics is universally")
  :config
  ;; (load-theme 'doom-nord-light t)
  (doom-themes-visual-bell-config)
  :hook
  (org-mode	.	doom-themes-org-config))

(use-package spacemacs-theme
  :defer t
  :straight t
  :custom
  (spacemacs-theme-comment-bg nil "turn off comment background")
  (spacemacs-theme-comment-italic t "turn on comment italics")
  (spacemacs-theme-org-bold t "turn on bold org headers")
  (spacemacs-theme-org-height t "turn on varying org header heights")
  (spacemacs-theme-org-agenda-height t "turn on varing org agenda heights")
  (spacemacs-theme-underline-parens t "underline parens when using show-paren-mode")
  :hook
  (after-init	.	aru/load-theme))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package helm
  :straight t
  :config
  (require 'package-helm)
  :bind
  (("M-x"	.	helm-M-x)
   ("C-x C-f"	.	helm-find-files)
   ("C-x b"	.	helm-mini)
   ("C-h a"	.	helm-apropos)
   ("C-c h m"	.	helm-man-woman)
   ("C-c h i"	.	helm-info)
   ("C-c h r"	.	helm-info-emacs)
   ("C-c n n"	.	aru/helm-browse-notes)
   ("C-c n p"	.	aru/helm-browse-project-notes)))

(use-package doom-modeline
  :disabled
  :straight t
  :hook
  (after-init	.	doom-modeline-init))

(use-package spaceline
  :straight t
  :config
  (require 'spaceline-config)
  :hook
  (after-init	.	spaceline-spacemacs-theme)
  (after-init	.	spaceline-helm-mode)
  :custom
  (spaceline-minor-modes-p nil "turn off minor modes segment")
  (spaceline-buffer-encoding-abbrev-p nil "turn off buffer encoding segment")
  (powerline-default-separator 'slant "set default separator"))

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

(use-package org-ref
  :straight t
  :after org
  :init
  (setq reftex-default-bibliography '("~/org/bib/ref.bib")
	org-ref-bibliography-notes "~/org/bib/notes.org"
	org-ref-default-bibliography '("~/org/bib/ref.bib")
	org-ref-pdf-directory "~/org/bib/pdfs/")
  (setq bibtex-completion-bibliography reftex-default-bibliography
	bibtex-completion-library-path org-ref-pdf-directory
	bibtex-completion-notes-path org-ref-bibliography-notes)
  :commands
  (doi-add-bibtex-entry
   org-ref-helm-insert-cite-link))

(use-package text-mode
  :hook (text-mode	.	auto-fill-mode))

(use-package fish-mode
  :straight t
  :mode ("\\.fish\\'"	.	fish-mode))

(use-package eshell
  :commands eshell
  :config
  (setq eshell-banner-message
	'(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face))
	eshell-kill-processes-on-exit t))

(use-package ace-window
  :straight t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind
  ("C-x o" . ace-window))
