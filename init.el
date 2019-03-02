;; all config files under elisp dir
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

(require 'core)

(use-package doom-themes
  :straight t
  :defer 5
  :custom
  (doom-themes-enable-bold t "turn on bold universally")
  (doom-themes-enable-italic t "turn on italics is universally")
  :config
  (aru/load-theme))

(use-package spacemacs-theme
  :disabled
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
  :bind
  (("M-x"	.	helm-M-x)
   ("C-x C-f"	.	helm-find-files)
   ("C-x b"	.	helm-mini)
   ("C-h a"	.	helm-apropos)
   ("C-c h m"	.	helm-man-woman)
   ("C-c h i"	.	helm-info)
   ("C-c h r"	.	helm-info-emacs)))

(use-package package-helm
  :after helm
  :bind
  (("C-c n n"	.	aru/helm-browse-notes)
   ("C-c n p"	.	aru/helm-browse-project-notes)
   ("C-c n b"	.	aru/helm-browse-bib-notes)))

(use-package doom-modeline
  :straight t
  :init
  ;; date and time in the format: Day Mon Date 24h Time
  (setq display-time-format "%a %b %d %R"
	display-time-default-load-average nil
	doom-modeline-buffer-file-name-style 'file-name)
  :config
  (display-time-mode)
  (doom-modeline-def-modeline 'aru-doom-modeline
			      '(bar evil-state buffer-info remote-host buffer-position selection-info)
			      '(misc-info input-method major-mode process vcs))
  (doom-modeline-set-modeline 'aru-doom-modeline 'default)
  :hook
  (after-init	.	doom-modeline-init))

(use-package spaceline
  :disabled
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
  ;; taken from doom-emacs/modules/lang/markdown
  ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode))

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
		org-archive-location "::* Archive"
		org-babel-load-languages '((emacs-lisp	. t)
																(python			. t)
																(shell			. t)))
  :config
  (require 'package-org)
  (aru/setup-org-capture)
  (aru/setup-org-ui)
  :bind
  (("C-c l"		.	org-store-link)
   ("C-c a"		.	org-agenda)
   ("\C-cc"		.	(lambda () (interactive) (org-capture nil "i"))))
  :hook
  ((org-agenda-finalize	.	aru/setup-org-agenda)
   (org-mode		.	org-indent-mode)))

(use-package org-ref
  :straight t
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
   org-ref-helm-insert-cite-link)
  :bind
  ("C-c ]" . org-ref-helm-insert-cite-link))

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

(use-package org-zettel
  :commands
  (org-zettel-get-inbound-links))

(use-package tramp
	:disabled
  :config
  (add-to-list 'tramp-default-user-alist
	       '("ssh" "ssh.data.vu.nl" "ase247")))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package doc-view
	:custom
	(doc-view-resolution 300 "Improve quality of pdfs"))

(use-package mu4e
	:commands
	(mu4e)
	:custom
	(mu4e-maildir "~/mail" "Set the maildir for mu4e")
	;; mu4e binary comes with mu which I install with brew
	:load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")

(use-package evil
	:straight t
	:hook
	((text-mode		. evil-mode)
		(prog-mode	. evil-mode)))

(use-package evil-surround
	:straight t
	:after (evil)
	:config
	(global-evil-surround-mode 1))

(use-package evil-matchit
	:straight t
	:after (evil)
	:config
	(global-evil-matchit-mode 1))

(use-package evil-commentary
	:straight t
	:after (evil)
	:config
	(evil-commentary-mode))
