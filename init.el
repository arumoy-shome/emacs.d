;; all config files under elisp dir
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

(require 'core)

(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-bold t "turn on bold universally")
  (doom-themes-enable-italic t "turn on italics is universally")
  :config
  :hook (after-init . aru/colors-dark))

(use-package magit
  :straight t
  :bind
  ("C-x g"	.	magit-status))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  :hook ((prog-mode	.	smartparens-mode)
	 (text-mode	.	smartparens-mode)))

;; taken from jwiegley/dot-emacs
(use-package winner
  :hook (after-init	.	winner-mode)
  :bind
  (("M-n"		.	winner-redo)
   ("M-p"		.	winner-undo)))

(use-package paren
  :hook (after-init	.	show-paren-mode))

(use-package whitespace
  :commands
  (whitespace-buffer
   whitespace-cleanup
   whitespace-mode
   whitespace-turn-off))

;; using the builtin org for now
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

(use-package auto-complete
  :straight t
  :hook
  (after-init . ac-config-default))
