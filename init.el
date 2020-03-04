;; all config files under elisp dir
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))

(require 'core)

(use-package telephone-line
  :straight t
  :init
  (setq telephone-line-evil-use-short-tag t
        telephone-line-primary-left-separator 'telephone-line-flat
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-flat
        telephone-line-secondary-right-separator 'telephone-line-nil)
  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-erc-modified-channels-segment
                     telephone-line-process-segment))
          (nil . (telephone-line-buffer-segment))))
  (setq telephone-line-rhs
        '((nil . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil . (telephone-line-airline-position-segment))))
  :hook
  (after-init . telephone-line-mode))

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :hook (after-init . evil-mode))

(use-package evil-surround
  :straight t
  :after (evil)
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :straight t
  :after (evil)
  :config
  (evil-commentary-mode))

(use-package helm
  :straight t
  :bind
  (("C-SPC"	.	helm-M-x)        ; default: execute-extended-command
   ("C-x C-f"	.	helm-find-files) ; default: find-file
   ("C-x b"	.	helm-mini)       ; default: switch to buffer
   ("C-h a"	.	helm-apropos)    ; default: apropos-command
   ("C-c h m"	.	helm-man-woman)
   ("C-c h i"	.	helm-info)
   ("C-c h r"	.	helm-info-emacs)))

(use-package package-helm
  :after helm
  :bind
  (("C-c n n"	.	aru/helm-browse-notes)
   ("C-c n p"	.	aru/helm-browse-project-notes)
   ("C-c n b"	.	aru/helm-browse-bib-notes)))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :hook
  (after-init . exec-path-from-shell-initialize))

(use-package projectile
  :straight t
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package helm-projectile
  :after (projectile)
  :straight t
  :config
  (helm-projectile-on))

(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-bold t "turn on bold universally")
  (doom-themes-enable-italic t "turn on italics is universally")
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
   ("C-c c"		.	(lambda () (interactive) (org-capture nil))))
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
