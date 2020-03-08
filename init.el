;; all config files under elisp dir
(add-to-list 'load-path (concat user-emacs-directory "aru/"))

(require 'aru-packages)

(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

(use-package gcmh
  :straight t
  :config
  (gcmh-mode +1)
  :blackout t)

(use-package no-littering :straight t :demand t)

(use-package aru-core :demand t)

(use-package menu-bar :config (menu-bar-mode -1))
(use-package tool-bar :config (tool-bar-mode -1))
(use-package tooltip :config (tooltip-mode -1))
(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(use-package delsel :config (setq delete-selection-mode t))
(use-package frame :config (blink-cursor-mode 0))
(use-package hl-line :config (global-hl-line-mode nil))
(use-package novice :config (setq disabled-command-function nil))
(use-package isearch :config (setq lazy-highlight-initial-delay 0))
(use-package saveplace :config (save-place-mode +1))
(use-package ibuffer :bind (([remap list-buffers] . #'ibuffer)))
(use-package windmove :config (windmove-default-keybindings))
(use-package winner :config (winner-mode +1))
(use-package paren :config (show-paren-mode +1))
(use-package text-mode :hook (text-mode	. auto-fill-mode))

(use-package files
  :config
  (setq confirm-kill-processes nil)
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (setq find-file-visit-truename t)
  (setq find-file-suppress-same-file-warnings t))

(use-package cus-edit
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (require 'custom)))

(use-package recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package simple                     ; case bindings for active region
  :bind
  (("M-c" . capitalize-dwim)
   ("M-l" . downcase-dwim)
   ("M-u" . upcase-dwim))
  :config
  (setq kill-do-not-save-duplicates t)
  (blackout 'auto-fill-mode))

(use-package selectrum
  :straight (selectrum :host github :repo "raxod502/selectrum")
  :config
  (selectrum-mode +1))

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode +1))

(use-package selectrum-prescient
  :straight (selectrum-prescient :host github :repo "raxod502/prescient.el"
                                 :files ("selectrum-prescient.el"))
  :after selectrum
  :config
  (selectrum-prescient-mode +1))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :hook
  (after-init . exec-path-from-shell-initialize))

(use-package projectile
  :straight t
  :config
  (setq projectile-completion-system 'default) ; use selectrum instead of ido
  (setq projectile-switch-project-action 'projectile-commander) ; ask what to do when switching
  
  ;; (def-projectile-commander-method ?/C-m
  ;;   "Find file in project."
  ;;   (call-interactively #'find-file))
  
  (projectile-mode +1)
  
  (defun aru/projectile-indexing-method-p (method)
    "Non-nil if METHOD is a safe value for `projectile-indexing-method'."
    (memq method '(native alien)))

  (put 'projectile-indexing-method 'safe-local-variable
       #'aru/projectile-indexing-method-p)
  
  (dolist (key '("C-r" "R"))
    (bind-key key #'projectile-replace-regexp projectile-command-map))
  :bind-keymap*
  (("C-c p" . projectile-command-map))
  :blackout t)

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config)              ; correct and improve org-mode native fontification
  :hook
  (after-init . aru/colors-dark))

(use-package magit
  :straight t
  :bind
  ("C-x g"	.	magit-status))

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  :hook ((prog-mode	.	smartparens-mode)
	 (text-mode	.	smartparens-mode))
  :blackout t)

(use-package whitespace
  :commands
  (whitespace-buffer
   whitespace-cleanup
   whitespace-mode
   whitespace-turn-off)
  :blackout t)

;; using the builtin org for now
(use-package org
  :config
  (defconst aru/org-inbox-file (expand-file-name "inbox.org" org-directory)
    "File to use for capturing org items")
  
  (setq org-directory "~/Dropbox/org")
  (setq org-ellipsis " ▼ ")
  (setq org-babel-load-languages '((emacs-lisp	. t)
				   (python			. t)
				   (shell			. t)))
  (setq org-agenda-files (expand-file-name "org-agenda-files.org" org-directory))
  (setq org-default-notes-file aru/org-inbox-file)
  (setq org-capture-templates
	'(("i" "Item" item (file+headline aru/org-inbox-file "Inbox")
	   "Note taken on %U \\\\\n%?" ; have to escape the '\\'
	   :prepend t)
	  ("t" "Todo" entry (file+headline aru/org-inbox-file "Inbox")
	   "** TODO %?"
	   :prepend t)))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "NEXT(n)" "WAITING(w@/!)" "LATER(l)" "|" "CANCELLED(c@)")))
  (setq org-todo-keyword-faces
	'(("WAITING" :inherit default :weight bold)
	  ("LATER" :inherit warning :weight bold)))
  :bind
  (("C-c l"		.	org-store-link)
   ("C-c a"		.	org-agenda)
   ("C-c c"		.	(lambda () (interactive) (org-capture nil))))
  :hook
  ((org-mode		.	org-indent-mode)))

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
  (doi-add-bibtex-entry)
  :bind
  ;; ("C-c ]" . org-ref-helm-insert-cite-link)
  )

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
  :config
  (mu4e-maildir "~/mail")
  ;; mu4e binary comes with mu which I install with brewn
  :load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
