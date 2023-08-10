;;; init.el --- personal emacs config -*- lexical-binding: t -*-

;;; Commentary:
;; This is my personal emacs configuration for Emacs 28 & above. While
;; many in the emacs community prefer a literate programming style
;; configuration, I prefer to keep mine in a single init.el file. This
;; file is loaded by emacs during the initialisation process.

;; Where possible & relevant I provide detailed comments on the
;; rationale behind a particular configuration/setting. Sometimes I
;; document my experience with alternative configs and why I chose the
;; one that is exists in this file. This is primarily to help me
;; remember if I revisit that particular config (which I often do).

;; I use outline-minor-mode to create an outline of this file.
;; Comments preceded by 3 semi-colons (;;;) result in an outline
;; header. Any comments with more than 3 semi-colons result in a
;; sub-header.

;;; Code:
;; This section contains the actual configuration. It is divided into
;; several sub sections. You can navigate this file using
;; outline-minor-mode bindings. Use C-c @ t to fold everything. With
;; point on a header, you can hit C-c @ s to show the header. You can
;; navigate between headers using C-c @ n and C-c @ p. You can see all
;; outline-minor-mode bindings using C-c @ C-h.

;; Emacs provides several toggle-like functionality. For example,
;; minor-modes for spell checking, syntax checking, line numbers, etc
;; can be toggled using the same command. I follow a convension of
;; assigning such frequently used minor modes to the ESC ESC
;; (translates to ESC M-<k> where k is the key I specify in my config)
;; prefix. Someday, I may migrate this to a dedicated keymap.

(add-to-list 'load-path (concat user-emacs-directory "aru/"))
(add-to-list 'load-path (concat user-emacs-directory "site/" "nano-emacs/"))

(use-package use-package-core
  :demand t
  :config
  (setq use-package-compute-statistics t)
  (setq use-package-verbose t))

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package diminish :ensure t)
(use-package aru-core :demand t)
(use-package cus-edit
  :demand t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(use-package eldoc
  :diminish t
  :hook ((after-init . global-eldoc-mode)))

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)))

(use-package elec-pair
  :hook ((after-init . electric-pair-mode)))

(use-package autorevert
  :hook  ((after-init . global-auto-revert-mode)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package so-long
  :hook ((after-init . global-so-long-mode)))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)))

(use-package outline
  ;; NOTE additionally set outline-minor-mode-prefix using custom
  :diminish outline-minor-mode
  :init
  (setq outline-minor-mode-cycle t)
  (setq outline-minor-mode-use-buttons nil)
  :hook (prog-mode . outline-minor-mode))

(use-package savehist
  :hook ((after-init . savehist-mode)))

(use-package compile
  :config
  (setq-default compilation-scroll-output t))

(use-package flyspell
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-dictionary "en_GB")
  :hook ((prog-mode . flyspell-prog-mode))
  :bind (("ESC M-s" . flyspell-mode)))	; ESC ESC s

(use-package flymake
  :bind (("ESC M-f" . flymake-mode)	; ESC ESC f
	 ("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error)))

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-use-ls-dired nil)
  (setq dired-listing-switches "-FAlrh")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  :hook ((dired-mode . dired-hide-details-mode)))

(use-package frame
  :bind (("C-z" . nil)))		; use C-x C-z instead

(use-package window
  :bind-keymap (("s-4" . ctl-x-4-map)
                ("s-5" . ctl-x-5-map))
  :bind (("s-f" . find-file)
	 ("s-b" . switch-to-buffer)
         ("s-1" . delete-other-windows)
	 ("s-2" . split-window-below)
	 ("s-3" . split-window-right)
	 ("s-]" . (lambda () (interactive) (other-window +1)))
         ("s-[" . (lambda () (interactive) (other-window -1))))
  :hook ((help-mode . visual-line-mode)
	 (man-mode . visual-line-mode)
	 (woman-mode . visual-line-mode)))

(use-package windmove
  :bind
  (("<left>"  . windmove-left)
   ("<right>" . windmove-right)
   ("<up>"    . windmove-up)
   ("<down>"  . windmove-down)
   ("S-<left>" . windmove-swap-states-left)
   ("S-<right>" . windmove-swap-states-right)
   ("S-<up>" . windmove-swap-states-up)
   ("S-<down>" . windmove-swap-states-down)))

(use-package paren
  :config
  (setq show-paren-delay 0.1)
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t)
  :hook ((after-init . show-paren-mode)))

(use-package files
  :config
  (setq confirm-kill-processes nil)
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (setq find-file-visit-truename t)
  (setq find-file-suppress-same-file-warnings t)
  (setq require-final-newline t)
  :bind (("s-q" . nil)))		; I hit this by mistake instead of M-q!

(use-package recentf
  :config
  (setq-default recentf-max-saved-items 1000)
  (setq-default recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
  :hook ((after-init . recentf-mode))
  :bind (:map ctl-x-map
	      ("f" . recentf-open)))	; default: set-fill-column

(use-package simple                     ; case bindings for active region
  :bind
  (("M-Q" . delete-indentation)
   ("ESC M-v" . visual-line-mode)	; ESC ESC v
   ("ESC C-M-i" . indent-tab-mode))	; ESC ESC TAB
  :config
  (setq kill-do-not-save-duplicates t)
  (setq async-shell-command-display-buffer nil)
  (setq shell-command-prompt-show-cwd t))

(use-package magit
  :ensure t
  :bind (:map ctl-x-map
              ("g" . magit-status)))

(use-package whitespace
  :bind (("ESC M-w" . whitespace-mode)))	; ESC ESC w

(use-package org
  ;; :hook ((org-mode . (lambda () (electric-indent-local-mode -1))) ; do not auto indent in org buffers
  ;; 	 (org-capture-mode . org-id-get-create)	; add ID automatically to all captured items
  ;; 	 (org-mode . (lambda () (setq-default indent-tabs-mode nil)))) ; do not use tabs for indent
  :config
  ;; advice
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers))) ; save all org agenda buffers after refile, stolen from purcell
  ;; general
  ;; (org-indent-mode -1)                  ; [default] do not indent text based on outline
  ;; (setq org-src-window-setup 'split-window-below)
  (setq org-startup-folded t)
  (setq org-reverse-note-order t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-goto-auto-isearch nil)
  (setq org-hide-block-startup t)
  (setq org-return-follows-link nil)
  ;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-directory "~/org")
  (defconst aru/org-inbox-file (expand-file-name "inbox.org" org-directory)
    "File to use for capturing org items.")
  (defconst aru/org-block-file (expand-file-name "blocks.org" org-directory)
    "File to use for capturing work blocks.")
  (defconst aru/org-wiki-file (expand-file-name "wiki.org" org-directory)
    "File to use for capturing wiki items.")
  (setq org-log-into-drawer t)
  (setq org-default-notes-file aru/org-inbox-file)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  ;; refile
  (setq org-refile-use-outline-path 'file) ; allow refiling as top level header
  (setq org-refile-targets '((nil . (:maxlevel . 2))
                             (org-agenda-files . (:maxlevel . 2))))
  ;; agenda
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-files (expand-file-name "org-agenda-files.org" org-directory))
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-ignore-drawer-properties '(effort appt stats))
  (setq org-agenda-custom-commands
	'(("g" "GTD" tags-todo "-TODO=\"PROJECT\"-paper")))

  ;; babel
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (python     . t)
				 (shell      . t)))
  ;; capture
  (setq org-capture-templates
	'(("p" "Paper" entry (file aru/org-inbox-file)
           "%[~/.emacs.d/org-templates/paper.txt]" :prepend t)
	  ("c" "Capture" entry (file aru/org-inbox-file)
           "%[~/.emacs.d/org-templates/capture.txt]" :prepend t)
	  ;; ("n" "Note" entry (file aru/org-inbox-file)
	  ;;   "%[~/.emacs.d/org-templates/note.txt]" :prepend t)
	  ;; ("w" "Wiki" entry (file aru/org-wiki-file)
	  ;;   "%[~/.emacs.d/org-templates/capture.txt]" :prepend t)
	  ("b" "Block" entry (file+olp+datetree aru/org-block-file)
	   "* %?"
	   :prepend t
	   :clock-in t)
	  ))
  ;; todo
  (setq org-todo-keywords
	'((sequence "TODO(t)" "PROJECT(p)" "|" "DONE(d!)" "CANCEL(c@)")))

  ;; archive
  (setq org-archive-location "~/org/archive/%s_archive::")

  ;; clocking
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . (lambda () (interactive) (org-capture nil)))))

(use-package ox-html :after org :config (setq org-html-validation-link nil))
(use-package ox-md :after org)
(use-package org-tempo :after org)
(use-package org-habit :after org)
(use-package org-id :after org)

(use-package text-mode
  :hook (text-mode . auto-fill-mode)
  :bind (:map text-mode-map
              ("C-c !" . org-time-stamp-inactive)))

(use-package tex-mode
  :hook (tex-mode . outline-minor-mode))

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
	 ("\\.qmd\\'" . markdown-mode)))

(use-package python
  :config
  (setq python-shell-interpreter "python") ; managed with direnv
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-completion-native-enable nil))

(use-package imenu
  :config
  (setq imenu-auto-rescan t))

(use-package tab-bar
  :hook
  (after-init . tab-bar-history-mode) ; separate window history per tab
  :bind-keymap ("s-t" . tab-prefix-map)	; previously menu-set-font
  :bind (("s-}" . tab-next)
         ("s-{" . tab-previous)
         :map tab-prefix-map
         ("w" . tab-close)))

(use-package calendar
  :config
  (setq calendar-week-start-day 1))     ; start on Mondays

(use-package aru-narrow
  :bind (:map narrow-map
              ("n" . aru-narrow-or-widen-dwim)))

(use-package custom
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes-common-palette-overrides
   '((fringe unspecified))))

(use-package aru-custom
  :config
  (add-hook 'ns-system-appearance-change-functions #'aru-load-theme-auto))

(use-package display-line-numbers
  :bind (("ESC M-l" . display-line-numbers-mode))) ; ESC ESC l

(use-package pdf-tools
  :ensure t
  :init
  (setq pdf-view-use-scaling t)
  (pdf-loader-install))	; sharper text on retina displays

(use-package direnv
  :ensure t
  :diminish
  :config
  (direnv-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s l" . consult-line)))

(use-package eglot
  :config
  (setq eglot-autoshutdown t)
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (latex-mode . eglot-ensure)
   (bibtex-mode . eglot-ensure)))

(use-package treesit
  :init
  ;; following taken from <https://www.masteringemacs.org/article/how-to-get-started-tree-sitter>
  (setq treesit-language-source-alist
	'((python "https://github.com/tree-sitter/tree-sitter-python")
	  (bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (make "https://github.com/alemuller/tree-sitter-make")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	  (latex "https://github.com/latex-lsp/tree-sitter-latex")))
  (setq major-mode-remap-alist
	'((yaml-mode . yaml-ts-mode)
	  (bash-mode . bash-ts-mode)
	  (js2-mode . js-ts-mode)
	  (typescript-mode . typescript-ts-mode)
	  (json-mode . json-ts-mode)
	  (js-json-mode . json-ts-mode)
	  (python-mode . python-ts-mode)
	  (css-mode . css-ts-mode)
	  (html-mode . html-ts-mode)))
  :mode (("\\.yaml'" . yaml-ts-mode)
         ("\\.yml'" . yaml-ts-mode)
	 ("\\.ipynb\\'" . json-ts-mode)))

(use-package mct
  :ensure t
  :config
  (mct-minibuffer-mode 1)
  (mct-region-mode 1))

(use-package orderless
  :ensure t
  :config
  (add-to-list 'completion-styles 'orderless t))

;;; init.el ends here
