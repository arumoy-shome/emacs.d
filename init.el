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

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "packages/" "use-package/"))
  (require 'use-package))

(setq use-package-compute-statistics t)
(setq use-package-verbose t)

(add-to-list 'load-path (concat user-emacs-directory "aru/"))

(use-package package
  :demand t
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package aru-core
  :demand t)

(use-package cus-edit
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file)
  :demand t)

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package autorevert
  :config
  (global-auto-revert-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package so-long
  :config
  (global-so-long-mode 1))

(use-package ibuffer
  :bind (([remap list-buffers] . #'ibuffer)))

(use-package savehist
  :init
  (savehist-mode +1))

(use-package outline
  :init
  ;; I used to use `outline-minor-mode-cycle' but found it to be
  ;; buggy. First it would randomly obliterate the entire keymap
  ;; associated with TAB (I was never able to debug this) and second
  ;; it would get confused between outline-headers and regular code (I
  ;; noticed this problem in python files mostly). The alternative is
  ;; to assign a nicer prefix for outline-minor-mode (the default C-c
  ;; @ is super clunky).
  (setq outline-minor-mode-prefix "\C-z")
  (setq outline-minor-mode-cycle t)
  (setq outline-minor-mode-use-buttons nil)
  :bind (:map outline-minor-mode-map
	      ("C-c C-n" . outline-next-visible-heading)
	      ("C-c C-p" . outline-previous-visible-heading)
	      ("C-c C-f" . outline-forward-same-level)
	      ("C-c C-b" . outline-backward-same-level)
	      ("C-c C-u" . outline-up-heading))
  :hook (prog-mode . outline-minor-mode))

(use-package foldout :after outline)

(use-package flyspell
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-dictionary "en_GB")
  :bind (("ESC M-s" . flyspell-mode)	; ESC ESC s
	 :map flyspell-mode-map
	 ("C-." . nil)))

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

(use-package winner
  :config
  (winner-mode +1)
  :bind (("s-<left>"  . winner-undo)    ; previously ns-prev-frame
         ("s-<right>" . winner-redo)))  ; previously ns-next-frame

(use-package frame
  :bind (("C-z" . nil)))		; use C-x C-z instead

(use-package window
  :bind-keymap (("s-4" . ctl-x-4-map)
                ("s-5" . ctl-x-5-map))
  :bind (("s-f" . find-file)
	 ("s-b" . switch-to-buffer)
         ("s-1" . delete-other-windows)
         ("s-h" . previous-buffer)      ; previously ns-do-hide-emacs
         ("s-l" . next-buffer)  ; previously goto-line, use M-g g instead
	 ("s-2" . split-window-below)
	 ("s-3" . split-window-right)
	 ("s-w" . delete-window)
	 ("s-o" . other-window)		; previously ns-open-file-using-panel
         :map ctl-x-5-map
         ("w" . delete-frame))
  :config
  (setq fit-window-to-buffer-horizontally t)
  (setq window-resize-pixelwise t)
  (setq window-min-width fill-column)
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
  (show-paren-mode +1))

(use-package files
  :config
  (setq confirm-kill-processes nil)
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (setq find-file-visit-truename t)
  (setq find-file-suppress-same-file-warnings t)
  (setq require-final-newline t)
  :bind (("s-q" . nil)))

(use-package recentf
  :config
  (recentf-mode +1))

(use-package simple                     ; case bindings for active region
  :bind
  (("M-c" . capitalize-dwim)
   ("M-l" . downcase-dwim)
   ("M-u" . upcase-dwim)
   ("s-n" . next-error)
   ("s-p" . previous-error)
   ("M-SPC" . cycle-spacing)           ; previously just-one-space
   ("M-Q" . delete-indentation)
   ("ESC M-v" . visual-line-mode)	; ESC ESC v
   ("ESC C-M-i" . indent-tab-mode))	; ESC ESC TAB
  :config
  (setq kill-do-not-save-duplicates t)
  (setq async-shell-command-display-buffer nil)
  (setq shell-command-prompt-show-cwd t)
  (column-number-mode +1))               ; show line and column numbers

(use-package magit
  :ensure t
  :bind (:map ctl-x-map
              ("g" . magit-status)))

(use-package whitespace
  :bind (("ESC M-w" . whitespace-mode))	; ESC ESC w
  :config
  (setq show-trailing-whitespace t))

(use-package org
  :hook ((org-mode . (lambda () (electric-indent-local-mode -1))) ; do not auto indent in org buffers
	 (org-capture-mode . org-id-get-create)	; add ID automatically to all captured items
	 (org-mode . (lambda () (setq-default indent-tabs-mode nil)))) ; do not use tabs for indent
  :config
  ;; general
  (org-indent-mode -1)                  ; [default] do not indent text based on outline
  (setq org-src-window-setup 'split-window-below)
  (setq org-startup-folded t)
  (setq org-reverse-note-order t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-goto-auto-isearch nil)
  (setq org-hide-block-startup t)
  (setq org-return-follows-link nil)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-directory "~/org")
  (defconst aru/org-inbox-file (expand-file-name "aru.org" org-directory)
    "File to use for capturing org items.")
  (defconst aru/org-block-file (expand-file-name "blocks.org" org-directory)
    "File to use for capturing work blocks.")
  (setq org-log-into-drawer t)
  (setq org-default-notes-file aru/org-inbox-file)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  ;; refile
  (setq org-refile-targets '((nil . (:maxlevel . 2))
                             (org-agenda-files . (:maxlevel . 2))))
  ;; agenda
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-files (expand-file-name "org-agenda-files.org" org-directory))
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-ignore-drawer-properties '(effort appt category stats))

  ;; babel
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
				 (python     . t)
				 (shell      . t)))
  ;; capture
  (setq org-capture-templates
	'(("p" "Paper" entry (file+headline aru/org-inbox-file "Inbox")
           "%[~/.emacs.d/org-templates/paper.txt]" :prepend t)
          ("c" "Capture" entry (file+headline aru/org-inbox-file "Inbox")
           "%[~/.emacs.d/org-templates/capture.txt]" :prepend t)
	  ("b" "Block" entry (file+olp+datetree aru/org-block-file)
	   ""				; empty template
	   :prepend t
	   :immediate-finish t
	   :clock-in t)))
  ;; todo
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)" "CANCEL(c@)")))

  ;; archive
  (setq org-archive-location "~/org/archive/%s_archive::")
  ;; clocking
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . (lambda () (interactive) (org-capture nil)))
   ("C-c d" . (lambda () (interactive) (dired org-directory)))
   :map ctl-x-4-map
   ("C-c d" . (lambda () (interactive) (dired-other-window org-directory)))
   :map ctl-x-5-map
   ("C-c d" . (lambda () (interactive) (dired-other-frame org-directory)))
   :map org-mode-map
   ("C-." . org-goto)))			; override imenu in org-mode

(use-package aru-ocp :after org)
(use-package ox-html :after org :config (setq org-html-validation-link nil))
(use-package ox-md :after org)
(use-package org-tempo :after org)
(use-package org-habit :after org)
(use-package org-id :after org)

(use-package time
  :config
  (setq display-time-24hr-format nil)
  (setq display-time-day-and-date nil)
  (setq display-time-format "%H:%M %a %b(%m) %d")
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

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
         ("\\.markdown\\'" . markdown-mode)))

(use-package python
  :config
  (setq python-shell-interpreter "python3")
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv :ensure t :after python)

(use-package imenu
  :config
  (setq imenu-use-markers t)
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 600000)
  (setq imenu-max-item-length 100)
  (setq imenu-use-popup-menu nil)
  (setq imenu-eager-completion-buffer t)
  (setq imenu-space-replacement " ")
  (setq imenu-level-separator "/")
  :bind (("C-." . imenu)))

(use-package tab-bar
  :config
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position t)             ; show tab-bar below tool-bar
  (setq tab-bar-show 1)                 ; only show when more than 1 tab open
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode +1)
  (tab-bar-history-mode -1)             ; separate window history per tab

  :bind-keymap ("s-t" . tab-prefix-map)
  :bind (:map tab-prefix-map
              ("w" . tab-close)))

(use-package calendar
  :config
  (setq calendar-week-start-day 1))     ; start on Mondays

(use-package aru-narrow
  :bind (:map narrow-map
              ("n" . aru-narrow-or-widen-dwim)))

(use-package icomplete
  :demand t		       ; need this since I am using :bind here
  :config
  (fido-vertical-mode +1)
  :bind (:map icomplete-vertical-mode-minibuffer-map
	      ;; the usual RET doesn't work when using commands that
	      ;; accept multiple arguments (and expect an empty string
	      ;; to exit the completion). I map RET to
	      ;; icomplete-fido-exit instead which exits the
	      ;; completion with an empty string.

	      ;; The downside is that the first argument is no longer
	      ;; selected automatically, which is the default
	      ;; behaviour. To make this a bit easier, I bind TAB to
	      ;; icomplete-force-complete which completes using the
	      ;; first entry. This is better than the default
	      ;; behaviour (minibuffer-complete) which opens the
	      ;; *Completions* buffer.
	      ("RET" . icomplete-fido-exit)
	      ("TAB" . icomplete-force-complete)))

(use-package doc-view
  :config
  (setq doc-view-resolution 180)
  :bind (:map doc-view-mode-map	; rebind windmove since doc-view overrides them
	      ("<left>" . windmove-left)
	      ("<right>" . windmove-right)
	      ("<up>" . windmove-up)
	      ("<down>" . windmove-down)
	      ("S-<left>" . windmove-swap-states-left)
	      ("S-<right>" . windmove-swap-states-right)
	      ("S-<up>" . windmove-swap-states-up)
	      ("S-<down>" . windmove-swap-states-down)))

(use-package modus-themes
  :init
  :disabled t
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-org-blocks 'tinted-background
	modus-themes-tabs-accented t
	modus-themes-intense-mouseovers t
	modus-themes-mode-line '(borderless accented)
	modus-themes-markup '(intense)
	modus-themes-syntax '(alt-syntax)
	modus-themes-hl-line '(intense)
	modus-themes-paren-match '(intense)
	modus-themes-links '(faint)
	modus-themes-prompts '(intense)
	modus-themes-lang-checkers '(straight-underline faint)
	modus-themes-headings
	(quote ((1 . (light 1.5))
		(2 . (light 1.4))
		(3 . (light 1.3))
		(4 . (light 1.2))
		(t . (light 1.1))))))

(use-package ef-themes
  :ensure t
  :init
  :bind (("ESC M-t" . ef-themes-toggle))) ; ESC ESC t

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (doom-themes-org-config))

(use-package aru-custom
  :config
  (aru-load-theme 'ef-day))

(use-package display-line-numbers
  :bind (("ESC M-l" . display-line-numbers-mode))) ; ESC ESC l

(use-package term
  :bind (:map term-mode-map ; rebind windmove since doc-view overrides them
	      ("<left>" . windmove-left)
	      ("<right>" . windmove-right)
	      ("<up>" . windmove-up)
	      ("<down>" . windmove-down)
	      ("S-<left>" . windmove-swap-states-left)
	      ("S-<right>" . windmove-swap-states-right)
	      ("S-<up>" . windmove-swap-states-up)
	      ("S-<down>" . windmove-swap-states-down))
  :hook (term-mode . visual-line-mode))

(use-package shell
  :hook (shell-mode . visual-line-mode))

(use-package pdf-tools
  :ensure t
  :config
  (setq pdf-view-use-scaling t))	; sharper text on retina displays

;;; init.el ends here
