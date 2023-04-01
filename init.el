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

;;;; packages
(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "packages/" "use-package/"))
  (require 'use-package))

(setq use-package-compute-statistics t)
(setq use-package-verbose t)

(add-to-list 'load-path (concat user-emacs-directory "aru/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package diminish :ensure t)
(use-package aru-core :demand t)
(use-package cus-edit
  :demand t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

;;;; editing
(use-package eldoc
  :diminish t
  :hook ((after-init . global-eldoc-mode)))

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)))

(use-package elec-pair
  :hook ((after-init . electric-pair-mode)))

;;;; buffers
(use-package autorevert
  :hook  ((after-init . global-auto-revert-mode)))

(use-package uniquify
  :config
  ;; (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package so-long
  :hook ((after-init . global-so-long-mode)))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)))

(use-package outline
  :diminish outline-minor-mode
  :init
  (setq outline-minor-mode-cycle t)
  (setq outline-minor-mode-use-buttons nil)
  :bind (:map outline-minor-mode-map
	      ("C-c C-n" . outline-next-visible-heading)
	      ("C-c C-p" . outline-previous-visible-heading)
	      ("C-c C-f" . outline-forward-same-level)
	      ("C-c C-b" . outline-backward-same-level)
	      ("C-c C-u" . outline-up-heading))
  :hook (prog-mode . outline-minor-mode))

(use-package foldout)

;;;; minibuffer
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

(use-package dirvish
  :ensure t
  :disabled t
  :config
  (dirvish-override-dired-mode))

(use-package winner
  :init
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
	 ("s-]" . (lambda () (interactive) (other-window +1)))
         ("s-[" . (lambda () (interactive) (other-window -1)))
         :map ctl-x-5-map
         ("w" . delete-frame))
  :init
    (setq display-buffer-alist
	'(("\\`\\*Warnings\\*"
	   (display-buffer-no-window))
	  ("\\*help"
	   (display-buffer--maybe-same-window
	    display-buffer-reuse-window
	    display-buffer-reuse-mode-window
	    display-buffer-in-side-window)
	   (side . right)
	   (window-width . 0.25)
           (window-parameters (mode-line-format . " %* %b %p")))
	  ("e?shell"
	   (display-buffer--maybe-same-window
	    display-buffer-reuse-window
	    display-buffer-reuse-mode-window
	    display-buffer-in-side-window)
	   (side . bottom)
	   (slot . -1)
	   (window-height . 0.25)
           (window-parameters (mode-line-format . " %* %b %p")))
	  ("\\*gud"
	   (display-buffer--maybe-same-window
	    display-buffer-reuse-window
	    display-buffer-reuse-mode-window
	    display-buffer-in-side-window)
	   (side . bottom)
	   (slot . -1)
	   (window-height . 0.25)
	   (window-parameters (mode-line-format . " %* %b %p")))
	  ("\\*\\(compilation\\|flymake\\)"
	   (display-buffer--maybe-same-window
	    display-buffer-reuse-window
	    display-buffer-reuse-mode-window
	    display-buffer-in-side-window)
	   (side . bottom)
	   (slot . 0)
	   (window-height . 0.25)
           (window-parameters (mode-line-format . " %* %b %p")))))
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
  :init
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
       ("n" "Note" entry (file+headline aru/org-inbox-file "Inbox")
         "%[~/.emacs.d/org-templates/note.txt]" :prepend t)
       ("w" "Wiki" entry (file aru/org-wiki-file)
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
   ("C-c c" . (lambda () (interactive) (org-capture nil)))))

(use-package aru-ocp :after org)
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

(use-package tex
  :disabled t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

(use-package reftex
  :disabled t
  :config
  (setq reftex-plug-into-AUCTeX t)
  :hook ((LaTeX-mode . turn-on-reftex))) ; turn on with auctex

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode)
          ("README" . gfm-mode)))

(use-package python
  :config
  (setq python-shell-interpreter "python") ; managed with direnv
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq python-shell-completion-native-enable nil)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Python\\*"
                 (display-buffer--maybe-same-window
		  display-buffer-reuse-window
		  display-buffer-reuse-mode-window
		  display-buffer-in-side-window)
		 (side . bottom)
		 (slot . -1)
		 (window-height . 0.25)
                 (window-parameters (mode-line-format . " %* %b %p")))))

(use-package pyvenv :ensure t :disabled t :after python)

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
  (setq tab-bar-close-last-tab-choice nil)
  (setq tab-bar-close-tab-select 'recent)
  (setq tab-bar-new-tab-choice t)
  (setq tab-bar-new-tab-to 'right)
  (setq tab-bar-position t)             ; show tab-bar below tool-bar
  (setq tab-bar-show t)                 ; always show tab-bar
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)
  (setq tab-bar-format
        '(tab-bar-format-tabs-groups
          tab-bar-separator
          tab-bar-format-align-right
          tab-bar-format-global))

  (tab-bar-mode +1)
  (tab-bar-history-mode +1)             ; separate window history per tab

  :bind-keymap ("s-t" . tab-prefix-map)
  :bind (("s-}" . tab-next)
         ("s-{" . tab-previous)
         :map tab-prefix-map
              ("w" . tab-close)))

(use-package time
  :config
  (setq display-time-24hr-format nil)
  (setq display-time-day-and-date nil)
  (setq display-time-format "%a %e %b, %H:%M")
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

(use-package calendar
  :config
  (setq calendar-week-start-day 1))     ; start on Mondays

(use-package aru-narrow
  :bind (:map narrow-map
              ("n" . aru-narrow-or-widen-dwim)))

(use-package icomplete
  :disabled t
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

(use-package emacs
  :init
  (setq modus-themes-italic-constructs t
	  modus-themes-bold-constructs t
	  modus-themes-org-blocks 'tinted-background
    modus-themes-common-palette-overrides
    '((fringe unspecified))))

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

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("s-b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key (kbd "M-.")
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package highlight-indent-guides
  :disabled t
  :ensure t
  :diminish
  :init
  (setq highlight-indent-guides-method 'bitmap)
  :hook ((prog-mode . highlight-indent-guides-mode)
         (text-mode . highlight-indent-guides-mode)))

(use-package corfu
  :ensure t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package citar
  :disabled t
  :ensure t
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/org/papers.bib"))
  (citar-library-paths '("~/Documents/papers")))

(use-package citar-embark
  :disabled t
  :diminish
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package editorconfig
  :ensure t
  :diminish
  :init
  (editorconfig-mode 1))

(use-package lsp-mode
  :disabled t
  :ensure t
  ;; :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (setq lsp-keymap-prefix "C-c l")
  :hook
  ((python-mode . lsp)
    (latex-mode . lsp)
    (bibtex-mode . lsp)
    (markdown-mode . lsp))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :disabled t
  :ensure t
  :commands lsp-ui-mode)
;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

; (use-package tree-sitter
;   :ensure t
;   :init
;   (global-tree-sitter-mode)
;   :hook
;   (tree-sitter-after-on . tree-sitter-hl-mode))

; (use-package tree-sitter-langs
;   :ensure t)

(use-package doom-modeline
  :disabled t
  :ensure t
  :hook ((after-init . doom-modeline-mode)))

(use-package eglot
  :hook
  ((python-mode . eglot-ensure)
    (python-ts-mode . eglot-ensure)
    (latex-mode . eglot-ensure)
    (bibtex-mode . eglot-ensure)))

(use-package emacs
  :config
  (add-to-list 'treesit-extra-load-path (concat user-emacs-directory "tree-sitter-module/" "dist/"))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode)))

;;; init.el ends here
