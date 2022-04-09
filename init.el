;; init.el -- Personal Emacs Config --   -*- lexical-binding: t; -*-

(add-to-list 'load-path (concat user-emacs-directory "aru/"))

(require 'aru-packages)

(use-package blackout :straight (:host github :repo "raxod502/blackout") :demand t)
(use-package gcmh :straight t :config (gcmh-mode +1) :blackout t)
(use-package no-littering :straight t :demand t)
(use-package org :straight (:host github :repo "bzg/org-mode")) ; initialise early, config later

(use-package aru-core :demand t)
(use-package aru-cus-edit :demand t)

(use-package delsel     :hook (after-init . delete-selection-mode))
(use-package hl-line    :config (global-hl-line-mode nil))
(use-package hi-lock    :blackout t)
(use-package novice     :config (setq disabled-command-function nil))
(use-package saveplace  :config (save-place-mode +1))
(use-package ibuffer    :bind (([remap list-buffers] . #'ibuffer)))
(use-package vc-hooks   :config (setq vc-follow-symlinks t))
(use-package autorevert :config (global-auto-revert-mode +1))
(use-package uniquify   :config (setq uniquify-buffer-name-style 'forward))
(use-package elec-pair  :config (electric-pair-mode +1))
(use-package man        :config (setq Man-switches "-a"))
(use-package ffap       :config (ffap-bindings))
(use-package emacs      :bind (("C-h h" . nil)))
(use-package re-builder :config (setq reb-re-syntax 'read))
(use-package hydra      :straight t)
(use-package help-at-pt :config (setq help-at-pt-display-when-idle 'always))
(use-package emacs      :config (setq narrow-to-defun-include-comments t))
(use-package newcomment :bind (("s-;" . comment-line)))
(use-package so-long    :config (global-so-long-mode 1))

(use-package text-mode
  :hook (text-mode . auto-fill-mode)
  :bind (:map text-mode-map
              ("C-c !" . org-time-stamp-inactive)))

(use-package olivetti
  :straight t
  :blackout t
  :init                                 ; changes require restart
  (setq olivetti-body-width 80)
  (setq olivetti-body-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(use-package org-tree-slide
  :straight t
  :blackout t
  :config
  (setq org-tree-slide-breadcrumbs t)
  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-heading-emphasis t)
  (setq org-tree-slide-cursor-init t)
  (setq org-tree-slide-modeline-display nil)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-skip-comments t)
  (setq org-tree-slide-fold-subtrees-skipped t)
  (setq org-tree-slide-skip-outline-level 8)
  (setq org-tree-slide-never-touch-face t)
  (setq org-tree-slide-activate-message
        (format "Presentation %s" (propertize "ON" 'face 'success)))
  (setq org-tree-slide-deactivate-message
        (format "Presentation %s" (propertize "OFF" 'face 'error)))
  :bind (:map org-tree-slide-mode-map
              ("<down>" . org-tree-slide-display-header-toggle)
              ("<right>" . org-tree-slide-move-next-tree)
              ("<left>" . org-tree-slide-move-previous-tree)))

(use-package aru-focus
  :config
  (setq aru-focus-org-presentation t)
  (setq aru-focus-hidden-modeline t))

(use-package eldoc
  :blackout t
  :config (global-eldoc-mode 1))

(use-package wgrep
  :straight t
  :commands (wgrep)
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t)
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

(use-package isearch
  :blackout t
  :config
  (setq isearch-lazy-highlight t)
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil)
  (setq isearch-yank-on-move 'shift)
  (setq isearch-allow-scroll 'unlimited)
  (setq lazy-highlight-initial-delay 0)
  (setq search-highlight t)
  (setq search-whitespace-regexp ".*?")
  (setq isearch-lax-whitespace t)
  (setq isearch-regexp-lax-whitespace nil))

(use-package outline
  :blackout outline-minor-mode
  :config
  (setq outline-minor-mode-prefix "\C-z")
  (setq outline-minor-mode-cycle t)
  :hook (prog-mode . outline-minor-mode))

(use-package foldout :after outline)

(use-package flyspell
  :blackout t
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-dictionary "en_GB")
  :bind (("ESC M-s" . flyspell-mode)	; ESC ESC s
	 :map flyspell-mode-map
	 ("C-." . nil)))

(use-package vc-dispatcher
  :config
  (setq vc-suppress-confirm t)
  (setq vc-command-messages t))

(use-package tex-mode
  :config
  (setq latex-run-command "pdflatex -interaction=nonstopmode")
  (setq tex-dvi-view-command "open")
  (setq tex-print-file-extension ".pdf")
  :hook (latex-mode . outline-minor-mode))

(use-package frame
  :config (blink-cursor-mode 0)
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)))

(use-package dired
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-FAlrh")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  :hook ((dired-mode . dired-hide-details-mode)))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t))

(use-package dired-x
  :bind (("s-j" . dired-jump)))

(use-package winner
  :config
  (winner-mode +1)
  :bind (("s-<left>"  . winner-undo)    ; previously ns-prev-frame
         ("s-<right>" . winner-redo)))  ; previously ns-next-frame

(use-package window
  :bind-keymap (("s-4" . ctl-x-4-map)
                ("s-5" . ctl-x-5-map))
  :bind (("s-f" . find-file)
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
  ;; adapted from elisp manual
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layouts-with-Side-Windows.html
  (defvar parameters
    '(window-parameters . ((no-other-window . t))))
  (setq fit-window-to-buffer-horizontally t)
  (setq window-resize-pixelwise t)
  (setq window-min-width fill-column)
  (setq display-buffer-alist
        ;; top
        `(("\\*\\(Buffer List\\|Ibuffer\\)\\*"
           display-buffer-in-side-window
           (side . top)
           (slot . 0)
           (window-height . fit-window-to-buffer)
           (preserve-size . (nil . t))
           ,parameters)
          ;; left
          ("\\*\\(Tags List\\|Completions\\)\\*"
           display-buffer-in-side-window
           (side . right)
           (slot . 0)
           (window-width . fit-window-to-buffer)
           (preserve-size . (t . nil))
           ,parameters)
          ;; bottom left
          ("\\*\\(?:help\\|grep\\|Occur\\|xref\\)\\*"
           display-buffer-in-side-window
           (side . bottom)
           (slot . -1)
           (preserve-size . (nil . t))
           ,parameters)
          ;; bottom right
          ("\\*\\(compilation\\|Warning\\|Backtrace\\|Async Shell Command\\)\\*"
           display-buffer-in-side-window
           (side . bottom)
           (slot . 1)
           (preserve-size . (nil . t))
           ,parameters)
          ("\\*.*\\(e?shell\\|v?term\\).*"
           display-buffer-in-side-window
           (side . bottom)
           (slot . 1)
           (preserve-size . (nil . t))
           ,parameters))))

(use-package aru-window
  :disabled t
  :bind (("s-o" . aru-window-other-window-dwim)
         ("s-2" . aru-window-split-window-below)
         ("s-3" . aru-window-split-window-right)
         ("s-w" . aru-window-delete-window)))

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
  (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package simple                     ; case bindings for active region
  :blackout ((visual-line-mode)
             (auto-fill-mode))
  :bind
  (("M-c" . capitalize-dwim)
   ("M-l" . downcase-dwim)
   ("M-u" . upcase-dwim)
   ("s-n" . next-error)
   ("s-p" . previous-error)
   ("M-SPC" . cycle-spacing)           ; previously just-one-space
   ("M-Q" . delete-indentation)
   ("ESC M-g" . window-toggle-side-windows)) ; ESC ESC g
  :config
  (setq kill-do-not-save-duplicates t)
  (setq async-shell-command-display-buffer nil)
  (setq shell-command-prompt-show-cwd t)
  (column-number-mode +1)
  (indent-tabs-mode -1))               ; show line and column numbers

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

(use-package consult
  :straight t
  :bind (("s-b" . consult-buffer)
         ("C-." . consult-outline)
         :map ctl-x-map                    ; C-x bindings
         ("M-:" . consult-complex-command) ; [default] repeat-complex-command
         ("b" . consult-buffer)            ; [default] switch-to-buffer
         :map ctl-x-4-map
         ("b" . consult-buffer-other-window) ; [default] switch-to-buffer-other-window
         :map ctl-x-5-map
         ("b" . consult-buffer-other-frame)  ; [default] switch-to-buffer-other-frame
         :map help-map
         ("a" . consult-apropos)
         :map search-map
         ("f" . consult-find)
         ("L" . consult-locate)
         ("g" . consult-grep)
         ("G" . consult-git-grep)
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("m" . consult-multi-occur))
  :config
  (setq consult-project-root-function #'vc-root-dir))

(use-package modus-themes
  :straight t                           ; install from source, don't use built-in version
  :commands (load-theme)
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-syntax '(alt-syntax)
        modus-themes-completions 'moderate
        modus-themes-hl-line '(accented)
        modus-themes-paren-match '(intense)
        modus-themes-org-blocks 'tinted-background
        modus-themes-mode-line '(moody accented)
        modus-themes-links '(faint neutral-underline)
        modus-themes-prompts '(intense bold)
        modus-themes-tabs-accented t
        modus-themes-lang-checkers '(straight-underline faint)
        modus-themes-subtle-line-numbers t
        modus-themes-headings '((1 . (1.3))
                                (2 . (1.2))
                                (3 . (1.1))))
  :bind (("ESC M-t" . modus-themes-toggle))) ; ESC ESC t

(use-package aru-custom
  :hook (after-init . (lambda () (aru-load-theme 'modus-operandi))))

(use-package magit
  :straight t
  :bind (:map ctl-x-map
              ("g" . magit-status)))

(use-package whitespace
  :commands
  (whitespace-buffer
   whitespace-cleanup
   whitespace-mode
   whitespace-turn-off)
  :bind (("ESC M-w" . whitespace-mode))	; ESC ESC w
  :hook ((prog-mode . (lambda () (setq show-trailing-whitespace t))))
  :blackout t)

(use-package org
  :hook (org-mode . (lambda () (electric-indent-local-mode -1))) ; do not auto indent in org buffers
  :config
  ;;; general
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
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▼ ")
  (setq org-default-notes-file aru/org-inbox-file)
  ;;; refile
  (setq org-refile-targets '((nil . (:maxlevel . 2))
                             (org-agenda-files . (:maxlevel . 2))))
  ;;; agenda
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-files (expand-file-name "org-agenda-files.org" org-directory))
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-ignore-drawer-properties '(effort appt category stats))
  (setq org-agenda-custom-commands
        '(("o" "List of all Open TODO entries"
           ((todo ""
                  ((org-agenda-overriding-header "\nUnscheduled TODO")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))))))
  ;;; babel
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
				 (python     . t)
				 (shell      . t)))
  ;;; capture
  (setq org-capture-templates
	'(("p" "Paper" entry (file+headline aru/org-inbox-file "Inbox")
           "%[~/.emacs.d/org-templates/paper.txt]" :prepend t)
          ("c" "Capture" entry (file+headline aru/org-inbox-file "Inbox")
           "%[~/.emacs.d/org-templates/capture.txt]" :prepend t)))

  ;; todo
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAITING(w@/!)" "LATER(l)" "|" "DONE(d!)" "CANCEL(c@)")))
  (setq org-todo-keyword-faces
	'(("WAITING" :inherit default :weight bold)
	  ("LATER" :inherit warning :weight bold)))
  ;; archive
  (setq org-archive-location "~/org/archive/%s_archive::")
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . (lambda () (interactive) (org-capture nil)))
   ("C-c t" . (lambda () (interactive) (find-file aru/org-inbox-file)))
   ("C-c d" . (lambda () (interactive) (dired org-directory)))
   :map ctl-x-4-map
   ("C-c t" . (lambda () (interactive) (find-file-other-window aru/org-inbox-file)))
   ("C-c d" . (lambda () (interactive) (dired-other-window org-directory)))
   :map ctl-x-5-map
   ("C-c t" . (lambda () (interactive) (find-file-other-frame aru/org-inbox-file)))
   ("C-c d" . (lambda () (interactive) (dired-other-frame org-directory)))))

(use-package aru-ocp :after org)
(use-package ox-html :after org :config (setq org-html-validation-link nil))
(use-package ox-md :after org)
(use-package org-tempo :after org)
(use-package org-habit :after org)
(use-package org-id :after org)

(use-package em-hist
  :bind (:map eshell-hist-mode-map      ; overrides for windmove
              ("<up>" . nil)
              ("<down>" . nil)))

(use-package eshell
  :after em-hist
  :config
  (setq eshell-prefer-lisp-functions t)
  (setq eshell-expand-input-functions '(eshell-expand-history-references))
  (setq eshell-cd-on-directory t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-save-history-on-exit t)
  (setq eshell-banner-message
	'(format "%s %s\n"
		 (propertize (format " %s " (string-trim (buffer-name)))
			     'face 'mode-line-highlight)
		 (propertize (current-time-string)
			     'face 'font-lock-keyword-face))
	eshell-kill-processes-on-exit t))

(use-package time
  :config
  (setq display-time-24hr-format nil)
  (setq display-time-day-and-date nil)
  (setq display-time-format "%H:%M %a %b %d")
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

(use-package markdown-mode
  :straight t
  :init (setq markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package mwheel
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse t))     ; default

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python")
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv :straight t :after python)

(use-package rust-mode
  :straight t
  :mode ("\\.rs\\'" . rust-mode)
  :interpreter "rust")

(use-package imenu
  :config
  (setq imenu-use-markers t)
  (setq imenu-auto-rescan t)
  (setq imenu-auto-rescan-maxout 600000)
  (setq imenu-max-item-length 100)
  (setq imenu-use-popup-menu nil)
  (setq imenu-eager-completion-buffer t)
  (setq imenu-space-replacement " ")
  (setq imenu-level-separator "/"))

(use-package flimenu
  :straight t
  :after imenu
  :config (flimenu-global-mode 1))

(use-package dabbrev
  :after (minibuffer)
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t))

(use-package moody
  :straight t
  :custom
  (moody-mode-line-height 20)		; works best for 12pt font
  :config
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

(use-package subword
  :blackout t
  :hook (prog-mode . subword-mode))

(use-package transpose-frame
  :straight t
  :commands (transpose-frame
             flip-frame
             flop-frame
             rotate-frame
             rotate-frame-clockwise
             rotate-frame-anticlockwise)
  :bind (("C-s-t" . flop-frame)
         ("C-s-r" . rotate-frame-clockwise)))

(use-package ibuffer-vc
  :straight t
  :after (ibuffer vc)
  :bind (:map ibuffer-mode-map
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)))

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

  (tab-bar-mode 1)
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

(use-package pdf-tools
  :straight t
  :magic ("%PDF" . pdf-view-mode)
  :config
  (setq pdf-tools-enabled-modes '(pdf-history-minor-mode
                                  pdf-isearch-minor-mode
                                  pdf-links-minor-mode
                                  pdf-outline-minor-mode
                                  pdf-misc-size-indication-minor-mode
                                  pdf-occur-global-minor-mode))
  (setq pdf-view-display-size 'fit-height)
  (setq pdf-view-continuous t)
  (setq pdf-view-use-dedicated-register nil)
  (setq pdf-view-max-image-width 1080)
  (setq pdf-outline-imenu-use-flat-menus t)
  (pdf-loader-install)
  :bind (:map pdf-view-mode-map            ; override for windmove
              ("<up>" . nil)
              ("<down>" . nil)))

(use-package shr
  :config
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  (setq shr-max-image-proportion 0.75)
  (setq shr-image-animate nil))
