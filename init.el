;; personal emacs config

(eval-when-compile
  (add-to-list 'load-path (concat user-emacs-directory "packages/" "use-package/"))
  (require 'use-package))

(setq use-package-compute-statistics t)
(setq use-package-verbose t)

(add-to-list 'load-path (concat user-emacs-directory "aru/"))

(use-package aru-core
  :demand t)

(use-package aru-cus-edit
  :demand t)

(use-package aru-custom
  :config
  (aru-load-theme 'modus-operandi))

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

(use-package outline
  :config
  (setq outline-minor-mode-prefix "\C-z")
  (setq outline-minor-mode-cycle t)
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
  (setq window-min-width fill-column))

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
   ("M-Q" . delete-indentation))
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
  (defconst aru/org-block-file (expand-file-name "blocks.org" org-directory)
    "File to use for capturing work blocks.")
  (setq org-log-into-drawer t)
  ;; (setq org-ellipsis " ▼ ")
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
           "%[~/.emacs.d/org-templates/capture.txt]" :prepend t)
	  ("b" "Block" entry (file+olp+datetree aru/org-block-file)
	   ""				; empty template
	   :prepend t
	   :immediate-finish t
	   :clock-in t)))

  ;;; todo
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)" "CANCEL(c@)")))

  ;;; archive
  (setq org-archive-location "~/org/archive/%s_archive::")
  ;;; clocking
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
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

(use-package time
  :config
  (setq display-time-24hr-format nil)
  (setq display-time-day-and-date nil)
  (setq display-time-format "%H:%M %a %b %d")
  (setq display-time-interval 60)
  (setq display-time-mail-directory nil)
  (setq display-time-default-load-average nil)
  :hook (after-init . display-time-mode))

(use-package text-mode
  :hook (text-mode . auto-fill-mode)
  :bind (:map text-mode-map
              ("C-c !" . org-time-stamp-inactive)))

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "multimarkdown")
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

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
  :config
  (icomplete-vertical-mode +1)
  (fido-mode +1))

(use-package doc-view
  :config
  (setq doc-view-resolution 180))

(use-package modus-themes
  :bind (("ESC M-t" . modus-themes-toggle))) ; ESC ESC t
