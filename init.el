;; startup optimization: set `gc-cons-threshold' to a high value now
;; and reset it later
(defvar aru/orig-gc-cons-threshold gc-cons-threshold
  "Original value of `gc-cons-threshold.'")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold aru/orig-gc-cons-threshold)))

(add-to-list 'load-path (concat user-emacs-directory "aru/"))

(require 'aru-packages)

(use-package blackout :straight (:host github :repo "raxod502/blackout") :demand t)
(use-package gcmh :straight t :config (gcmh-mode +1) :blackout t)
(use-package no-littering :straight t :demand t)

(use-package aru-core :demand t)
(use-package aru-path :demand t)
(use-package aru-windows
  :bind (:map ctl-x-map (("C-\\" . aru/split-window-horizontally-instead)
                         ("C--" . aru/split-window-vertically-instead)))) ; use s-- instead

(use-package org :straight t)

(use-package menu-bar :config (menu-bar-mode -1))
(use-package tool-bar :config (tool-bar-mode -1))
(use-package tooltip  :config (tooltip-mode -1))
(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(use-package delsel     :config (setq delete-selection-mode t))
(use-package hl-line    :config (global-hl-line-mode nil))
(use-package novice     :config (setq disabled-command-function nil))
(use-package isearch    :config (setq lazy-highlight-initial-delay 0))
(use-package saveplace  :config (save-place-mode +1))
(use-package ibuffer    :bind (([remap list-buffers] . #'ibuffer)))
(use-package winner     :config (winner-mode +1))
(use-package text-mode  :hook (text-mode	. auto-fill-mode))
(use-package vc-hooks   :config (setq vc-follow-symlinks t))
(use-package autorevert :config (global-auto-revert-mode +1))
(use-package uniquify   :config (setq uniquify-buffer-name-style 'forward))
(use-package elec-pair  :config (electric-pair-mode +1))
(use-package man        :config (setq Man-switches "-a"))
(use-package ffap       :config (ffap-bindings))
(use-package emacs      :bind (("C-h h" . nil)))
(use-package hippie-exp :bind (("M-/" . hippie-expand)))
(use-package eldoc      :blackout t)
(use-package isearch    :blackout t)

(use-package outline
  :blackout outline-minor-mode
  :bind (("C-c C-z a" . outline-show-all)
         ("C-c C-z b" . outline-backward-same-level)
         ("C-c C-z c" . outline-hide-entry)
         ("C-c C-z d" . outline-hide-subtree)
         ("C-c C-z e" . outline-show-entry)
         ("C-c C-z f" . outline-forward-same-level)
         ("C-c C-z <tab>" . outline-show-children)
         ("C-c C-z k" . outline-show-branches)
         ("C-c C-z l" . outline-hide-leaves)
         ("C-c C-z <return>" . outline-insert-heading)
         ("C-c C-z n" . outline-next-visible-heading)
         ("C-c C-z o" . outline-hide-other)
         ("C-c C-z p" . outline-previous-visible-heading)
         ("C-c C-z q" . outline-hide-sublevels)
         ("C-c C-z s" . outline-show-subtree)
         ("C-c C-z t" . outline-hide-body)
         ("C-c C-z u" . outline-up-heading)
         ("C-c C-z <down>" . outline-move-subtree-down)
         ("C-c C-z <up>" . outline-move-subtree-up)
         ("C-c C-z @" . outline-mark-subtree)
         ("C-c C-z <left>" . outline-promote)
         ("C-c C-z <right>" . outline-demote)))

(use-package project
  :bind (("C-c p f" . project-find-file)
         ("C-c p g" . project-find-regexp)
         ("C-c p s" . project-search)
         ("C-c p %" . project-query-replace-regexp)))

(use-package flyspell
  :blackout t
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package hideshow                   ; I follow the vim pneumonic for the keybindings
  :hook (prog-mode . (lambda () (hs-minor-mode +1)))
  :blackout hs-minor-mode
  :bind (("C-c z o" . hs-show-block)      ; "o" as in open
         ("C-c z c" . hs-hide-block)      ; "c" as in close
         ("C-c z a" . hs-toggle-hiding)   ; "a" as in alternate
         ("C-c z m" . hs-hide-all)        ; "m" as in more
         ("C-c z r" . hs-show-all)))      ; "r" as in reduce

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

(use-package dired-aux
  :config
  (setq dired-isearch-filenames t)
  (setq dired-create-destination-dirs 'ask))

(use-package dired-x
  :bind (:map ctl-x-map ("C-j" . dired-jump)))

(use-package window
  :bind (("s-]" . other-window)
         ("s-[" . (lambda () (interactive) (other-window -1)))
         ("s-3" . (lambda () (interactive) (split-window-right)
                    (other-window 1)))
         ("s-2" . (lambda () (interactive) (split-window-below)
                    (other-window 1)))
         ("s-1" . delete-other-windows)
         ("s-w" . delete-window)
         ("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("s-f" . find-file)
         ("s-F" . find-file-other-window)))

(use-package windmove
  :bind
  (("<left>"  . windmove-left)
   ("<right>" . windmove-right)
   ("<up>"    . windmove-up)
   ("<down>"  . windmove-down)))

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
  (setq require-final-newline t))

(use-package cus-edit
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (require 'custom)))

(use-package recentf
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (defun aru/recentf-find-file ()
    "Taken from
    https://github.com/raxod502/selectrum/wiki/Useful-Commands#switch-to-recent-file"
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))
  (defun aru/recentf-find-file-other-window ()
    "Like aru/recentf-find-file but in other window."
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file-other-window (completing-read "Find recent file: " files nil t))))
  :bind (("s-r" . aru/recentf-find-file)
         ("s-R" . aru/recentf-find-file-other-window)))

(use-package simple                     ; case bindings for active region
  :blackout ((visual-line-mode)
             (auto-fill-mode))
  :bind
  (("M-c" . capitalize-dwim)
   ("M-l" . downcase-dwim)
   ("M-u" . upcase-dwim)
   ("s-p" . execute-extended-command))  ; alternative for M-x
  :config
  (setq kill-do-not-save-duplicates t)
  (setq async-shell-command-display-buffer nil)
  (setq shell-command-prompt-show-cwd t)
  (column-number-mode +1))              ; show line and column numbers

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

(use-package modus-operandi-theme
  :straight t
  :commands (load-theme)
  :config
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-proportional-fonts nil
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-scale-1 1.05
        modus-operandi-theme-scale-2 1.1
        modus-operandi-theme-scale-3 1.15
        modus-operandi-theme-scale-4 1.2))

(use-package modus-vivendi-theme
  :straight t
  :commands (load-theme)
  :config
  (setq modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-proportional-fonts nil
        modus-vivendi-theme-scale-headings t
        modus-vivendi-theme-scale-1 1.05
        modus-vivendi-theme-scale-2 1.1
        modus-vivendi-theme-scale-3 1.15
        modus-vivendi-theme-scale-4 1.2))

(use-package doom-themes
  :straight t
  :commands (load-theme)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package poet-theme :straight t :commands (load-theme))
(use-package leuven-theme :straight t :commands (load-theme))
(use-package emacs :hook (after-init . (lambda () (aru/load-theme 'leuven))))

(use-package magit :straight t :bind ("C-x g" . magit-status))

(use-package whitespace
  :commands
  (whitespace-buffer
   whitespace-cleanup
   whitespace-mode
   whitespace-turn-off)
  :blackout t)

(use-package org
  :config
  ;;; general
  (org-indent-mode -1)                  ; [default] do not indent text based on outline
  (setq org-startup-folded t)
  (setq org-reverse-note-order t)
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-goto-auto-isearch nil)
  (setq org-hide-block-startup t)
  (setq org-return-follows-link nil)
  (setq org-directory "~/org")
  (defconst aru/org-inbox-file (expand-file-name "inbox.org" org-directory)
    "File to use for capturing org items")
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▼ ")
  (setq org-default-notes-file aru/org-inbox-file)
  ;;; refile
  (setq org-refile-targets '((nil . (:level . 1))
                             (org-agenda-files . (:level . 1))))
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
  (setq org-babel-load-languages '((emacs-lisp . t)
				   (python     . t)
				   (shell      . t)))
  ;;; capture
  (setq org-capture-templates
	'(("i" "Item" item (file+headline aru/org-inbox-file "Inbox")
	   "- %U %?")
	  ("t" "Todo" entry (file+headline aru/org-inbox-file "Inbox")
	   "* TODO %?")
          ("p" "Paper" entry (file "~/org/reading-list.org")
           "* %?%^{Author}p%^{Title}p%^{Type}p%^{Genre}p")
          ("j" "Journal" entry (file "~/org/journal.org"))
          ("e" "Experiment" entry (file aru/org-inbox-file)
           "* %?\n*Motivation*\n*Hypothesis*\n*Result*\n*Next*")))
  ;; todo
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "NEXT(n)" "WAITING(w@/!)" "LATER(l)" "|" "CANCELLED(c@)")))
  (setq org-todo-keyword-faces
	'(("WAITING" :inherit default :weight bold)
	  ("LATER" :inherit warning :weight bold)))
  ;; archive
  (setq org-archive-location "~/org/archive.org::datetree/") ; archive in single file, in datetree
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . (lambda () (interactive) (org-capture nil)))
   ("C-c t" . (lambda () (interactive) (find-file aru/org-inbox-file)))))

(use-package ox-html :after org :config (setq org-html-validation-link nil))
(use-package org-tempo :after org)
(use-package org-habit :after org)
(use-package ox-md :after org)
(use-package ox-publish
  :after ox-html
  :config
  (setq org-publish-project-alist
        '(("posts"
           :base-directory "~/code/arumoy-src/"
           :base-extension "org"
           :publishing-directory "~/code/arumoy/"
           :recursive t
           :section-numbers nil
           :table-of-contents nil
           :auto-preamble t
           :html-head "<link rel=\"stylesheet\" href=\"css/main.css\" type=\"text/css\"/>"
           :publishing-function org-html-publish-to-html)
          ("static"
           :base-directory "~/code/arumoy-src/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/code/arumoy/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("arumoy" :components ("posts" "static")))))

(use-package eshell
  :commands eshell
  :config
  (setq eshell-prefer-lisp-functions t)
  (setq eshell-expand-input-functions '(eshell-expand-history-references))
  (setq eshell-banner-message
	'(format "%s %s\n"
		 (propertize (format " %s " (string-trim (buffer-name)))
			     'face 'mode-line-highlight)
		 (propertize (current-time-string)
			     'face 'font-lock-keyword-face))
	eshell-kill-processes-on-exit t))

(use-package time
  :config
  (setq display-time-24hr-format t)
  (setq display-time-day-and-date nil)
  (setq display-time-format nil)
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

(use-package custom
  :config
  (defun aru/load-theme (theme)
    "Disable the current theme and load a new one. It is assumed
    that only one theme is enabled."
    ;; below sexp to interactively get theme is from the load-theme source
    (interactive
     (list
      (intern (completing-read "Load custom theme: "
                               (mapcar #'symbol-name
				       (custom-available-themes))))))
    (disable-theme (car custom-enabled-themes))
    (load-theme theme t)))

(use-package mwheel
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse t))     ; default

(use-package python
  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python"))

(use-package olivetti
  :straight t
  :blackout t
  :config
  (defun aru/writing-on ()
    "Turn on variable pitch mode and olivetti mode."
    (interactive)
    (variable-pitch-mode +1)
    (olivetti-mode +1)
    (setq cursor-type 'bar)
    (text-scale-increase 1)
    (delete-other-windows))
  (defun aru/writing-off ()
    "Turn off variable pitch mode and olivetti mode."
    (interactive)
    (variable-pitch-mode -1)
    (olivetti-mode -1)
    (setq cursor-type 'box)
    (text-scale-increase 0)
    (winner-undo)))

;; finally, start the server
(server-start)
