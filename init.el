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
(use-package hideshow   :hook (prog-mode . (lambda () (hs-minor-mode +1))))
(use-package ffap       :config (ffap-bindings))
(use-package emacs      :bind (("C-h h" . nil)))
(use-package hippie-exp :bind (("M-/" . hippie-expand)))

(use-package frame
  :config (blink-cursor-mode 0)
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames t)
  (setq dired-create-destination-dirs 'ask))

(use-package window
  :bind (("s-]" . other-window)
         ("s-[" . (lambda () (interactive) (other-window -1)))))

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
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package simple                     ; case bindings for active region
  :bind
  (("M-c" . capitalize-dwim)
   ("M-l" . downcase-dwim)
   ("M-u" . upcase-dwim))
  :config
  (setq kill-do-not-save-duplicates t)
  (setq async-shell-command-display-buffer nil)
  (setq shell-command-prompt-show-cwd t)
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

(use-package modus-operandi-theme
  :straight t
  :config
  (defun aru/colors-light ()
    (interactive)
    (setq modus-operandi-theme-slanted-constructs t
          modus-operandi-theme-bold-constructs t
          modus-operandi-theme-proportional-fonts nil
          modus-operandi-theme-scale-headings t
          modus-operandi-theme-scale-1 1.05
          modus-operandi-theme-scale-2 1.1
          modus-operandi-theme-scale-3 1.15
          modus-operandi-theme-scale-4 1.2)
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)))

(use-package modus-vivendi-theme
  :straight t
  :config
  (defun aru/colors-dark ()
    (interactive)
    (setq modus-vivendi-theme-slanted-constructs t
          modus-vivendi-theme-bold-constructs t
          modus-vivendi-theme-proportional-fonts nil
          modus-vivendi-theme-scale-headings t
          modus-vivendi-theme-scale-1 1.05
          modus-vivendi-theme-scale-2 1.1
          modus-vivendi-theme-scale-3 1.15
          modus-vivendi-theme-scale-4 1.2)
    (disable-theme 'modus-operandi)
    (load-theme 'modus-vivendi t)))

(use-package emacs :hook (after-init . aru/colors-dark))

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
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-goto-auto-isearch nil)
  (setq org-hide-block-startup t)
  (setq org-return-follows-link t)
  (setq org-directory "~/org")
  (defconst aru/org-inbox-file (expand-file-name "inbox.org" org-directory)
    "File to use for capturing org items")
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▼ ")
  (setq org-default-notes-file aru/org-inbox-file)
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-files (expand-file-name "org-agenda-files.org" org-directory))
  (setq org-agenda-skip-deadline-prewarning-if-scheduled t)
  (setq org-agenda-restore-windows-after-quit t)
  (setq org-agenda-use-tag-inheritance nil)
  (setq org-agenda-ignore-drawer-properties '(effort appt category stats))
  (setq org-babel-load-languages '((emacs-lisp . t)
				   (python     . t)
				   (shell      . t)))
  (setq org-capture-templates
	'(("i" "Item" item (file+headline aru/org-inbox-file "Inbox")
	   "- %U %?")
	  ("t" "Todo" entry (file+headline aru/org-inbox-file "Inbox")
	   "* TODO %?")
          ("p" "Paper" entry (file "~/org/reading-list.org")
           "* %?%^{Author}p%^{Title}p%^{Type}p%^{Genre}p")
          ("j" "Journal" entry (file "~/org/journal.org"))))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "NEXT(n)" "WAITING(w@/!)" "LATER(l)" "|" "CANCELLED(c@)")))
  (setq org-todo-keyword-faces
	'(("WAITING" :inherit default :weight bold)
	  ("LATER" :inherit warning :weight bold)))
  (setq org-agenda-custom-commands
        '(("o" "List of all Open TODO entries"
           ((todo ""
                  ((org-agenda-overriding-header "\nUnscheduled TODO")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))))))))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . (lambda () (interactive) (org-capture nil)))
   ("C-c t" . (lambda () (interactive) (find-file aru/org-inbox-file)))))

(use-package org-tempo :after org)
(use-package org-habit :after org)

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

(use-package doom-themes
  :straight t
  :commands (load-theme)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-org-config)) ; correct and improve org-mode native fontification

(use-package python
  :config
  (setq python-shell-interpreter "python3"))

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

;; finally, start the server
(server-start)
