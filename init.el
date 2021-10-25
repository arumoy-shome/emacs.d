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
(use-package aru-cus-edit :demand t)

(use-package org :straight t)

(use-package menu-bar :config (menu-bar-mode -1))
(use-package tool-bar :config (tool-bar-mode -1))
(use-package tooltip  :config (tooltip-mode -1))
(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))

(use-package delsel     :hook (after-init . delete-selection-mode))
(use-package hl-line    :config (global-hl-line-mode nil))
(use-package novice     :config (setq disabled-command-function nil))
(use-package saveplace  :config (save-place-mode +1))
(use-package ibuffer    :bind (([remap list-buffers] . #'ibuffer)))
(use-package text-mode  :hook (text-mode	. auto-fill-mode))
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
(use-package newcomment :bind (("s-/" . comment-line)))
(use-package emacs :bind-keymap ("C-s-n" . narrow-map))

(use-package winner
  :config
  (winner-mode +1)
  :bind (("s-<left>"  . winner-undo)    ; previously ns-prev-frame
         ("s-<right>" . winner-redo)))  ; previously ns-next-frame

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

(use-package outline-minor-faces
  :straight t
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(use-package flyspell
  :blackout t
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-dictionary "en_GB")
  :bind (:map flyspell-mode-map ("C-." . nil)))

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
  (setq dired-listing-switches
        "-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p)
  :hook ((dired-mode . dired-hide-details-mode)))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t))

(use-package dired-x
  :bind (("s-j" . dired-jump)
         ("s-J" . dired-jump-other-window)
         :map ctl-x-map
              ("C-j" . dired-jump)
              ("4 C-j" . dired-jump-other-window)))

(use-package window
  :bind-keymap ("C-s-p" . project-prefix-map)
  :bind (("s-]" . other-window)
         ("s-[" . (lambda () (interactive) (other-window -1)))
         ("s-1" . delete-other-windows)
         ("C-S-s-p" . project-other-window-command)
         ("s-C" . clone-indirect-buffer-other-window)
         ("s-w" . delete-window)
         ("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         ("s-h" . previous-buffer)      ; previously ns-do-hide-emacs
         ("s-l" . next-buffer))) ; previously goto-line, use M-g g instead

(use-package aru-window
  :bind (("s-m" . aru-window-mode)
         ("s-o" . aru-window-other-window-dwim)
         ("s-3" . aru-window-split-window-right)
         ("s-2" . aru-window-split-window-below)))

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
   ("M-Q" . delete-indentation))
  :config
  (setq kill-do-not-save-duplicates t)
  (setq async-shell-command-display-buffer nil)
  (setq shell-command-prompt-show-cwd t)
  (column-number-mode +1))               ; show line and column numbers

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
         ("s-B" . consult-buffer-other-window)
         ("C-." . consult-imenu)
         :map ctl-x-map                    ; C-x bindings
         ("M-:" . consult-complex-command) ; [default] repeat-complex-command
         ("b" . consult-buffer)            ; [default] switch-to-buffer
         ("4 b" . consult-buffer-other-window) ; [default] switch-to-buffer-other-window
         ("5 b" . consult-buffer-other-frame)  ; [default] switch-to-buffer-other-frame
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
  :commands (load-theme)
  :init
  (setq modus-themes-slanted-constructs t
        modus-themes-bold-constructs t
        modus-themes-syntax 'alt-syntax
        modus-themes-completions 'opinionated
        modus-themes-intense-hl-line t
        modus-themes-intense-paren-match t
        modus-themes-org-blocks 'rainbow
        modus-themes-mode-line 'moody
        modus-themes-links 'faint-neutral-underline
        modus-themes-prompts 'subtle
        modus-themes-headings
        '((1 . section)
          (2 . line)
          (3 . highlight)
          (t . rainbow-no-bold))
        modus-themes-scale-headings t))

(use-package leuven-theme :straight t :commands (load-theme))
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
  :blackout t)

(use-package org
  :hook (org-mode . (lambda () (electric-indent-local-mode -1))) ; do not auto indent in org buffers
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
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-directory "~/org")
  (defconst aru/org-inbox-file (expand-file-name "inbox.org" org-directory)
    "File to use for capturing org items.")
  (defconst aru/org-journal-file (expand-file-name "journal.org.gpg" org-directory)
    "File to use for capturing journal entries.")
  (setq org-log-into-drawer t)
  (setq org-ellipsis " ▼ ")
  (setq org-default-notes-file aru/org-inbox-file)
  ;;; refile
  (setq org-refile-targets '((nil . (:maxlevel . 6))
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
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
				                         (python     . t)
				                         (shell      . t)))
  ;;; capture
  (setq org-capture-templates
	      '(("t" "Todo" entry (file+headline aru/org-inbox-file "Inbox")
	         "* TODO %?")
          ("e" "Experiment" entry (file+headline aru/org-inbox-file "Inbox")
           "%[~/.emacs.d/org-templates/experiment.txt]")
          ("p" "Paper" entry (file+headline aru/org-inbox-file "Inbox")
           "%[~/.emacs.d/org-templates/paper.txt]")
          ("i" "Idea" entry (file+headline aru/org-inbox-file "Inbox")
           "%[~/.emacs.d/org-templates/idea.txt]")
          ("j" "Journal" entry (file aru/org-journal-file)
           "%[~/.emacs.d/org-templates/journal.txt]" :prepend t)))

  ;; todo
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "NEXT(n)" "WAITING(w@/!)" "LATER(l)" "|" "CANCELLED(c@)")
          (sequence "1PASS(1)" "|" "2PASS(2)" "3PASS(3)")
          (sequence "IDEA")))
  (setq org-todo-keyword-faces
	'(("WAITING" :inherit default :weight bold)
	  ("LATER" :inherit warning :weight bold)))
  ;; archive
  (setq org-archive-location "~/org/archive/%s_archive::") ; archive in single file, in datetree
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . (lambda () (interactive) (org-capture nil)))
   ("C-c t" . (lambda () (interactive) (find-file aru/org-inbox-file)))
   ("C-c T" . (lambda () (interactive) (find-file-other-window aru/org-inbox-file)))
   ("C-c d" . (lambda () (interactive) (dired org-directory)))
   ("C-c D" . (lambda () (interactive) (dired-other-window org-directory)))))

(use-package aocp :straight (:host github :repo "arumoy-shome/aocp.el") :after org)
(use-package ox-html :after org :config (setq org-html-validation-link nil))
(use-package org-tempo :after org)
(use-package org-habit :after org)
(use-package ox-md :after org)
(use-package org-id :after org)
(use-package ox-publish
  :after ox-html
  :config
  (setq org-publish-project-alist
        '(("org-posts"
           :base-directory "~/org"
           :base-extension "org"
           :publishing-directory "~/org/docs/"
           :exclude "inbox\\.org\\|org-agenda-files\\.org"
           :recursive t
           :section-numbers nil
           :table-of-contents nil
           :auto-preamble t
           :auto-sitemap t
           :html-head "<link rel=\"stylesheet\" href=\"assets/css/main.css\" type=\"text/css\"/>\n<meta name=\"robots\" content=\"noindex\">"
           :publishing-function org-html-publish-to-html)
          ("org-static"
           :base-directory "~/org/assets"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/org/docs/assets/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("org" :components ("org-posts" "org-static"))
          ("website-posts"
           :base-directory "~/code/arumoy"
           :base-extension "org"
           :publishing-directory "~/code/arumoy/docs/"
           :section-numbers nil
           :auto-preamble t
           :auto-sitemap t
           :html-head "<link rel=\"stylesheet\" href=\"assets/css/main.css\" type=\"text/css\"/>"
           :publishing-function org-html-publish-to-html)
          ("website-static"
           :base-directory "~/code/arumoy/assets"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/code/arumoy/docs/assets/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("website-cname"
           :base-directory "~/code/arumoy/"
           :base-extension ""
           :publishing-directory "~/code/arumoy/docs/"
           :include ("CNAME")
           :publishing-function org-publish-attachment)
          ("website" :components ("website-posts" "website-static" "website-cname")))))

(use-package eshell
  :commands (eshell)
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
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python"))

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
  (setq dabbrev-upcase-means-case-search t)
  :bind (("M-/" . dabbrev-expand)))

(use-package aru-dabbrev :bind (("C-M-/" . aru-dabbrev-completion)))

(use-package doom-modeline
  :straight t
  :config
  (setq doom-modeline-height 15)
  (setq doom-modeline-bar-width 2)
  (setq doom-modeline-window-width-limit 'fill-column)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-continuous-word-count-modes nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-number-limit 0)
  (setq doom-modeline-workspace-name t)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-github nil)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-mu4e nil)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-env-version nil)

  (defun aru/doom-modeline--make-xpm-filter-args (args)
    "Taken from
https://github.com/seagle0128/doom-modeline/issues/187#issuecomment-503950599
Force function to use =doom-modeline-height= instead of the
calculation done in =doom-modeline-refresh-bars=. Minimum height
set to =frame-char-height= + 2."
    (list (car args) (cadr args) (max (+ (frame-char-height) 2) doom-modeline-height)))

  (advice-add 'doom-modeline--make-xpm :filter-args
              #'aru/doom-modeline--make-xpm-filter-args)
  :hook (after-init . doom-modeline-mode))

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
  (setq tab-bar-position nil)
  (setq tab-bar-show nil)
  (setq tab-bar-tab-hints nil)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-all)

  (tab-bar-mode +1)
  (tab-bar-history-mode -1)

  :bind-keymap ("s-t" . tab-prefix-map))

(use-package calendar
  :config
  (setq calendar-week-start-day 1))     ; start on Mondays

;; finally, start the server
(server-start)
