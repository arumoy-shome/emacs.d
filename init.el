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
  :bind (:map ctl-x-map
              ("C-\\" . aru/split-window-horizontally-instead)
              ("C--" . aru/split-window-vertically-instead))) ; use s-- instead

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
(use-package re-builder :config (setq reb-re-syntax 'read))
(use-package olivetti   :straight t :blackout t)
(use-package hydra      :straight t)

(use-package eldoc
  :blackout t
  :config (global-eldoc-mode 1))

(use-package wgrep
  :straight t
  :commands wgrep
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
  :blackout ((outline-minor-mode)
             (aru/outline-minor-mode))
  :config
  (defun aru/bicycle-cycle-tab-dwim ()
    "Taken from protesilaos. Wrapper around TAB in outline-mode."
    (interactive)
    (if (outline-on-heading-p)
        (bicycle-cycle)
      (indent-for-tab-command)))

  (defvar aru/outline-minor-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-z n") 'outline-next-visible-heading)
      (define-key map (kbd "C-c C-z p") 'outline-previous-visible-heading)
      (define-key map (kbd "C-c C-z f") 'outline-forward-same-level)
      (define-key map (kbd "C-c C-z b") 'outline-backward-same-level)
      (define-key map (kbd "C-c C-z a") 'outline-show-all)
      (define-key map (kbd "C-c C-z u") 'outline-up-heading)
      (define-key map (kbd "C-c C-z o") 'outline-hide-other)
      (define-key map (kbd "C-c C-z z") 'foldout-zoom-subtree)
      (define-key map (kbd "C-c C-z x") 'foldout-exit-fold)
      (define-key map (kbd "C-c C-z <return>") 'outline-insert-heading)
      (define-key map (kbd "C-c C-z <down>") 'outline-move-subtree-down)
      (define-key map (kbd "C-c C-z <up>") 'outline-move-subtree-up)
      (define-key map (kbd "C-c C-z <left>") 'outline-promote)
      (define-key map (kbd "C-c C-z <right>") 'outline-demote)
      (define-key map (kbd "<tab>") 'aru/bicycle-cycle-tab-dwim)
      (define-key map (kbd "<C-tab>") 'bicycle-cycle)
      (define-key map (kbd "<S-tab>") 'bicycle-cycle-global)
      map)
    "Custom keymap to rid the clunky C-c C-@ prefix that outline-mode
uses by default.")

  (define-minor-mode aru/outline-minor-mode
    "Toggle 'outline-minor-mode' and extras."
    :init-value nil
    :lighter " Aru-Outline"
    :global nil
    :keymap aru/outline-minor-mode-map
    (if aru/outline-minor-mode
        (progn
          (when (eq major-mode 'org-mode)
            (user-error "Don't use 'outline-minor-mode' with Org"))
          (outline-minor-mode 1))
      (outline-minor-mode -1))))

(use-package outline-minor-faces
  :straight t
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-add-font-lock-keywords))

(use-package foldout :after outline)
(use-package bicycle :straight t :after outline)

(use-package project
  :bind (("C-c p f" . project-find-file)
         ("C-c p g" . project-find-regexp)
         ("C-c p s" . project-search)
         ("C-c p %" . project-query-replace-regexp)))

(use-package flyspell
  :blackout t
  :config
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-program-name "/usr/local/bin/aspell")
  (setq ispell-dictionary "en_GB")
  :bind (:map flyspell-mode-map ("C-." . nil)))

(use-package flymake
  :commands flymake-mode
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode nil)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)

  (defhydra aru/hydra-flymake (:color pink :hint nil)
    "
Actions
-------
_s_: Start checks
_n_: Next error
_p_: Previous error
_d_: Diagnostics' buffer
"
    ("s" flymake-start)
    ("d" flymake-show-diagnostics-buffer)
    ("n" flymake-goto-next-error)
    ("p" flymake-goto-prev-error)
    ("q" nil "cancel" :color blue))

  :bind (:map flymake-mode-map
              ("C-c h l" . aru/hydra-flymake/body)))

(use-package flymake-diagnostic-at-point
  :straight t
  :after flymake
  :config
  (setq flymake-diagnostic-at-point-display-diagnostic-function
        'flymake-diagnostic-at-point-display-minibuffer))

(use-package flymake-proselint
  :straight (:host github :repo "manuel-uberti/flymake-proselint")
  :after flymake
  :init
  (dolist (mode '("markdown-mode" "org-mode" "text-mode" "latex-mode"))
    (add-hook (intern (concat mode "-hook")) #'flymake-proselint-setup)))

(use-package flycheck-aspell
  :straight (:host github :repo "leotaku/flycheck-aspell")
  :after (flyspell flymake)
  :init
  (dolist (mode '("markdown-mode" "org-mode" "text-mode" "latex-mode"))
    (add-hook (intern (concat mode "-hook")) #'flymake-aspell-setup)))

(use-package hideshow                   ; aru/outline-minor-mode internally calls hideshow
  :hook (prog-mode . aru/outline-minor-mode)
  :blackout hs-minor-mode)

(use-package vc-dispatcher
  :config
  (setq vc-suppress-confirm t)
  (setq vc-command-messages t))

(use-package tex-mode
  :config
  (setq latex-run-command "pdflatex -interaction=nonstopmode")
  (setq tex-dvi-view-command "open")
  (setq tex-print-file-extension ".pdf")
  :hook (latex-mode . aru/outline-minor-mode))

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
  :hook ((dired-mode . dired-hide-details-mode)))

(use-package dired-aux
  :config
  (setq dired-isearch-filenames t)
  (setq dired-create-destination-dirs 'ask)
  (setq dired-vc-rename-file t))

(use-package dired-x
  :bind (("s-j" . dired-jump)
         ("s-J" . dired-jump-other-window)
         :map ctl-x-map
              ("C-j" . dired-jump)
              ("4 C-j" . dired-jump-other-window)))

(use-package window
  :init
  (defvar aru/window-configuration nil
    "Current window configuration.")

  (define-minor-mode aru/window-single-toggle
    "Taken from protesilaos. Toggle between single and multiple
    windows. Equivalent to maximizing."
    :lighter " [M]"
    :global nil
    (if (one-window-p)
        (when aru/window-configuration
          (set-window-configuration aru/window-configuration))
      (setq aru/window-configuration (current-window-configuration))
      (delete-other-windows)))

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
         ("s-F" . find-file-other-window)
         ("s-d" . list-directory)
         ("s-D" . dired-other-window)
         ("s-h" . previous-buffer)      ; previously ns-do-hide-emacs
         ("s-l" . next-buffer) ; previously goto-line, use M-g g instead
         ("s-m" . aru/window-single-toggle)))

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
  (setq aru/custom-file (expand-file-name "custom.el" user-emacs-directory))

  (defun aru/cus-edit ()
    (unless (file-exists-p aru/custom-file)
      (make-empty-file aru/custom-file))
    (load-file aru/custom-file))
  :hook (after-init . aru/cus-edit))

(use-package recentf
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  (defun aru/get-recentf-files ()
    "Get a list of recent files."
    (mapcar 'abbreviate-file-name recentf-list))

  (defun aru/recentf-find-file ()
    "Taken from
    https://github.com/raxod502/selectrum/wiki/Useful-Commands#switch-to-recent-file"
    (interactive)
    (let ((files (aru/get-recentf-files)))
      (find-file (completing-read "Find recent file: " files nil t))))

  (defun aru/recentf-find-file-other-window ()
    "Like aru/recentf-find-file but in other window."
    (interactive)
    (let ((files (aru/get-recentf-files)))
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
   ("s-n" . next-error)
   ("s-p" . previous-error)
   ("M-SPC" . cycle-spacing))           ; previously just-one-space
  :config
  (setq kill-do-not-save-duplicates t)
  (setq async-shell-command-display-buffer nil)
  (setq shell-command-prompt-show-cwd t)
  (column-number-mode +1)               ; show line and column numbers

  :bind ("M-Q" . delete-indentation))

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
  :init
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-syntax 'alt-syntax
        modus-operandi-theme-completions 'opinionated
        modus-operandi-theme-intense-hl-line t
        modus-operandi-theme-intense-paren-match t
        modus-operandi-theme-org-blocks 'grayscale
        modus-operandi-theme-headings
        '((1 . section)
          (2 . line)
          (3 . highlight)
          (t . rainbow-no-bold))
        modus-operandi-theme-scale-headings t))

(use-package modus-vivendi-theme
  :straight t
  :commands (load-theme)
  :init
  (setq modus-vivendi-theme-slanted-constructs t
        modus-vivendi-theme-bold-constructs t
        modus-vivendi-theme-syntax 'alt-syntax
        modus-vivendi-theme-completions 'opinionated
        modus-vivendi-theme-intense-hl-line t
        modus-vivendi-theme-intense-paren-match t
        modus-vivendi-theme-org-blocks 'grayscale
        modus-vivendi-theme-headings
        '((1 . section)
          (2 . line)
          (3 . highlight)
          (t . ranbow-section))
        modus-vivendi-theme-scale-headings t))

(use-package doom-themes
  :straight t
  :commands (load-theme)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

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
  (setq org-id-link-to-org-use-id t)
  (setq org-directory "~/org")
  (defconst aru/org-inbox-file (expand-file-name "inbox.org" org-directory)
    "File to use for capturing org items")
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
  (setq org-babel-load-languages '((emacs-lisp . t)
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
           "%[~/.emacs.d/org-templates/idea.txt]")))
  ;; todo
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "NEXT(n)" "WAITING(w@/!)" "LATER(l)" "|" "CANCELLED(c@)")))
  (setq org-todo-keyword-faces
	'(("WAITING" :inherit default :weight bold)
	  ("LATER" :inherit warning :weight bold)))
  ;; archive
  (setq org-archive-location "~/org/archive/%s_archive::") ; archive in single file, in datetree
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . (lambda () (interactive) (org-capture nil)))
   ("C-c t" . (lambda () (interactive) (find-file aru/org-inbox-file)))))

(use-package ox-html :after org :config (setq org-html-validation-link nil))
(use-package org-tempo :after org)
(use-package org-habit :after org)
(use-package ox-md :after org)
(use-package org-id :after org)
(use-package ox-publish
  :after ox-html
  :config
  (setq org-publish-project-alist
        '(("posts"
           :base-directory "~/org"
           :base-extension "org"
           :publishing-directory "~/code/arumoy/"
           :exclude "^private.*\\.org\\|inbox\\.org\\|org-agenda-files\\.org"
           :recursive t
           :section-numbers nil
           :table-of-contents nil
           :auto-preamble t
           :auto-sitemap t
           :html-head "<link rel=\"stylesheet\" href=\"assets/css/main.css\" type=\"text/css\"/>\n<meta name=\"robots\" content=\"noindex\">"
           :publishing-function org-html-publish-to-html)
          ("static"
           :base-directory "~/org"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/code/arumoy/"
           :include ("CNAME")
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

(use-package custom
  :config
  (defun aru/load-theme (theme)
    "Disable all current themes and load a new one."
    ;; below sexp to interactively get theme is from the load-theme source
    (interactive
     (list
      (intern (completing-read "Load custom theme: "
                               (mapcar #'symbol-name
				       (custom-available-themes))))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(use-package mwheel
  :config
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse t))     ; default

(use-package python
  :config
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python"))

(use-package emacs
  :config
  (setq narrow-to-defun-include-comments t))

(use-package newcomment
  :bind (("s-/" . comment-line)))

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

  (defun aru/dabbrev-completion ()
    "Taken from protesilaos. Expand current phrase or call `dabbrev-completion'."
    (interactive)
    (let* ((abbrev (dabbrev--abbrev-at-point))
           (ignore-case-p (dabbrev--ignore-case-p abbrev))
           (completion-list (dabbrev--find-all-expansions abbrev ignore-case-p)))
      (cond
       ((when (and (eq completion-list nil)
                   (not (eq last-repeatable-command 'mode-exit)))
          (insert " ")
          (dabbrev-expand 1)))
       (t
        (dabbrev-completion)))))

    :bind (("M-/" . dabbrev-expand)
           ("C-M-/" . aru/dabbrev-completion)))

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

  (defun aru/tab-bar-select-tab-dwim ()
    "Taken from protesilaos. Do-What-I-Mean function for getting to a
`tab-bar-mode' tab. If no other tab exists, create one and switch to
it. If there is one other tab (so two in total) switch to it without
further questions. Else use completion to select the tab to switch
to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t (tab-bar-switch-to-tab tabs)))))

  :bind (("s-t" . aru/tab-bar-select-tab-dwim)
         :map ctl-x-map
         ("t t" . aru/tab-bar-select-tab-dwim)
         ("t n" . tab-next)
         ("t p" . tab-previous)))

;; finally, start the server
(server-start)
