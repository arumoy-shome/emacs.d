;;; init.el --- personal emacs config -*- lexical-binding: t -*-

(add-to-list 'load-path (concat user-emacs-directory "aru/"))

(use-package use-package-core
  :custom
  (use-package-compute-statistics t)
  (use-package-verbose t))

(use-package editorconfig :hook after-init)

(use-package which-key :hook after-init)

(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(use-package diminish :ensure t)
(use-package cus-edit
  :demand t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (load custom-file))

(use-package eldoc
  :diminish eldoc-mode
  :hook ((after-init . global-eldoc-mode)))

(use-package hl-line
  :hook ((after-init . global-hl-line-mode)))

(use-package elec-pair
  :hook ((after-init . electric-pair-mode)))

(use-package autorevert
  :hook  ((after-init . global-auto-revert-mode)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package so-long
  :hook ((after-init . global-so-long-mode)))

(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer)))

(use-package outline
  ;; NOTE additionally set outline-minor-mode-prefix using custom
  :diminish outline-minor-mode
  :custom
  (outline-minor-mode-cycle t)
  (outline-minor-mode-use-buttons nil)
  :hook (prog-mode . outline-minor-mode))

(use-package backline
  :ensure t
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package outline-minor-faces
  :disabled t
  :ensure t
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-mode))

(use-package savehist :hook after-init)

(use-package compile
  :custom
  (compilation-scroll-output t))

(use-package flyspell
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  (ispell-program-name "/usr/local/bin/aspell")
  (ispell-dictionary "en")
  :hook ((prog-mode . flyspell-prog-mode)
	        (text-mode . flyspell-mode))
  :bind (("ESC M-s" . flyspell-mode)))	; ESC ESC s

(use-package flymake
  :bind (("ESC M-f" . flymake-mode)	; ESC ESC f
	 :map flymake-mode-map
	 ("M-n" . flymake-goto-next-error)
	 ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-show-diagnostics-at-end-of-line t))

(use-package dired
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-use-ls-dired nil)
  (dired-listing-switches "-FAlh")
  (dired-dwim-target t)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  :hook ((dired-mode . dired-hide-details-mode)))

(use-package frame
  :bind (("C-z" . nil))		; use C-x C-z instead
  :config
  (blink-cursor-mode -1))

(use-package window
  :bind-keymap (("s-4" . ctl-x-4-map)
                ("s-5" . ctl-x-5-map))
  :bind (("s-f" . find-file)
	 ("s-b" . switch-to-buffer)
         ("s-1" . delete-other-windows)
	 ("s-2" . split-window-below)
	 ("s-3" . split-window-right)
	 ("s-]" . (lambda () (interactive) (other-window +1)))
         ("s-[" . (lambda () (interactive) (other-window -1)))
	 ("s-w" . delete-window))	; previously delete-frame
  :hook ((help-mode . visual-line-mode)
	 (man-mode . visual-line-mode)
	 (woman-mode . visual-line-mode)))

(use-package windmove
  :disabled t
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
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :hook ((after-init . show-paren-mode)))

(use-package files
  :custom
  (confirm-kill-processes nil)
  (auto-save-default nil)
  (make-backup-files nil)
  (find-file-visit-truename t)
  (find-file-suppress-same-file-warnings t)
  (require-final-newline t)
  :bind (("s-q" . nil)))		; I hit this by mistake instead of M-q!

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
  :hook after-init
  :bind (:map ctl-x-map
	        ("f" . recentf-open)))        ; default: set-fill-column

(use-package simple                     ; case bindings for active region
  :bind
  (("M-Q" . delete-indentation)
   ("ESC M-v" . visual-line-mode)	; ESC ESC v
   ("ESC C-M-i" . indent-tab-mode)	; ESC ESC TAB
   ("C-c z" . delete-trailing-whitespace))
  :custom
  (kill-do-not-save-duplicates t)
  (async-shell-command-display-buffer nil)
  (shell-command-prompt-show-cwd t))

(use-package magit
  :ensure t
  :bind (:map ctl-x-map
              ("g" . magit-status)))

(use-package whitespace
  :bind (("ESC M-w" . whitespace-mode)))	; ESC ESC w

(use-package org
  :init
  ;; save all org agenda buffers after refile, stolen from purcell
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  (defconst aru/org-inbox-file "~/org/inbox.org"
    "File to use for capturing org items.")
  (org-babel-do-load-languages 'org-babel-load-languages
		'((emacs-lisp . t)
			 (python    . t)
			 (shell     . t)))
  (org-clock-persistence-insinuate)
  :custom
  (org-startup-folded t)
  (org-reverse-note-order t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-goto-auto-isearch nil)
  (org-hide-block-startup t)
  (org-return-follows-link nil)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-directory "~/org")
  (org-log-into-drawer t)
  (org-default-notes-file aru/org-inbox-file)
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  ;; refile
  (org-refile-use-outline-path 'file) ; allow refiling as top level header
  (org-refile-targets '((nil . (:maxlevel . 2))
                        (org-agenda-files . (:maxlevel . 2))))
  ;; agenda
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  (org-agenda-files (expand-file-name "org-agenda-files.org" org-directory))
  (org-agenda-skip-deadline-prewarning-if-scheduled t)
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-use-tag-inheritance nil)
  (org-agenda-ignore-drawer-properties '(effort appt stats))
  (org-agenda-custom-commands
   '(("g" "GTD" tags-todo "-TODO=\"PROJECT\"-paper")))
  ;; babel
  (org-confirm-babel-evaluate nil)
  ;; capture
  (org-capture-templates
    '(("p" "Paper" entry (file aru/org-inbox-file)
      "%[~/.emacs.d/org-templates/bib.txt]" :prepend t)
     ("c" "Capture" entry (file aru/org-inbox-file)
      "%[~/.emacs.d/org-templates/capture.txt]" :prepend t)
     ("t" "Today" entry (file aru/org-inbox-file)
      "%[~/.emacs.d/org-templates/today.txt]" :prepend t)))
  ;; todo
  (org-todo-keywords
   '((sequence "TODO(t)" "PROJECT(p)" "|" "DONE(d!)" "CANCEL(c@)")))
  ;; archive
  (org-archive-location "~/org/archive/%s_archive::")
  ;; clocking
  (org-clock-persist 'history)
  :bind
  (("C-c l" . org-store-link)
    ("C-c a" . org-agenda)
    ("C-c c" . (lambda () (interactive) (org-capture nil))))
  :hook
  ((org-mode . auto-fill-mode)))

(use-package org-modern
  :disabled t
  :ensure t
  :after org
  :custom
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)

  ;; Org styling, hide markup etc.
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")

  ;; Agenda styling
  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
  (org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  :config
  (global-org-modern-mode))

(use-package spacious-padding
  :ensure t
  :custom
  (spacious-padding-widths
   '(:internal-border-width 5 :right-divider-width 5 :scroll-bar-width 0))
  :hook after-init)

(use-package ox-html :after org :custom (org-html-validation-link nil))
(use-package ox-md :after org)
(use-package org-tempo :after org)
(use-package org-id :after org)

(use-package text-mode
  :after org
  :bind (:map text-mode-map
              ("C-c !" . org-time-stamp-inactive)))

(use-package tex-mode
  :hook (tex-mode . outline-minor-mode))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
          ("\\.md\\'" . markdown-mode)
          ("\\.markdown\\'" . markdown-mode)
	        ("\\.qmd\\'" . markdown-mode)
	        ("README" . gfm-mode))
  :hook
  ((markdown-mode . visual-line-mode)))

(use-package python
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "-i --simple-prompt")
  (org-babel-python-command "ipython --simple-prompt"))

(use-package imenu
  :custom
  (imenu-auto-rescan t))

(use-package tab-bar
  :hook
  (after-init . tab-bar-history-mode) ; separate window history per tab
  :bind-keymap ("s-t" . tab-prefix-map)	; previously menu-set-font
  :bind (("s-}" . tab-next)
         ("s-{" . tab-previous)
         :map tab-prefix-map
         ("w" . tab-close)))

(use-package calendar
  :custom
  (calendar-week-start-day 1))     ; start on Mondays

(use-package aru-narrow
  :bind (:map narrow-map
              ("n" . aru-narrow-or-widen-dwim)))

(use-package custom
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-org-blocks 'tinted-background))

(use-package aru-custom
  :config
  (add-hook 'ns-system-appearance-change-functions #'aru-load-theme-auto))

(use-package doc-view
  :custom
  (doc-view-resolution 200)
  (doc-view-mupdf-use-svg t))

(use-package marginalia
  :ensure t
  :hook after-init)

(use-package consult
  :ensure t
  :bind (("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s l" . consult-line)))

(use-package eglot
  :disabled t
  :custom
  (eglot-autoshutdown t)
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   ; (latex-mode . eglot-ensure)
   ; (bibtex-mode . eglot-ensure)
))

(use-package treesit
  :init
  ;; Reference URL: <https://www.masteringemacs.org/article/how-to-get-started-tree-sitter>
  ;; Manually install the following grammars using `treesit-install-language-grammar`
  (setq treesit-language-source-alist
    '(
       ;; (python "https://github.com/tree-sitter/tree-sitter-python")
       ;; (bash "https://github.com/tree-sitter/tree-sitter-bash")
       ;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
       ;; (html "https://github.com/tree-sitter/tree-sitter-html")
       ;; (css "https://github.com/tree-sitter/tree-sitter-css")
       ;; (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
       (json "https://github.com/tree-sitter/tree-sitter-json")
       ;; (make "https://github.com/alemuller/tree-sitter-make")
       ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
       ;; (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
       ;; (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
       (yaml "https://github.com/ikatyang/tree-sitter-yaml")
       ;; (latex "https://github.com/latex-lsp/tree-sitter-latex")
       (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
       ;; (rust "https://github.com/tree-sitter/tree-sitter-rust.git")
       ))
  :custom
  (major-mode-remap-alist
    `(
       (yaml-mode . yaml-ts-mode)
       ;; (bash-mode . bash-ts-mode)
       ;; (js2-mode . js-ts-mode)
       ;; (typescript-mode . typescript-ts-mode)
       (json-mode . json-ts-mode)
       (js-json-mode . json-ts-mode)
       ;; (python-mode . python-ts-mode)
       ;; (css-mode . css-ts-mode)
       ;; (html-mode . html-ts-mode)
       ))
  :mode
  (
    ("\\.ipynb\\'" . json-ts-mode)
    ("Dockerfile" . dockerfile-ts-mode)
    ("\\.yaml\\'" . yaml-ts-mode)
    ("\\.yml\\'" . yaml-ts-mode)
    ;; ("\\.ts\\'" . typescript-ts-mode)
    ;; ("\\.rs\\'" . rust-ts-mode)
    ))

(use-package vertico
  :ensure t
  :hook after-init)

(use-package corfu
  :ensure t
  :custom
  (tab-always-indent 'complete)
  :init
  (global-corfu-mode))

(use-package evil
  :ensure t
  :config
  (evil-set-leader nil (kbd "SPC"))
  :custom
  (evil-respect-visual-line-mode t)
  (evil-want-C-i-jump t)
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-undo-system 'undo-redo)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-want-keybinding nil)
  :bind (:map evil-normal-state-map
          ("<leader>o" . consult-outline))
  :hook after-init)

(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :hook org-mode
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode))

(use-package evil-commentary
  :ensure t
  :hook after-init)

(use-package display-line-numbers
  :after evil
  :hook after-init
  :custom
  (display-line-numbers-type 'relative)
  (display-line-numbers-widen t)) ; absolute numbers in narrowed buffers

(use-package orderless
  :ensure t
  :config
  (add-to-list 'completion-styles 'orderless t))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package lua-mode :ensure t)

(use-package doom-modeline
  :ensure t
  :hook after-init)

(use-package gptel
  :ensure t
  :custom
  (gptel-model "gpt-4o-mini")
  :bind
  (("s-<return>" . gptel-send)))

(use-package popper
  :ensure t
  :bind
  (("C-`" . popper-toggle)
    ("M-`" . popper-cycle)
    ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
    '("\\*Messages\\*"
       ("Output\\*" . hide)
       ("\\*Async Shell Command\\*" . hide)
       (compilation-mode . hide)
       help-mode))
  (popper-group-function #'popper-group-by-project)
  :init
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;; init.el ends here
