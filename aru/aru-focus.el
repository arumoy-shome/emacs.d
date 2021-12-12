;; aru-focus.el -*- lexical-binding: t; -*-
(defgroup aru-focus ()
  "Setup focus mode for text buffers."
  :group 'files)

(defcustom aru-focus-org-presentation nil
  "Org files should switch to focused view."
  :type 'boolean
  :group 'aru-focus)

(defcustom aru-focus-hidden-modeline nil
  "Hide the modeline."
  :type 'boolean
  :group 'aru-focus)

(defvar aru-focus--mode-hook nil
  "Hook that runs from function `aru-focus-mode'.")

;;;###autoload
(define-minor-mode aru-focus-mode
  "Buffer-local wrapper mode for focused editing. Also works for
  presentations. Use `aru-focus--mode-hook' to introduce their
  effects, on it's own it does not do anything."
  :init-value nil
  :global  nil
  :lighter " -Λ-"                      ; capital lambda
  (run-hooks 'aru-focus--mode-hook))

(autoload 'olivetti-mode "olivetti")

(defun aru-focus--olivetti-toggle ()
  "Toggle `olivetti-mode' when available."
  (if (or (bound-and-true-p olivetti-mode)
          (not (bound-and-true-p aru-focus-mode)))
      (olivetti-mode -1)
    (olivetti-mode 1)))

(autoload 'text-scale-mode "face-remap")

(defun aru-focus--increase-text-scale ()
  "Increase the text size by 1 point."
  (if (or (bound-and-true-p text-scale-mode)
          (not (bound-and-true-p aru-focus-mode)))
      (text-scale-decrease 1)
    (text-scale-increase 1)))

(autoload 'org-tree-slide-mode "org-tree-slide")

(defun aru-focus--org-tree-slide-toggle ()
  "Toggle `org-tree-slide-mode' when available."
  (let* ((buf (window-buffer (get-mru-window)))
         (mode (with-current-buffer buf major-mode)))
    (when (and aru-focus-org-presentation
               (eq mode 'org-mode))
      (if (or (bound-and-true-p org-tree-slide-mode)
              (not (bound-and-true-p aru-focus-mode)))
          (org-tree-slide-mode -1)
        (org-tree-slide-mode 1)))))

(defun aru-focus--modeline-toggle ()
  "Toggle modeline visibility."
  (when aru-focus-hidden-modeline
    (if (or (null mode-line-format)
            (not (bound-and-true-p aru-focus-mode)))
        (kill-local-variable 'mode-line-format)
      (setq-local mode-line-format nil)
      (force-mode-line-update))))

(add-hook 'aru-focus--mode-hook #'aru-focus--olivetti-toggle)
(add-hook 'aru-focus--mode-hook #'aru-focus--increase-text-scale)
(add-hook 'aru-focus--mode-hook #'aru-focus--org-tree-slide-toggle)
(add-hook 'aru-focus--mode-hook #'aru-focus--modeline-toggle)

(provide 'aru-focus)
