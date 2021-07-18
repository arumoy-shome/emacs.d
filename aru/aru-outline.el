(defvar aru-outline-minor-mode-map
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
    (define-key map (kbd "<tab>") 'aru-outline-bicycle-cycle-tab-dwim)
    (define-key map (kbd "<C-tab>") 'bicycle-cycle)
    (define-key map (kbd "<S-tab>") 'bicycle-cycle-global)
    map)
  "Custom keymap to rid the clunky C-c C-@ prefix that outline-mode
uses by default.")

(autoload 'bicycle-cycle "bicycle")

(defun aru-outline-bicycle-cycle-tab-dwim ()
  "Wrapper around TAB in outline-minor-mode."
  (interactive)
  (if (outline-on-heading-p)
      (bicycle-cycle)
    (indent-for-tab-command)))

;;;###autoload
(define-minor-mode aru-outline-minor-mode
  "Toggle 'outline-minor-mode' and extras."
  :init-value nil
  :lighter " -Ω-"                       ; ohm
  :global nil
  :keymap aru-outline-minor-mode-map
  (if aru-outline-minor-mode
      (progn
        (when (eq major-mode 'org-mode)
          (user-error "Don't use 'outline-minor-mode' with Org"))
        (outline-minor-mode 1))
    (outline-minor-mode -1)))

(provide 'aru-outline)
