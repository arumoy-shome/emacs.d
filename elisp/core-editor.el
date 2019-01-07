(setq-default
 delete-selection-mode t)

;; strip trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'core-editor)
