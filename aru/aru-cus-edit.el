;; aru-cus-edit.el -*- lexical-binding: t; -*-
(setq aru-cus-edit--custom-file (expand-file-name "custom.el" user-emacs-directory))

(defun aru-cus-edit--setup-file ()
  (unless (file-exists-p aru-cus-edit--custom-file)
    (make-empty-file aru-cus-edit--custom-file))
  (load-file aru-cus-edit--custom-file))

(add-hook 'after-init-hook #'aru-cus-edit--setup-file)

(provide 'aru-cus-edit)
