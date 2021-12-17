;; aru-narrow.el -*- lexical-binding: t; -*-

;;;###autoload
(defun aru-narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to
region, org subtree or function depending on the major-mode."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
	((region-active-p) (narrow-to-region (region-beginning) (region-end)))
	((equal major-mode 'org-mode) (org-narrow-to-subtree))
	(t (narrow-to-defun))))

(provide 'aru-narrow)
