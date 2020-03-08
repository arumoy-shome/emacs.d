(defun org-zettel-get-inbound-links ()
  (interactive)
  (let ((title (nth 4 (org-heading-components))))
    (org-occur (concat "*" title ))))

(provide 'org-zettel)
