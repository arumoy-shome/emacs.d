(defun aru/org-capture-template-paper(&optional mod-kill-ring-p)
  "Return the bibkey from a bibentry. Assumes the last entry in
    the kill-ring is a bibtex entry. Additionally adds the bibkey
    to the top of the kill-ring when `mod-kill-ring-p' is true."

  (let ((bib (current-kill 0 t))
        (bibkey nil))
    (string-match "@\\w+{\\(\\w+\\)," bib)
    (setq bibkey (match-string 1 bib))
    (if mod-kill-ring-p
        (kill-new bibkey))
    bibkey))

(provide 'aru-org)
