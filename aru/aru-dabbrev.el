;; aru-dabbrev.el -*- lexical-binding: t; -*-
(defun aru-dabbrev-completion ()
  "Taken from protesilaos. Expand current phrase or call
`dabbrev-completion'."
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

(provide 'aru-dabbrev)
