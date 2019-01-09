(defun aru/setup-org-agenda ()
  "Setup the org agenda mode. This function is run when the
org-agenda-finalize-hook is run"
  (setq org-agenda-files (expand-file-name "org-agenda-files.org" org-directory)))

(defun aru/setup-org-capture ()
  "Setup the org capture mode. This function is run after org is
loaded"
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
	org-capture-templates
	'(("t" "Todo" entry (file+headline "~/org/todo.org" "Inbox")
	   "* TODO %?\n  %i\n  %a"))))

(provide 'package-org)
