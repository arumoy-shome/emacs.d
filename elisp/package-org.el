(defvar aru/org-todo-file "~/org/todo.org"
  "File to use for capturing org Todo items")

(defvar aru/org-note-file "~/org/notes.org"
  "File to use for capturing org Note items")

(defun aru/setup-org-agenda ()
  "Setup the org agenda mode. This function is run when the
org-agenda-finalize-hook is run"
  (setq org-agenda-files (expand-file-name "org-agenda-files.org" org-directory)))

(defun aru/setup-org-capture ()
  "Setup the org capture mode. This function is run after org is
loaded"
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
	org-capture-templates
	'(("t" "Todo" entry (file+headline aru/org-todo-file "Inbox")
	   "* TODO %?\n  %i\n  %a"
	   :prepend t)
	  ("n" "Note" entry (file+headline aru/org-note-file "Inbox")
	   "* %?\n  :PROPERTIES:\n  :CUSTOM_ID: %<%Y%m%d%H%M%S>\n  :END:\n  %i\n  %a"))))

(provide 'package-org)
