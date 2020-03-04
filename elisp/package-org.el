(defvar aru/org-inbox-file "~/org/inbox.org"
  "File to use for capturing org items")

(defun aru/setup-org-agenda ()
  "Setup the org agenda mode. This function is run when the
org-agenda-finalize-hook is run"
  (setq org-agenda-files (expand-file-name "org-agenda-files.org" org-directory)))

(defun aru/setup-org-capture ()
  "Setup the org capture mode. This function is run after org is
loaded"
  (setq org-default-notes-file aru/org-inbox-file
	org-capture-templates
	'(("i" "Item" item (file+headline aru/org-inbox-file "Inbox")
	   "- %U %?"
	   :prepend t)
	  ("t" "Todo" entry (file+headline aru/org-inbox-file "Inbox")
	   "** TODO %?"
	   :prepend t))))

(defun aru/setup-org-ui ()
  "Setup how org looks. This function is run after org is
  loaded"
  (setq org-todo-keywords
	'((sequence "TODO(t)" "|" "DONE(d!)")
	  (sequence "NEXT(n)" "WAITING(w@/!)" "LATER(l)" "|" "CANCELLED(c@)"))
	org-todo-keyword-faces
	'(("WAITING" :inherit default :weight bold)
	  ("LATER" :inherit warning :weight bold))))

(provide 'package-org)
