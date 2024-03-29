;;; aru-ocp.el --- aru org-capture paper -*- lexical-binding: t -*-

;;; variables

(defconst aocp-bibkey-regex  "@\\w+{\\(\\w+\\)"
  "Format of bibkey in a bibentry.")

(defconst aocp-author-regex "author={\\(.+\\)}"
  "Format of authors in a bibentry.")

(defconst aocp-source-regex "\\(booktitle\\|journal\\)={\\(.+\\)}"
  "Format of publishing source in a bibentry.")

(defconst aocp-year-regex "year={\\(.+\\)}"
  "Format of year in a bibentry.")

(defvar bibentry nil
  "The bibentry.")

(defvar bibkey nil
  "The bibkey.")

;;; functions

(defun aocp--push-bibkey-to-kill-ring ()
  "Push bibkey to the kill-ring."
  (kill-new bibkey))

(defun aocp--mod-kill-ring-hooks ()
  "Add hooks to modify kill-ring."
  (add-hook 'org-after-refile-insert-hook 'aocp--push-bibkey-to-kill-ring)
  (add-hook 'org-capture-after-finalize-hook 'aocp--push-bibkey-to-kill-ring))

(defun aocp--get-bibkey (&optional mod-kill-ring-p)
  "Return the bibkey from a bibentry. Assumes the last entry in
    the kill-ring is a bibtex entry. Additionally adds the bibkey
    to the top of the kill-ring when `mod-kill-ring-p' is true."

  (when kill-ring
    (setq bibentry (current-kill 0 t))
    (string-match aocp-bibkey-regex bibentry)
    (setq bibkey (match-string 1 bibentry))
    (when mod-kill-ring-p (aocp--mod-kill-ring-hooks))
    bibkey))

(defun aocp--get-author ()
  "Return the authors of publication from a bibentry."

  (when bibentry
    (let ((author nil)))
    (string-match aocp-author-regex bibentry)
    (setq author (match-string 1 bibentry))
    author))

(defun aocp--get-source ()
  "Return the source of publication from a bibentry."

  (when bibentry
    (let ((source nil))
      (string-match aocp-source-regex bibentry)
      (setq source (match-string 2 bibentry))
      source)))

(defun aocp--get-year ()
  "Return the year of publication from a bibentry."

  (when bibentry
    (let ((year nil))
      (string-match aocp-year-regex bibentry)
      (setq year (match-string 1 bibentry))
      year)))

(provide 'aru-ocp)
