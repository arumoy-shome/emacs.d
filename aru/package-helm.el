(defvar aru/notes-dir "~/org/"
  "Directory containing my notes")

(defvar aru/project-notes-dir "~/org/projects/"
  "Directory containing my project notes")

(defvar aru/bib-notes-dir "~/org/bib/"
  "Directory containing my bibliography notes")

(defun aru/helm-browse-notes ()
    "Open the notes dir in helm-find-files"
  (interactive)
  (helm-find-files-1 aru/notes-dir))

(defun aru/helm-browse-project-notes ()
  "Open the projects notes dir in helm-find-files"
  (interactive)
  (helm-find-files-1 aru/project-notes-dir))

(defun aru/helm-browse-bib-notes ()
  "Open the bibliography notes dir in helm-find-files"
  (interactive)
  (helm-find-files-1 aru/bib-notes-dir))

(provide 'package-helm)

