;;; transient-bib-entry.el --- Create and edit bibliography entries -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Karl Hallsby <karl@hallsby.com>
;; Maintainer: Karl Hallsby <karl@hallsby.com>

;;; Commentary:
;; This defines and implements the necessary functions and keybindings for
;; searching a bibliography file making use of `transient-bib'.

;;; Code:
(require 'transient-bib-mode)

(defun transient-bib-entry-new-placeholder ()
  "PLACEHOLDER function for creating a new BibTeX/BibLaTeX entry."
  (interactive)
  (transient-bib-UNIMPLEMENTED "transient-bib-entry-new-placeholder"))

;;;###autoload
(defun transient-bib-entry-edit ()
  "Doing things wrong."
  (interactive)
  (transient-bib-UNIMPLEMENTED "transient-bib-entry-edit"))

(transient-define-prefix transient-bib-entry-new ()
  "Create a new BibTeX/BibLaTeX entry."
  :info-manual "(transient-bib)New Entry"
  ["Entry Type"
   ("a" "Article" bibtex-Article)
   ("p" "PLACEHOLDER" transient-bib-entry-new-placeholder)])

(transient-define-prefix transient-bib-entry ()
  "Create and manipulate BibTeX/BibLaTeX entries."
  :info-manual "(transient-bib)Entries"
  ["Create"
   [("n" "New" transient-bib-entry-new)]]
  ["Edit"
   [("e" "Edit" transient-bib-entry-edit)]])

(provide 'transient-bib-entry)
;;; transient-bib-entry.el ends here
