;;; transient-bib-entry.el --- Create and edit bibliography entries -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Karl Hallsby <karl@hallsby.com>
;; Maintainer: Karl Hallsby <karl@hallsby.com>

;;; Commentary:
;; This defines and implements the necessary functions and keybindings for
;; searching a bibliography file making use of `transient-bib'.

;;; Code:
(require 'transient-bib-mode)

(defcustom transient-bib-entry-clean-on-exit t
  "Whether to clean all bibliography entries before saving to the main bibliography file."
  :type 'boolean
  :require 'bibtex
  :require 'transient-bib)

(defun transient-bib-entry-exit-buffer (&optional clean)
  "Exit the bibliography's entry buffer and optionally CLEAN the entry using `bibtex-clean-entry'."
  (interactive)
  (when clean
    (bibtex-clean-entry))
  (save-buffer)
  (kill-buffer))

(defmacro transient-bib-entry ()
  "Open the bibliography entry in a new RW-allowed buffer which uses
`bibtex-mode'."
  (let ((entry-buffer (get-buffer-create "transient-bib-entry")))
    (setq major-mode bibtex-mode)
    (set-buffer-major-mode entry-buffer)))

(defun transient-bib-entry-new-placeholder ()
  "PLACEHOLDER function for creating a new BibTeX/BibLaTeX entry."
  (interactive)
  (transient-bib-UNIMPLEMENTED "transient-bib-entry-new-placeholder"))

(defun transient-bib-entry-new-article ()
  "Create a new article bibliography entry.

Opens a new read/write capable buffer in `bibtex-mode' for entering the entry.
Once done, the contents of the buffer are copied back to the main bibliography
buffer and the file is automatically saved."
  (interactive)
  (let ((bib-file-buffer (current-buffer))
        (entry-buffer (generate-new-buffer "*transient-bib-entry*")))
    ;; (with-temp-buffer ;; with-temp-buffer will create a temporary buffer
    (save-current-buffer
      ;; TODO: Verification of parent buffer for indirect buffer?
      (switch-to-buffer entry-buffer) ;; TODO: use pop-to-buffer-same-window instead?
      (setq inhibit-read-only t)
      (bibtex-mode) ;; Set up BibTeX major mode
      (bibtex-set-dialect) ;; No args to use BibTeX or user-defined values
      (bibtex-Article)
      ;; TODO: Wait until C-c C-c keybinding is pressed before finishing up here.
      ;; NOTE: C-C C-c is bound to bibtex-clean-entry when in bibtex-mode
      ;; (bibtex-clean-entry)
      ;; (setq inhibit-read-only nil) ;; Make the entry buffer read-only again
      ;; TODO: Ensure switching back to main bib file at end.
      )))

;;;###autoload
(defun transient-bib-entry-edit ()
  "Doing things wrong."
  (interactive)
  (transient-bib-UNIMPLEMENTED "transient-bib-entry-edit"))

(transient-define-prefix transient-bib-entry-new ()
  "Create a new BibTeX/BibLaTeX entry."
  :info-manual "(transient-bib)New Entry"
  ["Entry Type"
   ("a" "Article" transient-bib-entry-new-article)
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
