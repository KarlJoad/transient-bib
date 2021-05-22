;;; transient-bib-mode.el --- Define the derived transient-bib-mode major-mode -*- lexical-binding t -*-

;; Author: Karl Hallsby <karl@hallsby.com>
;; Maintainer: Karl Hallsby <karl@hallsby.com>

;;; Commentary:
;; This library defines and implements the abstract major-mode `transient-bib'
;; which is used in conjunction with Emacs' `bibtex-mode' major-mode. Using this
;; abstract base, major modes can be created for working with and editing both
;; BibTeX and BibLaTeX files.
;; This package provides a `transient' interface to these bibliography files,
;; similar to how `transient' is used by `magit' to assist in working with Git.

;;; Code:

(require 'bibtex)

;; Group for customizing the `transient-bib' group using Emacs' built-in
;; customization interface.
(defgroup transient-bib nil
  "Editing and controlling BibTeX/BibLaTeX files from within Emacs."
  :link '(url-link "https://github.com/KarlJoad/transient-bib")
  :group 'tools)

;;; Mode
(define-derived-mode transient-bib-mode nil "Transient-Bib"
  "Parent major mode from which various sub-modes inherit from."
  :group 'transient-bib)

(provide 'transient-bib-mode)
;;; transient-bib-mode.el ends here
