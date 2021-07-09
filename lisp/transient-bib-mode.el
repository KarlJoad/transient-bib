;;; transient-bib-mode.el --- Define the derived transient-bib-mode major-mode -*- lexical-binding t; coding: utf-8 -*-

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
(require 'transient)

;; Group for customizing the `transient-bib' group using Emacs' built-in
;; customization interface.
(defgroup transient-bib nil
  "Editing and controlling BibTeX/BibLaTeX files from within Emacs."
  :link '(url-link "https://github.com/KarlJoad/transient-bib")
  :group 'tools)

;;; Mode
(define-derived-mode transient-bib-mode special-mode "Transient-Bib"
  "Parent major mode from which various sub-modes inherit from."
  ;; By inheriting from special-mode, the buffer is made read-only and does not
  ;; inherit the full standard global keymap (no self-insert-command).
  ;; See (elisp) Basic Major Modes
  :group 'transient-bib)

(defun transient-bib-UNIMPLEMENTED (func-name &optional func-vars)
  "If FUNC-NAME and FUNC-VARS provided, print them and UNIMPLEMENTED to message buffer."
  (message (mapconcat 'identity (append
                                 (when (not (equal func-name ""))
                                   (list func-name))
                                 (list "UNIMPLEMENTED")
                                 (unless (equal func-vars '())
                                   (list "with"))
                                 func-vars)
                      " ")))

(defun transient-bib-entry-new-placeholder ()
  "PLACEHOLDER function for creating a new BibTeX/BibLaTeX entry."
  (interactive)
  (transient-bib-UNIMPLEMENTED "transient-bib-entry-new-placeholder"))

(defun transient-bib-search (search-param search-term)
  "Search for SEARCH-TERM against the key type SEARCH-PARAM, returning corresponding entry."
  (interactive "sSearch which keys? \nsWhat to search for: ")
  (transient-bib-UNIMPLEMENTED "transient-bib-search" '("search-param" "search-term")))

(defun transient-bib-search-key (search-key)
  "Search for SEARCH-KEY against all entry KEYS, returning a corresponding entry."
  (interactive "sEntry key: ")
  (transient-bib-UNIMPLEMENTED "transient-bib-search-key" '("search-key")))

(defun transient-bib-search-author (search-author)
  "Search for SEARCH-AUTHOR against all entry AUTHORS, returning a corresponding entry."
  (interactive "sAuthor: ")
  (transient-bib-UNIMPLEMENTED "transient-bib-search-author" '("search-author")))

(defun transient-bib-search-title (search-title)
  "Search for SEARCH-TITLE against all entry TITLES, returning a corresponding entry."
  (interactive "sTitle: ")
  (transient-bib-UNIMPLEMENTED "transient-bib-search-title" '("search-title")))

;; Define the drop-down menu at the top of the screen.
(easy-menu-define transient-bib-mode-menu transient-bib-mode-map
  "transient-bib menu"
  '("transient-bib"
    ["New Entry" transient-bib-new-entry t]
    "---"
    ("Edit Entry"
     ["Search by key" transient-bib-search-key t]
     ["Search by author" transient-bib-search-author t]
     ["Search by title" transient-bib-search-title t])))

(transient-define-prefix transient-bib-search ()
  "Search BibTeX/BibLaTeX file for an entry."
  :info-manual "(transient-bib)Searching"
  ["Search Type"
   ("k" "Key" transient-bib-search-key)
   ("a" "Author" transient-bib-search-author)
   ("t" "Title" transient-bib-search-title)])

(transient-define-prefix transient-bib-entry-new ()
  "Create a new BibTeX/BibLaTeX entry."
  :info-manual "(transient-bib)New Entry"
  ["Entry Type"
   ("a" "Article" bibtex-Article)
   ("p" "PLACEHOLDER" transient-bib-entry-new-placeholder)])

(add-to-list 'auto-mode-alist '("\\.bib\\'" . transient-bib-mode))

(provide 'transient-bib-mode)
;;; transient-bib-mode.el ends here
