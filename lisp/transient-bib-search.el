;;; transient-bib-search.el --- Searching within a bibliography file -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Karl Hallsby <karl@hallsby.com>
;; Maintainer: Karl Hallsby <karl@hallsby.com>

;;; Commentary:
;; This defines and implements the necessary functions and keybindings for
;; searching a bibliography file making use of `transient-bib'.

;;; Code:
(require 'transient-bib-mode)

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

(transient-define-prefix transient-bib-search ()
  "Search BibTeX/BibLaTeX file for an entry."
  :info-manual "(transient-bib)Searching"
  ["Search Type"
   ("k" "Key" transient-bib-search-key)
   ("a" "Author" transient-bib-search-author)
   ("t" "Title" transient-bib-search-title)])

(provide 'transient-bib-search)
;;; transient-bib-search.el ends here
