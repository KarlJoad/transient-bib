;;; transient-bib-mode.el --- Define the derived transient-bib-mode major-mode -*- lexical-binding: t; coding: utf-8 -*-

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
;; External Dependencies
(require 'bibtex)
(require 'transient)

;; Group for customizing the `transient-bib' group using Emacs' built-in
;; customization interface.
(defgroup transient-bib nil
  "Editing and controlling BibTeX/BibLaTeX files from within Emacs."
  :link '(url-link "https://github.com/KarlJoad/transient-bib")
  :group 'tools)

(defcustom transient-bib-mode-hook nil
  "List of functions to call on entry to transient-bib mode."
  :group 'transient-bib
  :type 'hook)

;;; Main keymap. Used when viewing bibliography document.
;; Must be defined before the major mode, so that the keybindings are set.
(defvar transient-bib-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "e") 'transient-bib-entry)
    (define-key map (kbd "s") 'transient-bib-search)
    (define-key map (kbd "s-k") 'transient-bib-test-Karl)
    (define-key map (kbd "?") 'transient-bib-dispatch)
    map)
  "Parent keymap for all keymaps of modes derived from `transient-bib-mode'.")

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

(transient-define-prefix transient-bib-dispatch ()
  "Invoke a main command to dispatch to a sub-function."
  :info-manual "(transient-bib)Top"
  ["Entry Commands"
   ("e" "Entry" transient-bib-entry)]
  ["Searching"
   ("s" "Search" transient-bib-search)])

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bib\\'" . transient-bib-mode))

(provide 'transient-bib-mode)
;; Internal dependencies. MUST be specified at end, otherwise circular dependency
(require 'transient-bib-search)
(require 'transient-bib-entry)

;;; transient-bib-mode.el ends here
