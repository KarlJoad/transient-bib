;;; transient-bib-search.el --- tests for transient-bib-search
;;; Commentary:
;; A test suite for the transient-bib-search-* array of functions.
;;; Code:

(require 'ert)
(require 'transient-bib)

(ert-deftest transient-bib-UNIMPLEMENTED:empty-func-no-vars ()
  "Basic test of transient-bib-UNIMPLEMENTED function."
  (should (equal "UNIMPLEMENTED"
                 (transient-bib-UNIMPLEMENTED ""))))

(ert-deftest transient-bib-UNIMPLEMENTED:func-no-vars ()
  "Basic test of transient-bib-UNIMPLEMENTED function."
  (should (equal "transient-bib-UNIMPLEMENTED:func-no-vars UNIMPLEMENTED"
                 (transient-bib-UNIMPLEMENTED
                  "transient-bib-UNIMPLEMENTED:func-no-vars"))))

(ert-deftest transient-bib-UNIMPLEMENTED:empty-func-vars ()
  "Basic test of transient-bib-UNIMPLEMENTED function."
  (should (equal "UNIMPLEMENTED with a b c"
                 (transient-bib-UNIMPLEMENTED "" '("a" "b" "c")))))

(ert-deftest transient-bib-UNIMPLEMENTED:func-vars ()
  "Basic test of transient-bib-UNIMPLEMENTED function."
  (should (equal "transient-bib-UNIMPLEMENTED:func-vars UNIMPLEMENTED with a b c"
                 (transient-bib-UNIMPLEMENTED "transient-bib-UNIMPLEMENTED:func-vars"
                                              '("a" "b" "c")))))

;;; transient-bib-tests.el ends soon
(provide 'transient-bib-tests)
;; End:
;;; transient-bib-search.el ends here
