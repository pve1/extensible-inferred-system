(defpackage #:extensible-inferred-system
  (:use #:cl)
  (:export "SYSTEM"
           "SYSDEF-SYSTEM-SEARCH"

           "DEPENDENCY-FORM-P"
           "READ-DEPENDENCIES"
           "EXTRACT-DEPENDENCIES"
           "INFER-SUB-SYSTEM"

           "REQUIRES"
           "REQUIRES-SYSTEM"
           "MAYBE-USE-EXISTING-SYSTEM"))

(in-package #:extensible-inferred-system)

(defclass system (asdf:system)
  ())

(defgeneric dependency-form-p (primary-system form)
  (:documentation "Returns non-nil if FORM specifies dependencies."))

(defgeneric extract-dependencies (primary-system dependency-form)
  (:documentation "Returns dependencies specified by DEPENDENCY-FORM."))

(defgeneric read-dependencies (primary-system file)
  (:method ((primary-system system) file)
    (let ((form (uiop:with-input-file (stream file)
                  (loop :for form = (read stream nil nil) :while form
                        :when (dependency-form-p primary-system form)
                        :return form))))
      (extract-dependencies primary-system form)))
  (:documentation "Searches FILE for a form satisfying DEPENDENCY-FORM-P. Returns
dependencies specified by the first form found."))

(defgeneric infer-sub-system (primary-system full-sub-system-name)
  (:method (primary-system full-sub-system-name)
    nil)
  (:documentation "Given FULL-SUB-SYSTEM-NAME (like \"my-library/foo\"), generates an
asdf system instance that corresponds to that name if a suitable
source file can be found. Returns NIL otherwise."))

(defun sysdef-system-search (system-name)
  ;; If system-name is "my-library/foo", then the primary name is
  ;; "my-library".
  (let ((primary-name (asdf:primary-system-name system-name)))
    ;; Don't do anything if system-name wasn't of the form
    ;; "my-library/foo".
    (unless (equal primary-name system-name)
      ;; Default method returns NIL.
      (infer-sub-system (asdf:find-system primary-name) system-name))))

(pushnew 'sysdef-system-search asdf:*system-definition-search-functions*)
