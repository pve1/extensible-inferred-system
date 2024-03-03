(defpackage #:extensible-inferred-system
  (:use #:cl)
  (:export "SYSTEM"
           "SYSDEF-SYSTEM-SEARCH"

           "DEPENDENCY-FORM-P"
           "READ-DEPENDENCIES"
           "EXTRACT-DEPENDENCIES"
           "MAYBE-USE-EXISTING-SYSTEM"
           "GENERATE-FRESH-SUB-SYSTEM"
           "DISCOVER-SYSTEM"
           "INFER-SUB-SYSTEM"

           "REQUIRES"
           "REQUIRES-SYSTEM"))

(in-package #:extensible-inferred-system)

(defclass system (asdf:system)
  ()
  (:documentation "The base system class. Provides default behaviour for
INFER-SUB-SYSTEM and READ-DEPENDENCIES."))

;;; Dependency handling.

(defgeneric dependency-form-p (primary-system form)
  (:documentation "Returns non-nil if FORM specifies dependencies in the context of
PRIMARY-SYSTEM. Is used by READ-DEPENDENCIES to determine when to call
EXTRACT-DEPENDENCIES."))

(defgeneric extract-dependencies (primary-system dependency-form &key pathname)
  (:documentation "Returns dependencies specified by DEPENDENCY-FORM. Is called by
READ-DEPENDENCIES when a dependency-form is found."))

(defgeneric read-dependencies (primary-system file)
  (:method ((primary-system system) file)
    (let ((form (uiop:with-input-file (stream file)
                  (loop :for form = (read stream nil nil) :while form
                        :when (dependency-form-p primary-system form)
                        :return form))))
      (extract-dependencies primary-system form :pathname file)))
  (:documentation "Searches FILE for a form satisfying DEPENDENCY-FORM-P. Returns
dependencies specified by the first form found."))

;;; Inferring systems.

(defgeneric discover-system (primary-system full-sub-system-name)
  (:documentation "Returns information about the sub-system FULL-SUB-SYSTEM-NAME or NIL
if no system can be found."))

(defgeneric maybe-use-existing-system (primary-system discovery)
  (:documentation "Returns a previously registered asdf system that matches the
information in DISCOVERY, if one exists."))

(defgeneric generate-fresh-sub-system (primary-system discovery)
  (:documentation "Generates a new asdf:system instance that corresponds to DISCOVERY."))

(defgeneric infer-sub-system (primary-system full-sub-system-name)
  (:method (primary-system full-sub-system-name)
    nil)
  (:method ((primary-system system) full-sub-system-name)
    (let ((discovery (discover-system primary-system
                                      full-sub-system-name)))
      (when discovery
        (or (maybe-use-existing-system primary-system discovery)
            (generate-fresh-sub-system primary-system discovery)))))
  (:documentation "Given FULL-SUB-SYSTEM-NAME (like \"my-library/foo\"), returns an asdf
system instance that corresponds to that name if a suitable source
file can be found. Returns NIL otherwise."))

(defun sysdef-system-search (system-name)
  ;; If system-name is "my-library/foo", then the primary name is
  ;; "my-library".
  (let ((primary-name (asdf:primary-system-name system-name)))
    ;; Don't do anything if system-name wasn't of the form
    ;; "my-library/foo", i.e. not a primary name.
    (unless (equal primary-name system-name)
      ;; Default method returns NIL.
      (infer-sub-system (asdf:find-system primary-name) system-name))))

(pushnew 'sysdef-system-search asdf:*system-definition-search-functions*)
