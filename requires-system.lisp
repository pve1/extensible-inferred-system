(in-package :extensible-inferred-system)

;;;; A simple system that looks for forms like
;;;;
;;;;    (extensible-inferred-system:requires :alexandria "foo" "bar").
;;;;
;;;; The "requires" symbol is tested using string-equal, so
;;;; "my-lib:requires" works too.

(defmacro requires (&rest rest)
  (declare (ignore rest))
  nil)

(defclass requires-system (system)
  ())

(defmethod extract-dependencies ((primary-system requires-system)
                                 dependency-form
                                 &key pathname)
  (declare (ignore pathname))
  (rest dependency-form))

(defmethod dependency-form-p ((primary-system requires-system) form &aux car)
  (and (listp form)
       (setf car (car form))
       (symbolp car)
       (string-equal car "REQUIRES")))

(defclass system-discovery ()
  ((full-sub-system-name :initarg :full-sub-system-name
                         :accessor full-sub-system-name
                         :initform nil)
   (sub-system-name :initarg :sub-system-name
                    :accessor sub-system-name
                    :initform nil)
   (sub-system-file :initarg :sub-system-file
                    :accessor sub-system-file
                    :initform nil)
   (sub-system-directory :initarg :sub-system-directory
                         :accessor sub-system-directory
                         :initform nil)
   (system-directory :initarg :system-directory
                     :accessor system-directory
                     :initform nil)
   (relative-path :initarg :relative-path
                  :accessor relative-path
                  :initform nil)
   (dependencies :initarg :dependencies
                 :accessor dependencies
                 :initform nil)
   (component-type :initarg :component-type
                   :accessor component-type
                   :initform nil)
   (file-type :initarg :file-type
              :accessor file-type
              :initform nil)))

(defmethod discover-system ((primary-system requires-system) full-sub-system-name)
  (let* ((sub-system-name (subseq full-sub-system-name
                                  (1+ (length (asdf:component-name
                                               primary-system)))))
         (relative-path sub-system-name) ; May change below.
         (system-directory (asdf:component-pathname primary-system))
         (component-type (class-name
                          (asdf/defsystem:class-for-type primary-system
                                                         :file)))
         (file-type (asdf:file-type (make-instance component-type)))
         (sub-system-name-ends-in-slash ; Indicates directory.
           (eql #\/ (uiop:last-char sub-system-name)))
         ;; Are we looking at a lisp file?
         (sub-system-file
           (uiop:file-exists-p
            (uiop:subpathname system-directory
                              sub-system-name
                              :type file-type)))
         ;; Or a directory, in which case the heuristic below is
         ;; applied.
         (sub-system-directory
           (uiop:directory-exists-p
            (uiop:subpathname system-directory sub-system-name))))

    ;; FULL-SUB-SYSTEM-NAME: "some-library/foo/util"
    ;; SUB-SYSTEM-NAME: "foo/util"
    ;; SUB-SYSTEM-FILE: "/full/path/to/some-library/foo/util.lisp"
    ;; SYSTEM-DIRECTORY: "/full/path/to/some-library/"
    ;; FILE-TYPE: "lisp"

    ;; Heuristic to determine sub-system-file.
    ;; Simple case: "/my-lib/foo.lisp" was found.
    (cond ((and (not sub-system-name-ends-in-slash)
                sub-system-file)
           t)

          ;; Sub-system is a directory. Expand like so:
          ;; "/my-lib/foo/" -> "/my-lib/foo/foo.lisp"
          ((and (or (not sub-system-file)
                    sub-system-name-ends-in-slash)
                sub-system-directory)
           (let ((last-directory        ; the "foo" part
                   (car (last (pathname-directory
                               sub-system-directory)))))
             (setf relative-path        ; "foo" -> "foo/foo"
                   (concatenate 'string
                                sub-system-name
                                (if sub-system-name-ends-in-slash
                                    ""
                                    "/")
                                last-directory))
             ;; Update sub-system-file to reflect new location.
             (setf sub-system-file
                   (uiop:subpathname system-directory
                                     relative-path
                                     :type file-type)))))

    (when (uiop:file-exists-p sub-system-file)
      (let ((dependencies
              ;; Canonicalize relative dependencies.
              (loop :for dep :in (read-dependencies primary-system
                                                    sub-system-file)
                    :collect
                       (asdf:coerce-name
                        (etypecase dep
                          (string
                           (concatenate 'string
                                        (asdf:component-name
                                         primary-system)
                                        "/"
                                        sub-system-name
                                        (if sub-system-name-ends-in-slash
                                            ""
                                            "/")
                                        dep))
                          (symbol dep))))))

        (make-instance 'system-discovery
          :full-sub-system-name full-sub-system-name
          :sub-system-name sub-system-name
          :relative-path relative-path
          :dependencies dependencies
          :system-directory system-directory
          :sub-system-file sub-system-file
          :sub-system-directory sub-system-directory
          :file-type file-type
          :component-type component-type)))))

(defmethod maybe-use-existing-system ((existing-system requires-system)
                                      (discovery system-discovery))
    ;; Should we check around-compile-hook?
    (and (eq (type-of existing-system) 'requires-system)
         (equal (asdf:component-name existing-system)
                (full-sub-system-name discovery))
         (uiop:pathname-equal
          (system-directory discovery)
          (asdf:component-pathname existing-system))
         (equal (dependencies discovery)
                (asdf:component-sideway-dependencies
                 existing-system))
         ;; Single child of type cl-source-file?
         (let (children child)
           (and (setf children (asdf:component-children existing-system))
                (setf child (first children))
                (null (cdr children))
                (eq (type-of child) 'asdf:cl-source-file)
                (uiop:pathname-equal (sub-system-file discovery)
                                     (asdf:component-pathname
                                      child))))
         existing-system))

(defmethod generate-fresh-sub-system ((primary-system requires-system) discovery)
  (eval
   `(asdf:defsystem ,(full-sub-system-name discovery)
      :class ,(class-name (class-of primary-system))
      :default-component-class asdf:cl-source-file
      :source-file ,(asdf:system-source-file primary-system)
      :pathname ,(system-directory discovery) ;; Primary system
      :depends-on ,(dependencies discovery)
      ;; :around-compile ,around-compile
      :components ((,(component-type discovery)
                    ,(file-type discovery)
                    :pathname ,(relative-path discovery))))))
