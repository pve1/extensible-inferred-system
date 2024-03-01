(in-package :extensible-inferred-system)

;;;; A simple system that looks for forms like
;;;;
;;;;    (extensible-inferred-system:requires :alexandria "foo" "bar").
;;;;
;;;; The "requires" symbol is tested using string-equal, so
;;;; "my-lib:requires" works too.

(defclass requires-system (system)
  ())

;;; Support loading lisp files.

(defvar *asdf-active* nil)
(defvar *anonymous-requires-system-class* 'requires-system)

(defmethod asdf:operate :around (operation (system requires-system)
                                 &key)
  (let ((*asdf-active* t))
    (call-next-method)))

(defun ensure-anonymous-requires-system (file)
  (eval `(asdf:defsystem "extensible-inferred-system-anonymous-requires-system"
           :class ,(string-downcase
                    (with-standard-io-syntax
                      (prin1-to-string *anonymous-requires-system-class*)))
           :pathname ,(directory-namestring file)
           :source-file
           ,(merge-pathnames
             (make-pathname :name "extensible-inferred-system-anonymous-requires-system"
                            :type "asd")
             file))))

(defun load-anonymous-requires-system-dependencies (file)
  (let* ((system (ensure-anonymous-requires-system file))
         (sub-system-name (concatenate 'string
                                       (asdf:component-name system)
                                       "/"
                                       (pathname-name file))))
    (asdf:operate 'asdf:prepare-op sub-system-name)))

;; Idea: Autoload dependencies when loading a lisp file. REQUIRES is
;; defined here, but see below for definition of
;; LOAD-ANONYMOUS-REQUIRES-SYSTEM-DEPENDENCIES.

(defmacro requires (&rest dependencies)
  (declare (ignore dependencies))
  (if *asdf-active*
      nil
      `(eval-when (:execute) ; Only scripts
         (unless *asdf-active*
           (load-anonymous-requires-system-dependencies
            *load-truename*)))))

;;; Dealing with dependencies.

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

;;; Discovering potential systems.

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
   (sub-system-relative-directory :initarg :sub-system-relative-directory
                                  :accessor sub-system-relative-directory
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

;;; This method returns an instance of SYSTEM-DISCOVERY. It is used to
;;; either find an existing requires-system or to generate a fresh one.

(defmethod discover-system ((primary-system requires-system) full-sub-system-name)
  (let* ((sub-system-name (subseq full-sub-system-name
                                  (1+ (length (asdf:component-name
                                               primary-system)))))
         (system-directory (asdf:component-pathname primary-system))
         (relative-path sub-system-name) ; May change below.
         (sub-system-relative-directory
           (if (position #\/ sub-system-name)
               (subseq relative-path
                       0
                       (1+ (position #\/ relative-path
                                     :from-end t)))
               ""))
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
         (sub-system-directory-full-path
           (uiop:directory-exists-p
            (uiop:subpathname system-directory sub-system-name))))

    ;; SYSTEM-DIRECTORY: "/full/path/to/some-library/"
    ;; FULL-SUB-SYSTEM-NAME: "some-library/foo/util"
    ;; SUB-SYSTEM-NAME: "foo/util"
    ;; SUB-SYSTEM-RELATIVE-DIRECTORY: "foo/"
    ;; SUB-SYSTEM-FILE: "/full/path/to/some-library/foo/util.lisp"
    ;; FILE-TYPE: "lisp"

    ;; Heuristic to determine sub-system-file.
    ;; Simple case: "/my-lib/foo.lisp" was found.
    (cond ((and (not sub-system-name-ends-in-slash)
                sub-system-file)
           t)
          ;; Sub-system is a directory. Expand like so:
          ;; "/my-lib/foo/bar/" -> "/my-lib/foo/bar/bar.lisp"
          ((and (or (not sub-system-file)
                    sub-system-name-ends-in-slash)
                sub-system-directory-full-path)
           (let ((last-directory        ; the "bar/" part
                   (car (last (pathname-directory
                               sub-system-directory-full-path)))))
             ;; For defsystem component specification.
             (setf relative-path        ; "bar/" -> "bar/bar"
                   (concatenate 'string
                                sub-system-name
                                (if sub-system-name-ends-in-slash
                                    ""
                                    "/")
                                last-directory))
             ;; For canonicalization below.
             (setf sub-system-relative-directory
                   (concatenate 'string
                                sub-system-relative-directory
                                last-directory
                                "/"))
             ;; Update sub-system-file to reflect new location.
             (setf sub-system-file
                   (uiop:subpathname system-directory
                                     relative-path
                                     :type file-type)))))
    ;; Finally create the instance if a source file was found.
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
                                        sub-system-relative-directory
                                        dep))
                          (symbol dep))))))
        (make-instance 'system-discovery
          :full-sub-system-name full-sub-system-name
          :sub-system-name sub-system-name
          :relative-path relative-path
          :dependencies dependencies
          :system-directory system-directory
          :sub-system-file sub-system-file
          :sub-system-relative-directory sub-system-relative-directory
          :file-type file-type
          :component-type component-type)))))

;;;; If a suitable system has already been defined, use that.

(defmethod maybe-use-existing-system ((existing-system requires-system)
                                      (discovery system-discovery))
  (let ((existing-sub-system (asdf:registered-system
                              (full-sub-system-name discovery))))
    ;; Should we check around-compile-hook?
    (and existing-sub-system
         (eq (type-of existing-sub-system) 'requires-system)
         (equal (asdf:component-name existing-sub-system)
                (full-sub-system-name discovery))
         (uiop:pathname-equal
          (system-directory discovery)
          (asdf:component-pathname existing-system))
         (equal (dependencies discovery)
                (asdf:component-sideway-dependencies
                 existing-sub-system))
         ;; Single child of type cl-source-file?
         (let (children child)
           (and (setf children (asdf:component-children existing-sub-system))
                (setf child (first children))
                (null (cdr children))
                (eq (type-of child) 'asdf:cl-source-file)
                (uiop:pathname-equal (sub-system-file discovery)
                                     (asdf:component-pathname
                                      child))))
         existing-sub-system)))

;;;; Otherwise generate a fresh system.

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

