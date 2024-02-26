(in-package :extensible-inferred-system)

;;;; A simple system that looks for forms like
;;;;
;;;;    (extensible-inferred-system:requires :alexandria "foo" "bar").
;;;;
;;;; The "requires" symbol is tested using string-equal, so
;;;; my-lib:requires works too.

(defmacro requires (&rest rest)
  (declare (ignore rest))
  nil)

(defclass requires-system (system)
  ())

(defmethod extract-dependencies ((primary-system requires-system)
                                 dependency-form)
   (loop :for dep :in (rest dependency-form)
        :collect (asdf:coerce-name
                  (etypecase dep
                    (string (concatenate 'string
                                         (asdf:component-name primary-system)
                                         "/"
                                         dep))
                    (symbol dep)))))

(defmethod dependency-form-p ((primary-system requires-system) form &aux car)
  (and (listp form)
       (setf car (car form))
       (symbolp car)
       (string-equal car "REQUIRES")))

(defgeneric maybe-use-existing-system (existing-system candidate-properties)
  (:method (existing-system candidate-properties)
    nil)
  (:documentation "Compares EXISTING-SYSTEM to CANDIDATE-PROPERTIES. If the properties
match those in the system, return EXISTING-SYSTEM, otherwise NIL. The
purpose of this function is to avoid having to generate a new system
instance when doing ASDF:FIND-SYSTEM if a previously registered system
can be used."))

(defmethod maybe-use-existing-system ((existing-system requires-system)
                                      candidate-properties)
  (destructuring-bind (&key system-directory
                            dependencies
                            full-sub-system-name
                            sub-system-file) candidate-properties
    ;; Should we check around-compile-hook?
    (and (eq (type-of existing-system) 'requires-system)
         (equal (asdf:component-name existing-system)
                full-sub-system-name)
         (uiop:pathname-equal
          system-directory
          (asdf:component-pathname existing-system))
         (equal dependencies (asdf:component-sideway-dependencies
                              existing-system))
         ;; Single child of type cl-source-file?
         (let (children child)
           (and (setf children (asdf:component-children existing-system))
                (setf child (first children))
                (null (cdr children))
                (eq (type-of child) 'asdf:cl-source-file)
                (uiop:pathname-equal sub-system-file
                                     (asdf:component-pathname
                                      child))))
         existing-system)))

(defclass system-discovery ()
  ((full-sub-system-name :initarg :full-sub-system-name
                         :accessor full-sub-system-name
                         :initform nil)
   (system-directory :initarg :system-directory
                     :accessor system-directory
                     :initform nil)
   (component-type :initarg :component-type
                   :accessor component-type
                   :initform nil)
   (file-type :initarg :file-type
              :accessor file-type
              :initform nil)
   (sub-system-file :initarg :sub-system-file
                    :accessor sub-system-file
                    :initform nil)
   (sub-system-directory :initarg :sub-system-directory
                         :accessor sub-system-directory
                         :initform nil)))

(defgeneric discover-system (primary-system full-sub-system-name)
  (:method ((primary-system requires-system) full-sub-system-name)
    (let* ((sub-system-name (subseq full-sub-system-name
                                    (1+ (length (asdf:component-name
                                                 primary-system)))))
           (system-directory (asdf:component-pathname primary-system))
           (component-type (class-name
                            (asdf/defsystem:class-for-type primary-system
                                                           :file)))
           (file-type (asdf:file-type (make-instance component-type)))
           (sub-system-name-ends-in-slash ; Indicates directory.
             (eql #\/ (uiop:last-char sub-system-name)))
           (sub-system-file
             (uiop:file-exists-p
              (uiop:subpathname system-directory
                                sub-system-name
                                :type file-type)))
           (sub-system-directory
             (uiop:directory-exists-p
              (uiop:subpathname system-directory sub-system-name))))

      ;; Heuristic to determine sub-system-file.
      ;; Simple case: "/my-lib/foo.lisp" was found.
      (cond ((and (not sub-system-name-ends-in-slash)
                  sub-system-file)
             t)

            ;; Sub system is a directory. Expand like so:
            ;; "/my-lib/foo" -> "/my-lib/foo/foo.lisp"
            ((and (or (not sub-system-file)
                      sub-system-name-ends-in-slash)
                  sub-system-directory)
             (let ((last-directory ; the "foo" part
                     (car (last (pathname-directory
                                 sub-system-directory)))))
               (setf sub-system-file
                     (uiop:subpathname
                      system-directory
                      (concatenate 'string
                                   sub-system-name
                                   (if sub-system-name-ends-in-slash
                                       ""
                                       "/")
                                   last-directory)
                      :type file-type)))))
      (when sub-system-file
        (make-instance 'system-discovery
          :full-sub-system-name full-sub-system-name
          :system-directory system-directory
          :sub-system-file sub-system-file
          :sub-system-directory sub-system-directory
          :file-type file-type
          :component-type component-type))))
  (:documentation "Returns a discovery instance containing preliminary information about
the system specified by FULL-SUB-SYSTEM-NAME. Returns NIL if no
applicable system is found."))

(defmethod infer-sub-system ((primary-system requires-system) full-sub-system-name)
  (let* ((sub-system-name (subseq full-sub-system-name
                                  (1+ (length (asdf:component-name
                                               primary-system)))))
         (system-directory (asdf:component-pathname primary-system))
         (component-type (class-name
                          (asdf/defsystem:class-for-type primary-system
                                                         :file)))
         (file-type (asdf:file-type (make-instance component-type)))
         (sub-system-file
           (uiop:probe-file*
            (uiop:subpathname system-directory
                              sub-system-name
                              :type file-type)
            :truename asdf:*resolve-symlinks*)))

    ;; FULL-SUB-SYSTEM-NAME: "some-library/foo/util"
    ;; SUB-SYSTEM-NAME: "foo/util"
    ;; SUB-SYSTEM-FILE: "/full/path/to/some-library/foo/util.lisp"
    ;; SYSTEM-DIRECTORY: "/full/path/to/some-library/"
    ;; FILE-TYPE: "lisp"

    (cond ((uiop:directory-pathname-p sub-system-file))
          ;; "/foo/bar/"

          ;; "/.../foo/bar.lisp"
          ((uiop:file-pathname-p sub-system-file)
           (let* ((dependencies (read-dependencies primary-system
                                                   sub-system-file))
                  (existing-system (asdf:registered-system
                                    full-sub-system-name)))
             (or (maybe-use-existing-system
                  existing-system
                  (list :system-directory system-directory
                        :dependencies dependencies
                        :full-sub-system-name full-sub-system-name
                        :sub-system-file sub-system-file))
                 (eval
                  `(asdf:defsystem ,full-sub-system-name
                     :class ,(class-name (class-of primary-system))
                     :default-component-class asdf:cl-source-file
                     :source-file ,(asdf:system-source-file primary-system)
                     :pathname ,system-directory ;; Primary system
                     :depends-on ,dependencies
                     ;; :around-compile ,around-compile
                     :components ((,component-type
                                   ,file-type
                                   :pathname ,sub-system-name))))))))))

