(in-package :extensible-inferred-system)

;;;; A system similar to requires-system, except dependencies are
;;;; specified using the feature expression #+requires (...)  at the
;;;; beginning of a file.
;;;;
;;;; Example:
;;;;
;;;; #+requires (:alexandria
;;;;             :cl-ppcre
;;;;             "macros"
;;;;             "util"))

(defclass feature-system (requires-system)
  ())

(defmethod read-dependencies :around ((system feature-system) file)
  (let ((*features* (cons :requires *features*)))
    (call-next-method)))

(defmethod extract-dependencies ((primary-system feature-system)
                                 dependency-form
                                 &key pathname)
  (declare (ignore pathname))
  dependency-form)

(defmethod dependency-form-p ((primary-system feature-system) form)
  (listp form))
