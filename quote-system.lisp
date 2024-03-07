(in-package :extensible-inferred-system)

;;;; A system similar to requires-system, except dependencies are
;;;; specified with a QUOTE form at the beginning of a file.
;;;;
;;;; Example:
;;;;
;;;; (quote (:alexandria
;;;;         :cl-ppcre
;;;;         "macros"
;;;;         "util"))

(defclass quote-system (requires-system)
  ())

(defmethod extract-dependencies ((primary-system quote-system)
                                 dependency-form
                                 &key pathname)
  (declare (ignore pathname))
  (second dependency-form))

(defmethod dependency-form-p ((primary-system quote-system) form &aux car)
  (and (listp form)
       (setf car (car form))
       (symbolp car)
       (eq 'quote car)))
