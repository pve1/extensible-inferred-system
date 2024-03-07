(in-package :extensible-inferred-system)

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
