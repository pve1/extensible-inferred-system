(in-package :extensible-inferred-system)

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
