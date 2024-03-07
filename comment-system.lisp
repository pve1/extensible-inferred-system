(in-package :extensible-inferred-system)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :extensible-inferred-system-user)
    (make-package :extensible-inferred-system-user)))

(defclass comment-system (requires-system)
  ())

(defmethod extract-dependencies ((primary-system comment-system)
                                 dependency-form
                                 &key pathname)
  (declare (ignore pathname))
  (rest dependency-form))

(defmethod dependency-form-p ((primary-system comment-system) form &aux car)
  (and (listp form)
       (setf car (car form))
       (symbolp car)
       (eq 'extensible-inferred-system-user::requires car)))

(defmethod read-dependencies ((primary-system comment-system) file)
  (let* ((string (with-output-to-string (s)
                   (princ #\( s)
                   (uiop:with-input-file (stream file)
                     (loop :for line = (read-line stream nil stream)
                           :while (and (not (zerop (length line)))
                                       (eql #\; (aref line 0)))
                           :do (map nil (lambda (x)
                                          (unless (eql x #\;)
                                            (princ x s)))
                                    line)))
                   (princ #\) s)))
         (dependency-form
           (let ((*package* (find-package :extensible-inferred-system-user)))
             (read-from-string string))))
    (when (dependency-form-p primary-system dependency-form)
      (extract-dependencies primary-system dependency-form))))

