(in-package :extensible-inferred-system)

;;;; A system similar to requires-system, except dependencies are
;;;; specified using the reader conditional expression #+requires
;;;; (...) at the beginning of a file.
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
  (let* ((*features* (cons :requires *features*))
         (first-line (uiop:with-input-file (stream file)
                       (loop :for c = (read-char stream nil nil)
                             :while c
                             :do (unless (member c '(#\space #\newline #\tab))
                                   (unread-char c stream)
                                   (loop-finish)))
                       (read-line stream nil "")))
         (requires "#+requires")
         (mismatch (mismatch requires
                             first-line
                             :test #'char-equal)))
    (when (or (null mismatch)
              (= (length requires)
                 mismatch))
      (call-next-method))))

(defmethod extract-dependencies ((primary-system feature-system)
                                 dependency-form
                                 &key pathname)
  (declare (ignore pathname))
  dependency-form)

(defmethod dependency-form-p ((primary-system feature-system) form)
  (listp form))
