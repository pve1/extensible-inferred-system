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
  ((feature-expression
    :initarg :feature-expression
    :accessor feature-expression
    :initform "REQUIRES")
   (skip-comments :initarg :skip-comments
                  :accessor feature-system-skip-comments-p
                  :initform t)))

(defmethod feature-expression-symbol ((f feature-system))
  (let ((*package* (find-package :keyword)))
    (read-from-string (feature-expression f))))

(defgeneric feature-system-skip-comments (system stream)
  (:method ((system feature-system) stream)
    (loop :while (eql (peek-char t stream nil) #\;)
          :do (read-line stream nil))))

(defmethod read-dependencies :around ((system feature-system) file)
  (let* ((*features* (cons (feature-expression-symbol system) ; :requires
                           *features*))
         (correct-reader-conditional-found
           (uiop:with-input-file (stream file)
             (when (feature-system-skip-comments-p system)
               (feature-system-skip-comments system stream))
             (peek-char t stream nil)
             (and (eql #\# (read-char stream nil nil))
                  (eql #\+ (read-char stream nil nil))
                  (eq (let ((*package* (find-package :keyword)))
                        (read stream nil stream))
                      (feature-expression-symbol system))))))
    (when correct-reader-conditional-found
      (call-next-method))))

(defmethod extract-dependencies ((primary-system feature-system)
                                 dependency-form
                                 &key pathname)
  (declare (ignore pathname))
  dependency-form)

(defmethod dependency-form-p ((primary-system feature-system) form)
  (listp form))
