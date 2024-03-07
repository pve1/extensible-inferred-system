(in-package :extensible-inferred-system)

;;;; A system similar to requires-system, except dependencies are
;;;; specified in a comment at the beginning of a file. The format of
;;;; the comment is "requires dep1 dep2 ..." and it may span multiple
;;;; lines. A non-comment line ends the dependency specification.
;;;;
;;;; Example (indentation optional):
;;;;
;;;; ;;;; Requires
;;;; ;;;;
;;;; ;;;;   alexandria
;;;; ;;;;   cl-ppcre
;;;; ;;;;   "macros"
;;;; ;;;;   "util"

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :extensible-inferred-system-temporary)
    (make-package :extensible-inferred-system-temporary)))

(defclass comment-system (requires-system)
  ((skip-initial-blank-lines :initarg :skip-initial-blank-lines
                             :accessor skip-initial-blank-lines
                             :initform nil)
   (requires-symbol-name :initarg :requires-symbol-name
                         :accessor requires-symbol-name
                         :initform "REQUIRES")))

(defgeneric requires-symbol (system))

(defmethod requires-symbol ((system comment-system))
  (intern (requires-symbol-name system)
          :extensible-inferred-system-temporary))

(defmethod extract-dependencies ((primary-system comment-system)
                                 dependency-form
                                 &key pathname)
  (declare (ignore pathname))
  (rest dependency-form))

(defmethod dependency-form-p ((primary-system comment-system) form &aux car)
  (and (listp form)
       (setf car (car form))
       (symbolp car)
       (eq (requires-symbol primary-system) car)))

(defmethod read-dependencies ((primary-system comment-system) file)
  (let* ((string (with-output-to-string (s)
                   (princ #\( s)
                   (uiop:with-input-file (stream file)
                     (loop :with scanning-for-comment =
                              (skip-initial-blank-lines primary-system)
                           :for line = (read-line stream nil stream)
                           :while (or (and scanning-for-comment
                                           (zerop (length line)))
                                      (and (not (zerop (length line)))
                                           (eql #\; (aref line 0))))
                           ;; Skip initial empty lines if the system
                           ;; allows.
                           :do (when scanning-for-comment
                                 (cond ((equal "" line)
                                        nil)
                                       ((search (requires-symbol-name
                                                 primary-system)
                                                line
                                                :test #'char-equal)
                                       ;; Found something interesting.
                                       (setf scanning-for-comment nil))
                                       ;; Found something uninteresting.
                                       (t (loop-finish))))
                               (unless scanning-for-comment
                                 (map nil (lambda (x)
                                            (unless (eql x #\;)
                                              (princ x s)))
                                      line))))
                   (princ #\) s)))
         (dependency-form
           (let ((*package* (find-package :extensible-inferred-system-temporary))
                 (*read-eval* nil))
             (read-from-string string))))
    (when (dependency-form-p primary-system dependency-form)
      (prog1 (extract-dependencies primary-system dependency-form)
        (do-symbols (sym :extensible-inferred-system-temporary)
          (unintern sym :extensible-inferred-system-temporary))))))
