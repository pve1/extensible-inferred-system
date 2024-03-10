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
   (initial-symbol-name :initarg :initial-symbol-name
                        :accessor initial-symbol-name
                        :initform "REQUIRES")
   (ignore-lines :initarg :ignore-lines
                 :accessor ignore-lines
                 :initform nil)))

(defgeneric initial-symbol (system))

(defmethod initial-symbol ((system comment-system))
  (let ((*package* (find-package :extensible-inferred-system-temporary)))
    (read-from-string (initial-symbol-name system))))

(defmethod extract-dependencies ((primary-system comment-system)
                                 dependency-form
                                 &key pathname)
  (declare (ignore pathname))
  (rest dependency-form))

(defmethod dependency-form-p ((primary-system comment-system) form)
  t) ; Already checked by read-dependencies.

(defgeneric ignore-line-p (system line)
  (:method (system line)
    nil)
  (:method ((system comment-system) line)
    (let ((ignore-lines (ignore-lines system)))
      (when (stringp ignore-lines)
        (setf ignore-lines (list ignore-lines)))
      (some (lambda (ignore)
              (search ignore line))
            ignore-lines))))

(defmethod read-dependencies ((primary-system comment-system) file)
  (let* ((string (with-output-to-string (s)
                   ;; Collect the initial comment, skipping ";".
                   (uiop:with-input-file (stream file)
                     (peek-char t stream nil)
                     (loop :for line = (read-line stream nil "")
                           :while (and (not (zerop (length line)))
                                       (eql #\; (aref line 0)))
                           :unless (ignore-line-p primary-system
                                                  line)
                           :do (map nil (lambda (x)
                                          (unless (eql x #\;)
                                            (princ x s)))
                                    line)))))
         (dependency-form
           (with-input-from-string (stream string)
             (let ((*package* (find-package :extensible-inferred-system-temporary))
                   (*read-eval* nil)
                   initial-symbol
                   rest)
               ;; Check the initial symbol first and only read the
               ;; rest if it's ok.
               (setf initial-symbol (read stream nil stream))
               (when (eq initial-symbol (initial-symbol primary-system))
                 (setf rest (loop :for object = (read stream nil stream)
                                  :until (eq object stream)
                                  :collect object))
                 (cons initial-symbol rest))))))
    (when (dependency-form-p primary-system dependency-form)
      (prog1 (extract-dependencies primary-system dependency-form)
        (do-symbols (sym :extensible-inferred-system-temporary)
          (unintern sym :extensible-inferred-system-temporary))))))
