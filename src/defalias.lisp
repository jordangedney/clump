;;;; Definitions for defalias which allows redefinition of
;;;; macros, procedures, and special forms.

(in-package :clump)

(defun macrop (x)
  "Is this a macro?"
  (and (symbolp x) (macro-function x)))

(defun make-macro (new old &optional doc)
  "Generates the code for making the macros NEW and OLD equivalent."
  (cl:let ((rest (gensym "REST")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (macro-function ',new)
             (macro-function ',old)
             (documentation ',new 'function)
             ,(or doc `(documentation ',old 'function))))))

(defun fnp (x)
  "Is this a procedure?"
  (and (symbolp x) (symbol-function x)))

(defun make-fn (new old &optional doc)
  "Generates the code for making NEW and OLD the same procedure."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (symbol-function ',new)
           (symbol-function ',old)
           (documentation ',new 'function)
           ,(or doc `(documentation ',old 'function)))))

(defun make-special-macro (new old)
  "Generates the code to create a macro NEW which expands into a use
   of the special form OLD."
  (cl:let ((rest (gensym "REST")))
    `(defmacro ,new (&rest ,rest)
       `(,',old ,@,rest))))

(defmacro defalias (new old &optional doc)
  "Makes a use of NEW the equivalent to a use of OLD. Works on
   procedures, macros, and (most) special forms."
  (cond ((special-operator-p old)
         (make-special-macro new old))
        ((macrop old)
         (make-macro new old doc))
        ((fnp old)
         (make-fn new old doc))
        (:else
         (error "Don't know what to do for object ~A of type ~A"
                old (type-of old)))))
