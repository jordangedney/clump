;;;; These are utilities for taking advantage of memoization.

(in-package :clump)
(use-syntax :clump)

(def memo (f)
  "Returns a memoized version of the procedure F."
  (let cache (table :test #'iso)
    (fn (&rest args)
      (or2= (gethash args cache) (apply f args)))))

(mac defmemo (name args &body body)
  "Defines a memoized procedure."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (declaim (ftype (function (&rest t) t) ,name))
     (set (symbol-function ',name) (memo (fn ,args (block ,name ,@body))))
     ,(when (stringp (car body))
            `(set (documentation ',name 'function) ,(car body)))
     ',name))
