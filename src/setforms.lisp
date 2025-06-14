(in-package :clump)
(use-syntax :clump)


(def setforms (exp &optional env)
  "Given an expression for a place, returns three values. The first
   one is a list of variables and values that should be bound with
   'withs' to create an environment for the other expressions to be
   evaluated in. The second value is an expression which will get the
   value of the place. The last value is a procedure name (which will
   generally be a lambda expression), that when called with the new
   value will set the place to that value."
  (mvb (vars forms var set access) (get-setf-expansion exp env)
    (values (mappend #'list vars forms)
            access
            `(lambda ,var ,set))))

(mac zap (op place &rest args &environment env)
  "Assigns the result of calling OP on the rest of the arguments
   (including PLACE) to PLACE. For example (zap #'+ x n) is
   equivalent to (incf x n)."
  (mvb (vars access set) (setforms place env)
    `(withs ,vars
       (,set (call ,op ,access ,@args)))))

(mac or= (place new &environment env)
  "If PLACE is nil, assign the result of evaluating NEW there.
   Otherwise returns whatever value was already in PLACE and does not
   evaluate NEW."
  (mvb (vars access set) (setforms place env)
    `(withs ,vars
       (,set (or ,access ,new)))))

(mac or2= (place new &environment env)
  "Equivalent to or= but will not carry through with the assignment
   if accessing PLACE has a second return value which is non-nil."
  (mvb (vars access set) (setforms place env)
    (w/uniq (val win)
      `(withs ,vars
         (mvb (,val ,win) ,access
           (,set (if (or ,val ,win) ,val ,new)))))))

(mac initialize (&rest args)
  "Sets every one of its arguments to t."
  `(do ,@(map (fn (a) `(set ,a t)) args)))

(mac wipe (&rest args)
  "Sets every one of its arguments to nil."
  `(do ,@(map (fn (a) `(set ,a nil)) args)))

(mac pull (test place)
  "Removes all of the elements in PLACE that satisfy test and stores
   that value back into PLACE."
  (w/uniq g
    (mvb (binds val setter) (setforms place)
      `(withs ,(join (list g test) binds)
         (,setter (rem ,g ,val))))))
