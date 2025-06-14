;;;; Higher order funtions.

(in-package :clump)
(use-syntax :clump)

(def mapv (f seq &rest seqs)
  "Map a procedure over the given sequences and return a vector
   containing the results."
  (apply #'cl:map 'vector f seq seqs))

(def testify (x &optional (test #'iso))
  "If passed a procedure, returns it. Otherwise returns a function
   which tests equality for the object passed."
  (if (functionp x) x [call test x _]))

(def rem (f xs  &rest args &key (test #'iso) &allow-other-keys)
  "Equivalent to remove-if but 'testifies' TEST first."
  (apply #'remove-if (testify f test) xs :allow-other-keys t args))

(def keep (f xs &rest args &key (test #'iso) &allow-other-keys)
  "Equivalent to remove-if-not but 'testifies' TEST first."
  (apply #'remove-if-not (testify f test) xs :allow-other-keys t args))

(def mem (f xs &rest args &key (test #'iso) &allow-other-keys)
  "Equivalent to member-if but 'testifies' TEST first."
  (apply #'member-if (testify f test) xs :allow-other-keys t args))

(def find (f xs &rest args &key (test #'iso) &allow-other-keys)
  "Equivalent to find-if but 'testifies' TEST first."
  (apply #'find-if (testify f test) xs :allow-other-keys t args))

(def count (f xs &rest args &key (test #'iso) &allow-other-keys)
  "Equivalent to count-if but 'testifies' TEST first."
  (apply #'count-if (testify f test) xs :allow-other-keys t args))

(def find-index (f xs &rest args &key (test #'iso) &allow-other-keys)
  "Equivalent to position-if but 'testifies' TEST first."
  (apply #'position-if (testify f test) xs :allow-other-keys t args))

(def mappend (f &rest xss)
  "Equivalent to map but appends the results instead of just
   returning them."
  (apply #'join (apply #'map f xss)))

(def partition (test seq &key (key #'identity) (start 0))
  "Returns two lists, the first one containing all of the elements of
   XS that pass the 'testified' version of test and the second
   containing all of those that don't."
  (loop with f = (testify test)
        for x in (cut (coerce seq 'list) start)
        if (call f (funcall key x))
          collect x into pass
        else
          collect x into fail
        finally (return (values pass fail))))

(def trues (f xs)
  "Maps F over XS and returns a list of the non-nil results."
  (keep #'idfn (map f xs)))

(mac hofeach (f var xs &body body)
  "Wrap BODY in a anonymous function with argument VAR and call the
   higher order function F with that function and XS as arguments."
  `(call ,f (fn (,var) ,@body) ,xs))

(mac mapeach (var xs &body body)
  "Executes BODY repetitively with each element of XS bound to VAR.
   Returns a list of the results. VAR can be a destructuring list."
  `(loop for ,var in ,xs collect (do ,@body)))

(mac mappendeach (var xs &body body)
  "Executes BODY repetitively with each element of XS bound to VAR.
   Returns a list of all of the results appended together. VAR can be
   a destructuring list."
  `(loop for ,var in ,xs append (do ,@body)))

(def positions (f seq &key (test #'iso) (key #'idfn))
  "Returns a list of all of the positions of elements in SEQ that
   pass TEST."
  ;; The macros 'accum' and 'on' are not defined yet.
  (loop with fn = (testify f test)
        for x in (coerce seq 'list)
        for i from 0
        if (call fn (call key x))
          collect i))
