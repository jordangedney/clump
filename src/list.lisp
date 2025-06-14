;;;; These are utilities for working with lists.

(in-package :clump)
(use-syntax :clump)

(def mklist (x)
  "If X is a list, return it. Otherwise return a list containing X."
  (if (listp x) x (list x)))

(def dotted (x)
  "Is this a dotted list?"
  (and (listp x)
       (rec (rest x)
         (if (null rest)
             nil
             (or (atom rest)
                 (recur (cdr rest)))))))

(def proper (x)
  "Is this a proper list?"
  (and (listp x)
       (not (dotted x))))

(def range (a b &optional (by 1))
  "Returns a list of numbers from A to B (inclusive) in steps of BY.
   The argument BY has to be a positive integer."
  ;; The loop macro generates code that is more efficent than what
  ;; should be written by hand.
  (if (< a b)
      (loop for i from a to b by by collect i)
      (loop for i downfrom a to b by by collect i)))

(def firstn (n seq)
  "Returns a list of the first N elements of the sequence SEQ or a
   list of all the elements if SEQ is too short. If N is nil, returns
   the entire sequence."
  (if (no n)
      seq
      (loop repeat n
	    ;; Cannot use cut to access the the elements because
            ;; this should not throw an error when the sequence
            ;; is too short.
	    for x in (coerce seq 'list)
	    collect x)))

(def split (seq n)
  "Given a sequence and an integer will return two sequences. The first
   one will contain the first N elements of the sequence, and the second
   will contain the rest of the elements of the initial sequence. The
   return sequences are of the same type as the sequence passed in."
  (values (cut seq 0 n) (cut seq n)))

(def group (xs &key (by 2) (with #'list))
  "Groups every BY elements of the given list using the procedure
   WITH."
  (if (no xs)
      '()
      (cons (apply with (firstn by xs))
	    (group (nthcdr by xs) :by by :with with))))

(def last (xs)
  "Returns the last element of XS. Not the last cons pair."
  (car (lastcons xs)))

(def flat (tree)
  "Returns a list of all of the atoms in a tree (not including nil)"
  (rec (left tree acc '())
    (if (null left)
          acc
        (atom left)
          (cons left acc)
        :else
          (recur (car left)
                 (recur (cdr left)
                        acc)))))

;;; These are predicates for testing the length of sequences. They may
;;; be further optimized, but benchmarks would be needed before then.

(def len< (seq n)
  "Is this sequence shorter than some length?"
  (< (len seq) n))

(def len> (seq n)
  "Is this sequence longer than some length?"
  (> (len seq) n))

(mac n-of (n exp)
  "Returns a list containing the results of evaluating EXP, N times."
  ;; Loop generates faster code than what I would write by hand.
  `(loop repeat ,n collect ,exp))

(mac drain (exp &optional (endval nil))
  "Repeatedly evaluates EXP until it passes the testified version of
   ENDVAL. Then returns a list of the results."
  (w/uniq (gval gtest)
    `(loop with ,gtest = (testify ,endval)
           for ,gval = ,exp
           until (call ,gtest ,gval)
           collect ,gval)))

(def caris (x val)
  "Is X a cons pair, and is its car the given value?"
  (and (consp x) (is (car x) val)))

(def carif (x)
  "Returns X if it is an atom, otherwise returns (car X)."
  (if (atom x)
      x
      (car x)))

(def conswhen (f x y)
  "Cons X and Y if (F X) is non-nil. Otherwise return Y."
  (if (call f x)
      (cons x y)
      y))

(def consif (x y)
  "Cons X and Y if X is non-nil. Otherwise return Y."
  (conswhen #'idfn x y))

(def cars (seq)
  "Returns a list of the cars of each list within a given sequence."
  (map #'car seq))

(def cdrs (seq)
  "Returns a list of the cdrs of each list within a given sequence."
  (map #'cdr seq))

(def linearlize (arr)
  "Return a vector that has the same elements as a possibly
   multidimensional array."
  (make-array (array-total-size arr) :displaced-to arr))

(defgeneric get (obj arg)
  (:documentation "Returns whatever is associated with ARG in OBJ."))

(defgeneric (setf get) (val obj arg)
  (:documentation "Sets ARG to be associated with VAL in OBJ."))

(defmethod get ((seq sequence) (n integer))
  "Returns the Nth element of a sequence."
  (elt seq n))

(defmethod (setf get) (val (seq sequence) (n integer))
  "Sets the Nth element of SEQ to VAL."
  (set (elt seq n) val))

(defmethod get ((tab hash-table) x)
  "Returns whatever is stored in TAB under X."
  (gethash x tab))

(defmethod (setf get) (val (tab hash-table) x)
  "Sets VAL to be stored under X in TAB."
  (set (gethash x tab) val))

(defmethod get ((a array) (index integer))
  "If A is a vector, return the corresponding element. Otherwise
   return a displaced array that acts like the subarray."
  (if (vectorp a)
      (call-next-method)
      (withs ((rows . rest) (array-dimensions a)
	      size (reduce #'* rest))
	(assert (< index rows) (index)
		"Index ~A out of bounds for array with dimension size ~A" index rows)
	(make-array rest
		    :displaced-to a
		    :displaced-index-offset (* index size)))))

(defmethod get ((arr array) (list list))
  "It is convient to consider indexing a list into an arraying being
   the same as just considering each element of the list as a different
   dimension."
  (apply #'aref arr list))

(defmethod (setf get) (val (arr array) (list list))
  "Sets the value when using an list to access into a array."
  (set (apply #'aref arr list) val))

(defmethod get (obj x)
  "Calls X on OBJECT."
  (call x obj))

(defmethod (setf get) (val obj x)
  "Calls (setf X) on val and obj. This may or may not work depending
   on how the setter for X was defined."
  (call (fdefinition `(setf ,x)) val obj))

(mac trav (x &rest fs)
  "Traverse X, calling FS in sequence. The symbol 'recur' is bound to
   a procedure which can be used to recursively traverse the object.
   The return value is nil."
  (w/uniq g
    `(rec (,g ,x)
       (when ,g
         ,@(map (fn (f) `(call ,f ,g)) fs)))))

(def intersperse (x ys)
  "Returns a list with the element X in between every element in YS."
  (and ys
       (cons (car ys)
             (loop for y in (cdr ys)
                   collect x
                   collect y))))
