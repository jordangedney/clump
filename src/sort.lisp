;;;; Utilities for specific kinds of sorting.

(in-package :clump)
(use-syntax :clump)

(def compare (comparer scorer)
  "Returns a procedure which compares its arguments score from SCORER
   using COMPARER. Generally should use the :key argument to other
   procedures instead of using compare."
  (fn (x y) (call comparer (funcall scorer x) (funcall scorer y))))

;;; It should be easy to modify best so that it works on all
;;; sequences as well as lists.
(def best (f xs &optional (key #'identity))
  "Finds the first element of the list XS if it was sorted using
   the procedure F."
  (if (no xs)
      nil
      (ret wins (car xs)
        (let score (call key wins)
          (each elt (cdr xs)
            (let elt-score (call key elt)
              (when (call f elt-score score)
                (set wins elt
                   score elt-score))))))))

(def bestn (n f seq &optional (key #'identity))
  "Returns a list containg the first N elements of SEQ if it was
   sorted using the procedure F."
  (firstn n (sort f seq key)))

(def nsort (comparer sequence &optional (key #'idfn))
  "Destructively sorts SEQUENCE using COMPARER."
  (cl:sort sequence comparer :key key))

(def sort (comparer sequence &optional (key #'idfn))
  "Non-destructively sorts SEQUENCE using COMPARER."
  (nsort comparer (copy-seq sequence) key))

(def nssort (comparer sequence &optional (key #'idfn))
  "Destructively and stabily sort SEQUENCE using COMPARER."
  (cl:stable-sort sequence comparer :key key))

(def ssort (comparer sequence &optional (key #'idfn))
  "Non-destructively and stabily sort SEQUENCE using COMPARER."
  (nssort comparer sequence key))

(def med (fn seq &optional key)
  "Returns the median of a sequence. The median is the middle element
   when the list is sorted using FN. If the list contains an even
   number of elements, the middle element that comes first is
   returned."
  (elt (sort fn seq key) (dec (ceiling (len seq) 2))))
