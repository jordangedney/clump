(in-package :clump-tests)
(use-syntax :clump)

(defsuite memoize (clump))

(deftest defmemo (memoize)
  (defmemo fib (n)
    (if (<= 0 n 1)
        n
        (+ (fib (- n 1))
           (fib (- n 2)))))
  ;; We know defmemo works if this ever finishes with the answer.
  (assert-eql 354224848179261915075 (fib 100)))
