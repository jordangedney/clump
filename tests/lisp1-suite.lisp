(in-package :experimental-tests)
(use-syntax :clump)

(deftest lisp1 (clump-experimental)
  (assert-equal '((1 4) (2 5) (3 6))
      (w/lisp1 (map list '(1 2 3) '(4 5 6))))
  (assert-equal '((1 4) (2 5) (3 6))
      (w/lisp1 (let var list
                 (map var '(1 2 3) '(4 5 6))))))
