(in-package :asdf-user)
;; dead file, I think
(defsystem "clump-tests"
  :description "tests for clump"
  :depends-on ("clunit" "check-it" "clump" "clump-experimental")
  :serial t
  :components ((:module "tests"
                :components ((:file "clump-suite")
                             (:file "base-suite")
                             (:file "binding-suite")
                             (:file "conditionals-suite")
                             (:file "fns-suite")
                             (:file "fnops-suite")
                             (:file "hof-suite")
                             (:file "iter-suite")
                             (:file "list-suite")
                             (:file "print-suite")
                             (:file "memoize-suite")
                             (:file "misc-suite")
                             (:file "setforms-suite")
                             (:file "tables-suite")
                             (:file "strings-suite")
                             (:file "sort-suite")
                             (:file "io-suite")

                             (:file "clump-experimental-suite")
                             (:file "destructuring-suite")
                             (:file "coerce-suite")
                             (:file "ssyntax-suite")
                             (:file "lisp1-suite")))))

(defmethod perform ((op test-op) (c (eql (find-system :clump-tests))))
  (let ((*package* (find-package :clump-tests)))
    (print (symbol-call :clump-tests :run-suite (intern* :clump :clump-tests)))))
