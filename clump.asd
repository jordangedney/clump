(in-package :asdf-user)

(defsystem "clump"
  :description "Common Lisp with Arc Macros and Procedures"
  :version "0.3"
  :author "malisper"
  :depends-on ("iterate" "cl-syntax")
  :in-order-to ((test-op (test-op :clump-tests)))
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "defalias" :depends-on ("package"))
     (:file "aliases"  :depends-on ("defalias"))
     (:file "syntax"   :depends-on ("package"))
     (:file "base"     :depends-on ("aliases" "syntax"))
     (:file "read"     :depends-on ("aliases"))
     (:file "hof"      :depends-on ("aliases" "base"))
     (:file "binding"  :depends-on ("hof"))
		 (:file "fns"      :depends-on ("aliases" "base" "binding"))
     (:file "print"    :depends-on ("aliases" "base" "binding" "hof"))
     (:file "time"     :depends-on ("aliases" "print"))
     (:file "macros"   :depends-on ("binding" "print"))
     (:file "fnops"    :depends-on ("binding" "base" "conditionals"))
     (:file "setforms" :depends-on ("binding" "macros"))
     (:file "memoize"  :depends-on ("setforms"))
     (:file "strings"  :depends-on ("misc"))
     (:file "iter"     :depends-on ("hof" "macros" "fnops"))
     (:file "list"     :depends-on ("aliases" "macros" "fns" "base"))
     (:file "conditionals" :depends-on ("macros" "list" "fns"))
     (:file "misc"     :depends-on ("macros" "conditionals" "iter" "list" "hof" "fnops"))
     (:file "sort"     :depends-on ("binding" "list" "iter"))
     (:file "io"       :depends-on ("iter" "read"))
     (:file "tables"   :depends-on ("binding" "iter"))
     (:file "disk"     :depends-on ("macros" "conditionals" "io"))
     (:file "code"     :depends-on ("iter" "hof" "misc" "read" "io"))
		 (:file "deftem"   :depends-on ("binding" "list" "hof" "macros" "print"))
     (:file "threading":depends-on ("aliases" "syntax"))))))

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
