(defpackage :clump-experimental-tests
  (:use :clunit :clump :clump-experimental)
  (:import-from :syntax :use-syntax)
  (:nicknames :experimental-tests)
  (:shadowing-import-from :clump-experimental
     :def :defmemo :defmethod :mac :fn :coerce))

(in-package :clump-experimental-tests)

(defsuite clump-experimental ())
