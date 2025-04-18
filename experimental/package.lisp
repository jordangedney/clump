(defpackage :clump-experimental
  (:nicknames :experimental)
  (:use :clump)
  (:shadow :coerce :def :defmethod :fn :mac :defmemo)
  (:import-from :syntax :use-syntax)
  (:export
   ;; From ssyntax.
   :w/ssyntax :defssyntax-test :defssyntax-macro :defssyntax-sym-mac

   ;; From destructuring.
   :fn :! :?

   ;; From coerce.
   :coerce :defcoerce

   ;; From lisp1.
   :w/lisp1

   ;; From def.
   :def :defmemo :defmethod :mac))

;;;; There must be someway to export all of the symbols in clump
;;;; except for desired symbols.
