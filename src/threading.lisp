(in-package :clump)
(use-syntax :clump)

(def insert-first (arg surround)
  "Inserts ARG into the list form SURROUND as its first argument, after the
operator."
  (list* (car surround)
         arg
         (cdr surround)))

(def insert-last (arg surround)
  "Inserts ARG into the list form SURROUND as its last argument."
  (join surround (list arg)))

(def simple-inserter (insert-fun)
  (fn (acc next)
    (if (listp next)
        (call insert-fun acc next)
        (list next acc))))

(def simple-inserter (insert-fun)
  (fn (acc next)
    (if (listp next)
        (call insert-fun acc next)
        (list next acc))))

(defun <>? (form)
  "Predicate identifying the placeholders for the -<> and -<>> macros."
  (and (symbolp form)
       (string= form "<>")))

(def diamond-inserter (insert-fun)
  (simple-inserter (fn (acc next)
                     (case (count-if #'<>? next)
                       0 (call insert-fun acc next)
                       1 (substitute-if acc #'<>? next)
                       t (let r (uniq "R")
                            `(let ,r ,acc
                               ,(substitute-if r #'<>? next)))))))

(mac -> (initial-form &rest forms)
  "Like ->, but if a form in FORMS has one or more symbols named <> as top-level
element, each such symbol is substituted by the primary result of the form
accumulated so far, instead of it being inserted as first argument.  Also known
as diamond wand."
  (reduce (diamond-inserter #'insert-first)
          forms
          :initial-value initial-form))

(mac ->> (initial-form &rest forms)
  "Like -<>, but if a form has no symbol named <>, the insertion is done at the
end like in ->>.  Also known as diamond spear."
  (reduce (diamond-inserter #'insert-last)
          forms
          :initial-value initial-form))
