;;;; Utilities for printing.

(in-package :clump)
(use-syntax :clump)

(def pr (&rest args)
  "Prints all of its arguments to *standard-output* in a human
   readable format."
  (map #'princ args)
  (car args))

(def prn (&rest args)
  "Prints all of its arguments to *standard-output* in a human
   readable format with an additional newline."
  (do1 (apply #'pr args)
       (terpri)))

(def prf (control-string &rest args)
  "Equivalent to format but automatically prints to *standard-output*."
  (apply #'format t control-string args))

(def prs (&rest args)
  "Prints the arguments seperated by spaces and returns the arguments
   as a list."
  (prf "~{~A~^ ~}" args)
  args)

(def prns (&rest args)
  "The same as prs, but prints a newline at the end."
  (do1 (apply #'prs args)
       (terpri)))

(mac w/outstring (var &rest body)
  "Creates a string output stream and binds it to VAR."
  `(with-output-to-string (,var)
     ,@body))

(mac tostring (&body body)
  "Collects all of the output to *standard-output* into a string."
  `(w/outstring *standard-output* ,@body))

(mac w/instring (var string &rest body)
  "Binds an string input stream which reads from STRING, to VAR."
  `(with-input-from-string (,var ,string)
     ,@body))

(mac fromstring (string &body body)
  "Makes the input from *standard-input* read from STRING."
  `(w/instring *standard-input* ,string ,@body))

(mac tofile (name &body body)
  "Redirects *standard-output* to the file NAME.
   WARNING: supersedes the file."
  `(w/outfile *standard-output* ,name
     ,@body))

(mac fromfile (name &body body)
  "Makes the input from *standard-input* read from the file NAME."
  `(w/infile *standard-input* ,name
     ,@body))

(def sp (&optional (n 1))
  "Prints the given number of spaces."
  (loop repeat n
        do (pr " ")))

(defparameter bar* " | " "The character used for w/bars.")

(mac w/bars (&rest exps)
  "Executes each expression in EXPS and outputs the combined output
   of each one with bar* in between each expression."
  ;; The macro w/uniq is not defined until the macros file which is
  ;; loaded after print.
  (with (out (gensym) needbars (gensym))
    `(let ,needbars nil
       (do ,@(mapeach e exps
               `(let ,out (tostring ,e)
                  (unless (iso ,out "")
                    (if ,needbars
                      (pr bar* ,out)
                      (do (set ,needbars t)
                          (pr ,out))))))))))
