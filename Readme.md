Note: Don't use this!

Based on clamp:

https://github.com/arclanguage/Clamp
by Michael Malis (originally at https://github.com/malisper/Clamp)

Arc is an amazing programming language because of its brevity and succinctness,
but at the same time, it lacks some of the most basic features of a programming
language. It has neither a debugger nor a module system. Common Lisp on the
other hand has many of the fundamentals that Arc lacks and much more (restarts,
reader macros, etc), but lacks the conciseness of Arc. Clamp is an attempt to
bring the powerful, but verbose, language of Common Lisp up to the terseness
of Arc.

There are currently two parts to Clamp. There is the core of Clamp, which
implements the utilities of Arc that are easily converted from Arc to Common
Lisp. The other part is the 'experimental' part. It contains features of Arc
that are not so easy to copy (ssyntax, argument destructuring, etc).

The package `:clump` exports not only the symbols that are new in Clamp, but
also exports most of the symbols from the `:cl` package. This is done so that
it is possible to shadow Common Lisp operators which do different things than
the Arc operators of the same names. By using Clamp in a package, you are
automatically using most of the symbols exported by `:cl` (some, such as
`rplaca`, have not been exported because use of them is generally considered
bad style).

The package `:clump-experimental` works a little differently. It only exports
the new symbols it defines. In order to use both `:clump` and `:clump-experimental`,
you will have to use both and then handle the conflicts.

In both packages, a lot of code has been taken from both the original Arc and
Anarki.

To install Clamp you'll need a Common Lisp implementation. One way is to
install SBCL and the Quicklisp package manager on Linux:

  ```shell
  $ sudo apt-get install sbcl  # other Common Lisp implementations might work as well
  $ wget https://beta.quicklisp.org/quicklisp.lisp  # following instructions at https://quicklisp.org
  $ sbcl --load quicklisp.lisp
  * (quicklisp-quickstart:install)
  * (ql:add-to-init-file)
  * (quit)
  ```

(These instructions were tested on Ubuntu 14.04 with sbcl 1.1.14.)

Now add the Clamp git repository to ~/quicklisp/local-projects. Then you can
start up clump in three ways:

a) Run the 'clump' script in the repo:

  ```
  $ clump
  * (map [+ _ 1] '(1 2 3))
  ```

This requires either that you be in ~/quicklisp/local-projects/Clamp, or that
you copy the 'clump' script somewhere in your PATH.

b) If you want to run from any directory and don't want to mess with the PATH,
you'll need to type in a few commands into sbcl: 

  ```
  $ sbcl
  * (ql:quickload :clump)
  * (in-package :clump)
  * (use-syntax :clump)
  ```

c) To run Clamp with Emacs, follow the instructions to install MELPA and SLIME
at https://www.common-lisp.net/project/slime/doc/html/Installation.html, and
then add these lines in your .emacs:

  ```
  (setq inferior-lisp-program (concat (getenv "HOME") "/quicklisp/local-projects/Clamp/clump"))
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  ```

Either way, you should now be ready to go:

  ``` lisp
  * (map [+ _ 1] '(1 2 3))  ; example showing off Arc-specific syntax
  (2 3 4)
  ```

Finally, to run Clamp's unit tests:

  ``` lisp
  * (ql:quickload :clump-tests)
  * (in-package :clump-tests)
  * (run-suite 'clump)
  ```
