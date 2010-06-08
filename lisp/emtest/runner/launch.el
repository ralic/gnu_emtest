;;;_ emtest/runner/launch.el --- Launchers for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ , Commentary:

;; 

;;;_ , Requires

(require 'emtest/runner/tester)
(require 'emtest/viewer/emviewer2)
(require 'emtest/runner/explorers/library) ;;Just for the library launching.
(require 'emtest/common/result-types)
(require 'emtest/common/config)  ;;Just for receiver configuration
(require 'emtest/editing/lisp)

;;;_. Body

;;;_ , Borrowed variables
;;;_  . Counter

;;Should probably live elsewhere.  Possibly in emtest/viewer/receive.
;;But then emtl:dispatch-normal must move there as well.
(defvar emtl:testrun-counter 0 
   "A counter used to make testrun-id.
With `cl' loaded, use it as (incf emtl:testrun-counter)." )

;;;_ , emtl:dispatch-normal
(defun emtl:dispatch-normal (what-to-run &optional prefix receiver)
   ""
   (emtt:test-finder:top 
      what-to-run 
      prefix  ;;Default is the empty list.
      (prin1-to-string (incf emtl:testrun-counter))
      (or receiver emtl:receiver-f)))

;;;_ , emt:sexp-at-point
;;;###autoload
(defun emt:sexp-at-point (form)
   ""
   (interactive 
      (list 
	 (save-excursion (read (current-buffer)))))
   
   (emtl:dispatch-normal
      (emthow:make-form
	 :test-form form)
      (list "form")))

;;;_ , emtl:run-suite
(defun emtl:run-suite (suite-sym)
   "Run the test suite associated with SUITE-SYM."
   
   ;;$$UPDATE ME - will need to change what it makes
   (emtl:dispatch-normal 
      (emthow:make-suite
	 :suite-ID suite-sym)
      (list (format "Suite %s" suite-sym))))


;;;_ , emt:defun-at-point
;;;###autoload
(defun emt:defun-at-point (arg)
  "Run tests on the function or suite under point.

If prefix ARG is non-nil, eval it first.

Does nothing if the buffer is not in a known lisp mode."

   (interactive "P")
   ;;Only proceed if we know how to run tests
   (when (eq major-mode 'emacs-lisp-mode)

      ;;If `arg', eval that definition first.
      (when arg (eval-defun nil))
      (let
	 ((suite-sym
	     (emtel:suite-sym-at-point)))
	 (check-type suite-sym symbol)
	 (emtl:run-suite suite-sym))))

;;;_ , emt:lib-at-point

;;Command: Run the library of symbol at point, or failing that, file
;;at point.  Give a prompt for confirmation.
;;Can use `symbol-file'

;;;_ , emtel:read-testable-library
(defun emtel:read-testable-library (prompt)
   "Interactively read the name of a library containing tests.
PROMPT is a prompt string"
   
   (completing-read 
      prompt
      load-history
      ;;Narrow to just libraries that have tests in them.
      #'(lambda (lib-data)
	   (some
	      #'(lambda (x)
		   (get (emtl:ldhst-el->symbol x) 'emt:suite))
	      (cdr lib-data)))
      t))
;;$$ADD TESTS There are example loads in emtest/runner/explorers/library/tests

;;;_ , emt:library
;;;###autoload
(defun emt:library (library &optional receiver)
   "Run the test suites of LIBRARY.
LIBRARY is the absolute file name of the library"
   
   (interactive
      (list
	 (emtel:read-testable-library 
	    "Run tests of which library: ")))
   
   (let*
      (
	 (test-id
	    (emthow:make-library:elisp-load
	       :load-name library)))
      (emtl:dispatch-normal test-id nil receiver)))

;;;_ , emtt:eval
(defun emtt:eval (expression)
   ""
   (emtl:dispatch-normal
      (emthow:make-form
	 :test-form (list nil expression))
      (list "expression")))

;;;_ , emt:eval-last-sexp
;;;###autoload
(defun emt:eval-last-sexp (arg)
   ""
   
   (interactive
      (list (preceding-sexp)))
   ;;Unlike eval-last-sexp, this does not try to print value in
   ;;minibuffer, nor in current buffer, nor optionally trigger the
   ;;debugger.
   (emtt:eval arg))

;;;_ , emt:eval-expression
;;;###autoload
(defun emt:eval-expression (arg &optional insert-value)
   ""
   ;;Interactive form borrowed from "simple.el"
   (interactive
      (list (let ((minibuffer-completing-symbol t))
	       (read-from-minibuffer "Eval: "
		  nil read-expression-map t
		  'read-expression-history))
	 current-prefix-arg))
   ;;Unlike eval-expression, this does not do the extra stuff
   (emtt:eval arg))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/launch)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/launch.el ends here
