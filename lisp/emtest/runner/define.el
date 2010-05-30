;;;_ emtest/runner/define.el --- Test definer for Emtest 

;;;_. Headers
;;;_ , License

;; Copyright (C) 2007,2009

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp

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

;;This file contains the entry point for defining emtest tests.

;;There are two ways to require this file.  The first is for files
;;that contain only test code.  For those, just write:

;;  (require 'emtest/runner/define)

;;The second is for files that include both main code and tests.  For
;;these, command `emt:insert' and choose `emt:insert-requires'.  It
;;will insert something like:

;; (unless (fboundp 'emt:deftest)
;;    (defmacro emt:deftest (&rest dummy))
;;    (defmacro emt:if-avail (&rest dummy)))

;;The code above means: If `emt:deftest' is not bound (not even as an
;;autoload), define dummies for the entry points.

;;Then put everything emtest-specific except deftests inside
;;`emt:if-avail'.  This lets the file be loaded even by users who
;;don't have emtest.

;;;_ , Requires

;;$$IMPROVE ME.  We should instead pull testhelp's autoload file(s)
;;in.  Which have yet to be created.
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/persist)

;;;_. Body
;;;_ , emt:if-avail
;;;###autoload
(defmacro emt:if-avail (&rest body)
   "Equivalent to `progn' just if emtest is available."
   (cons 'progn body))

;;;_ , emtd:make-prop-eval-form
(defun emtd:make-prop-eval-form (spec)
   ""
   
   (destructuring-bind (sym val-form) spec
      `(list ',sym ,val-form)))

;;;_ , Version 3
;;;_  . emt:deftest
(defalias 'emt:deftest 'emt:deftest-3)
;;;_  . emt:deftest-3
;;;###autoload
(defmacro emt:deftest-3 (name-or-props &rest clauses)
   "Define a test in Emtest"
   ;;If `props-or-first-clause' is a list starting with `props', it's
   ;;keywise properties, otherwise it's a clause.
   (let*
      ((props 
	  ;;In PROPS, the second element of each item is a form, which
	  ;;later will be evaluated.
	  (cond
	     ;;Bare symbol is treated as the name.
	     ((symbolp name-or-props)
		`((of ',name-or-props)))
	     ((atom name-or-props)
		(error 
		   "Deftest first arg must be a symbol or a key-value list"))
	     (t
		name-or-props)))
	 (of-what
	    (second (assq 'of props))))
      (unless of-what
	 (error "Deftest needs an `of' property"))

      `(eval-when-compile
	  (put ,of-what 'emt:properties
	     (list
		;;Get the name of the file the test lives in.  This
		;;can be `nil', eg if user used `eval-region'.
		'(load-file-name ,load-file-name)
		,@(if props
		     (mapcar
			#'emtd:make-prop-eval-form
			props)
		     ())))
	  
	  (put ,of-what 'emt:suite ',clauses))))

;;;_  . emtd:destructure-suite-3

(defmacro emtd:destructure-suite-3 (suite &rest body)
   "Run BODY with the contents of SUITE bound as indicated.
SUITE must be a symbol naming a test suite.

 * clause-list - List of the clauses in SUITE.
 * props - The property list of SUITE (an alist)."
   (declare (debug (sexp body)))
   `(let
       (
	  (clause-list
	     (get ,suite 'emt:suite))
	  (props
	     (get ,suite 'emt:properties)))
       
       ,@body))

;;;_  . emtd:destructure-clause-3
(defmacro emtd:destructure-clause-3 (clause &rest body)
   "Run BODY with the contents of CLAUSE bound as indicated.

CLAUSE must be a clause as in a test suite, for instance any element
of `clause-list' as bound by `emtt:destructure-suite'.

 * form - the test form 
 * (Used to be doc - GONE)"
   (declare (debug (sexp body)))
   `(destructuring-bind (governor &optional form) ,clause
       ,@body))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/define)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:
;;;_ , End
;;; emtest/runner/define.el ends here
