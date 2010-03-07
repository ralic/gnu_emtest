;;;_ tester/define.el --- Test definer for Emtest 

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

;;  (require 'tester/define)

;;The second is for files that include both main code and tests.  For
;;these, instead of writing (require 'tester/define), write this:

;; (unless (fboundp 'emt:deftest)
;;    (defmacro emt:deftest (&rest dummy))
;;    (defmacro emt:if-avail (&rest dummy)))

;;The code above means: If `emt:deftest' is not bound (not even as an
;;autoload), define dummies for the entry points.

;;Then put everything emtest-specific except deftests inside
;;`emt:if-avail'.  This lets the file be loaded even by users who
;;don't have emtest.

;;;_ , Requires

;;Clunky for now.  Really we'd pull their autoload file(s) in.
(require 'tester/testhelp/standard)
(require 'tester/testhelp/persist)

;;;_. Body
;;;_ , emt:if-avail
;;;###autoload
(defmacro emt:if-avail (&rest body)
   "Return (progn . BODY) just if emtest is available."

   (cons 'progn body))
;;;_  . Tests
;;It's direct
;;;_ , emt:deftest-2-make-prop-eval-form
(defun emt:deftest-2-make-prop-eval-form (spec)
   ""
   
   (destructuring-bind (sym val-form) spec
      `(list ',sym ,val-form)))

;;;_ , emt:deftest-2
;;;###autoload
(defmacro emt:deftest-2 (symbol props-or-first-clause &rest clauses)
   "Define a test in Emtest"
   (error "emt:deftest-2 is OBSOLETE, use emt:deftest-3")
   ;;If `props-or-first-clause' is a list starting with `props', it's
   ;;keywise properties, otherwise it's a clause.
   (let*
      ((props-p
	  (and 
	     (consp props-or-first-clause)
	     (eq (car props-or-first-clause) 'props)))
	 (clauses-1
	    (if props-p
	       clauses
	       (cons props-or-first-clause clauses))))
      
      `(eval-when-compile
	  ;;Version when no internally defined properties were
	  ;;recorded. 
;; 	  ,@(if props-p
;; 	       `((put ',symbol 'emt:properties
;; 		    (list
;; 		       ;;Get the name of the file the test lives in.
;; 		       '(load-file-name ,load-file-name)
;; 		       ,@(mapcar
;; 			    #'emt:deftest-2-make-prop-eval-form
;; 			    (cdr props-or-first-clause)))))
;; 	       ())
	  (put ',symbol 'emt:properties
	     (list
		;;Get the name of the file the test lives in.  This
		;;can be `nil', eg if user used `eval-region'.
		'(load-file-name ,load-file-name)
		,@(if props-p
		     (mapcar
			#'emt:deftest-2-make-prop-eval-form
			(cdr props-or-first-clause))
		     ())))
	  
	  (put ',symbol 'emt:suite
	     ',(cons
		  (symbol-name symbol)
		  clauses-1)))))
;;;_ , emtt:destructure-clause
(defmacro emtt:destructure-clause (clause &rest body)
   "Run BODY with the contents of CLAUSE bound as indicated.

CLAUSE must be a clause as in a test suite, for instance any element
of `clause-list' as bound by `emtt:destructure-suite'.

 * form - the test form 
 * doc - MAY CHANGE.  The docstring."
   (error "emtt:destructure-clause is OBSOLETE, use emtt:destructure-clause-3")
   
   `(destructuring-bind (doc form) ,clause
       ,@body))

;;;_  . Tests

;;Tested thru `emtt:destructure-suite'.  NB
;;`emtt:destructure-suite' cannot test normally thru emtest due to
;;a bootstrap issue.

;;;_ , emtt:destructure-suite
(defmacro emtt:destructure-suite (suite &rest body)
   "Run BODY with the contents of SUITE bound as indicated.
SUITE must be a symbol naming a test suite.

 * clause-list - List of the clauses in SUITE.
 * props - The property list of SUITE (an alist)."
   (error "emtt:destructure-suite is OBSOLETE, use emtt:destructure-suite-3")
   
   `(let
       (
	  (clause-list
	     ;;`cdr' is to account for the initial docstring.
	     (cdr 
		(get ,suite 'emt:suite)))
	  (props
	     (get ,suite 'emt:properties)))
       
       ,@body))



;;;_ , Version 3
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
			#'emt:deftest-2-make-prop-eval-form
			props)
		     ())))
	  
	  (put ,of-what 'emt:suite ',clauses))))

;;;_  . emtt:destructure-suite-3

(defmacro emtt:destructure-suite-3 (suite &rest body)
   "Run BODY with the contents of SUITE bound as indicated.
SUITE must be a symbol naming a test suite.

 * clause-list - List of the clauses in SUITE.
 * props - The property list of SUITE (an alist)."
   
   `(let
       (
	  (clause-list
	     (get ,suite 'emt:suite))
	  (props
	     (get ,suite 'emt:properties)))
       
       ,@body))

;;;_  . emtt:destructure-clause-3
;;No longer will provide `doc'.  This form is tentative.  Later
;;clauses may become more complex.
;;I've made FORM optional - also considered making FORM the rest of it.
(defmacro emtt:destructure-clause-3 (clause &rest body)
   "Run BODY with the contents of CLAUSE bound as indicated.

CLAUSE must be a clause as in a test suite, for instance any element
of `clause-list' as bound by `emtt:destructure-suite'.

 * form - the test form 
 * (Used to be doc - GONE)"
   
   `(destructuring-bind (governor &optional form) ,clause
       ,@body))


;;;_. Footers
;;;_ , Provides

(provide 'tester/define)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:
;;;_ , End
;;; tester/define.el ends here
