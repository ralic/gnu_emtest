;;;_ emt-define.el --- Test definer for Emtest 

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

;;  (require 'emt-define)

;;The second is for files that include both main code and tests.  For
;;these, instead of writing (require 'emt-define), write this:

;; (unless (fboundp 'emt:deftest)
;;    (defmacro emt:deftest (&rest dummy))
;;    (defmacro emt:if-avail (&rest dummy)))

;;The code above means: If `emt:deftest' is not bound (not even as an
;;autoload), define dummies for the entry points.

;;Then put everything emtest-specific except deftests inside
;;`emt:if-avail'.  This lets the file be loaded even by users who
;;don't have emtest.

;;;_ , Requires

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
;;;_  . Tests
(rtest:deftest emt:deftest-2-make-prop-eval-form

   (  "Shows: Makes a form that evals as expected."
      (progn
	 (assert
	    (equal
	       (eval
		  (emt:deftest-2-make-prop-eval-form '(four (+ 2 2))))
	       '(four 4))
	    t)
	 ;;Is not over-evalled.
	 (assert
	    (equal
	       (eval
		  (emt:deftest-2-make-prop-eval-form '(four-form '(+ 2 2))))
	       '(four-form (+ 2 2)))
	    t)
	 t))
   
   )

;;;_ , emt:deftest-2
;;;###autoload
(defmacro emt:deftest-2 (symbol props-or-first-clause &rest clauses)
   "Define a test in Emtest"
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
;;;_  . Tests
(put 'emt:deftest-2 'rtest:test-thru
   'emt:test:destructure-suite)
;;;_ , emt:test:destructure-clause
(defmacro emt:test:destructure-clause (clause &rest body)
   "Run BODY with the contents of CLAUSE bound as indicated.

CLAUSE must be a clause as in a test suite, for instance any element
of `clause-list' as bound by `emt:test:destructure-suite'.

 * form - the test form 
 * doc - MAY CHANGE.  The docstring."
   
   `(destructuring-bind (doc form) ,clause
       ,@body))

;;;_  . Tests

;;Tested thru `emt:test:destructure-suite'.  NB
;;`emt:test:destructure-suite' cannot test normally thru emtest due to
;;a bootstrap issue.

;;;_ , emt:test:destructure-suite
(defmacro emt:test:destructure-suite (suite &rest body)
   "Run BODY with the contents of SUITE bound as indicated.
SUITE must be a symbol naming a test suite.

 * clause-list - List of the clauses in SUITE.
 * props - The property list of SUITE (an alist)."
   
   `(let
       (
	  (clause-list
	     ;;`cdr' is to account for the initial docstring.
	     (cdr 
		(get ,suite 'emt:suite)))
	  (props
	     (get ,suite 'emt:properties)))
       
       ,@body))


;;;_  . Tests

;;There's a bootstrap problem in defining a suite to test this, so
;;this has to be run manually.  For now it's in rtest so no bootstrap
;;issue. 
(rtest:deftest emt:test:destructure-suite

   (  "Situation: A test is defined.
Response: Destructuring finds the expected clauses."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    ("Docstring" (progn 12)))
	 (emt:test:destructure-suite 'dummy-sym
	    (assert
	       (= (length clause-list) 1) t)
	    (emt:test:destructure-clause
	       (car clause-list)
	       (equal form '(progn 12)))
	    t)))
   
   
   (  "Situation: A test is defined with properties
Response: Destructuring finds the expected properties."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    (props 
	       (db-id "my-db")
	       (four (+ 2 2))
	       (four-form '(+ 2 2)))
	    ("Docstring" (progn 12)))
	 (emt:test:destructure-suite 'dummy-sym
	    (assert
	       (equal
		  (assoc 'db-id props)
		  '(db-id "my-db")))
	    (assert
	       (equal
		  (assoc 'four props)
		  '(four 4))
	       t)
	    (assert
	       (equal
		  (assoc 'four-form props)
		  '(four-form (+ 2 2)))
	       t)
	    t)))
   
   )

;;;_. Footers
;;;_ , Provides

(provide 'emt-define)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:
;;;_ , End
;;; emt-define.el ends here
