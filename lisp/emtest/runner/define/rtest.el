;;;_ tester/define/rtest.el --- Tests for Emtest define

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

;;;_. Body

;;;_  . emt:deftest-2-make-prop-eval-form
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
	 t)))

;;;_  . emt:deftest-2
'  ;;OBSOLETE
(put 'emt:deftest-2 'rtest:test-thru
   'emtt:destructure-suite)
;;;_  . Tests

;;There's a bootstrap problem in defining a suite to test this, so
;;this has to be run manually.  For now it's in rtest so no bootstrap
;;issue. 
'  ;;OBSOLETE
(rtest:deftest emtt:destructure-suite

   (  "Situation: A test is defined.
Response: Destructuring finds the expected clauses."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    ("Docstring" (progn 12)))
	 (emtt:destructure-suite 'dummy-sym
	    (assert
	       (= (length clause-list) 1) t)
	    (emtt:destructure-clause
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
	 (emtt:destructure-suite 'dummy-sym
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
	    t))))


;;;_  . emt:deftest-3
;;There's a bootstrap problem in defining a suite to test this, so
;;this has to be run manually.  Thus the use of `assert'.  For now
;;it's in rtest so no bootstrap issue.
(rtest:deftest emt:deftest-3

   (  "Situation: A test is defined.
Response: Destructuring finds the expected clauses."
      (let-noprops '(dummy-sym)
	 (emt:deftest-3 dummy-sym
	    (test (progn 12)))
	 (emtt:destructure-suite-3 'dummy-sym
	    (assert
	       (= (length clause-list) 1) t)
	    (emtt:destructure-clause
	       (car clause-list)
	       (equal form '(progn 12)))
	    t)))

   (  "Situation: A test is defined with properties
Response: Destructuring finds the expected properties."
      (let-noprops '(dummy-sym)
	 (emt:deftest-3
	    (
	       (of    'dummy-sym)
	       (db-id "my-db")
	       (four  (+ 2 2))
	       (four-form '(+ 2 2)))
	    (test (progn 12)))
	 (emtt:destructure-suite-3 'dummy-sym
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
	    t))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/define/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/define/rtest.el ends here