;;;_ tester/tester/tests.el --- Tests for tester/tester

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
;;;_ , emtt:get-properties
(rtest:deftest emtt:get-properties

   (  "Situation: A test is defined with properties.
That test is now being run.
Behavior: `emtt:get-properties' returns the relevant property."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    (props (db-id "my-db")(example-dir "examples/"))
	    ("Docstring" 
	       (emtp tp:531a913c-aa10-4730-9be5-30c1cb02b7c4
		  ()
		  t)))
	 
	 (emt:testpoint:eval
	    (emtt:th:run-suite 'dummy-sym #'ignore)
	    (tp:531a913c-aa10-4730-9be5-30c1cb02b7c4
	       ()
	       (progn
		  (assert
		     (equal
			(emtt:get-properties 'db-id)
			"my-db")
		     t)
		  (assert
		     (equal
			(emtt:get-properties 'example-dir)
			"examples/")
		     t))))
	 t)))



;;;_ , emtt:explore-clause
(rtest:deftest emtt:explore-clause
   
   ;;Check that it stores top-level badnesses.
   ;;Check that it stores top-level info.

   )

;;;_  . emtt:explore-one

(put 'emtt:explore-one 'rtest:test-thru
   'emt:test-finder:top)

;;;_   , emt:test-finder:top

(rtest:deftest emt:test-finder:top

   (  "Shows: It passes callback an `emt:testral:report'."
      (progn
	 (emtt:th:explore-one '(error "An example error") 
	    #'(lambda (report)
		 (check-type report emt:testral:report)
		 (let
		    ((emty:use t))
		    (check-type report emt:testral:report))))
	 t)))

;;;_. Footers
;;;_ , Provides

(provide 'tester/tester/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/tester/tests.el ends here
