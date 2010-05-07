;;;_ tester/launch/rtest.el --- Tests of tester/launch.el

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

(require 'emtest/runner/launch/testhelp)

;;;_. Body

;;;_  . Tests

(rtest:deftest emtt:sexp-at-point

   ;;This can only be tested fully-manually.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emtt:sexp-at-point
	 '("Situation: While executing a dummy test with no checks"
	     (progn))))

   '  
   (  "Situation: WRITEME.
Response: WRITEME."
      (emtt:sexp-at-point
	 '("Situation: While executing a dummy test
with a successful check.   The test is fully written out." 
	     (emt:should-f t))))
   
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emtt:sexp-at-point
	 '("Situation: While executing a dummy test with a successful
check.  The test is a macro.
"
	     (should t))))

   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emtt:sexp-at-point
	 '("Situation: While executing a dummy test with a failing check"
	     (should nil))))
   '
   (  "Situation: Body raises an error of type `emt:already-handled'.
Response: Reports a failure but essentially nothing special is reported."
      (emtt:sexp-at-point
	 '("Situation: While executing a dummy test that raises
`emt:already-handled'."
	     (signal 'emt:already-handled ())))))

;;;_  . Test helper emtt:suite:th:get-id-list

(defun emtt:suite:th:get-id-list (sym) 
   ""
   (let
      ((test-id-list ()))
      (emtp:eval
	 (emtt:th:run-suite sym #'ignore)
	 (tp*
	    (:id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 
	       :count nil 
	       :fallthru t)
	    (test-id)
	    (typecase test-id
	       ;;Intercept clause
	       (emt:test-ID:e-n:indexed-clause
		  (push test-id test-id-list)
		  ;;Don't try to run it, return instead.
		  (throw 'emtp:tag-return nil))
	       ;;Let suite fall thru to handler
	       (emt:test-ID:e-n:suite t)
	       ;;We don't expect to see any other types of
	       ;;explores.
	       (t
		  (error "This test shouldn't reach here")))))
      test-id-list))

;;;_  . Tests
(rtest:deftest emtt:suite

   (  "Situation: SUITE-SYM has a test suite defined.
Response: That test-suite is run.  A result is returned."
      (let-noprops '(dummy-sym)
	 (emt:deftest-3 dummy-sym
	    (nil (progn (emt:doc "Docstring"))))
	 (emtp:eval
	    (emtt:th:run-suite 'dummy-sym 
	       #'(lambda (x)
		    (emty:check x emt:testral:report)))
	    (tp-reached tp:798212b4-1abe-4779-beb1-baf53ff39a8c 
	       1)
	    (tp* 
	       (  :id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 
		  :count nil
		  :fallthru t)
	       (test-id)
	       (typecase test-id
		  ;;Intercept clause, count it
		  (emt:test-ID:e-n:indexed-clause
		     ;;NOT emt:test-ID:e-n:form
		     ;;A reached-point for counting invocations.
		     (emtp tp:798212b4-1abe-4779-beb1-baf53ff39a8c
			())
		     ;;Fall thru
		     t)
		  ;;Let suite fall thru to handler
		  (emt:test-ID:e-n:suite t)
		  ;;We don't expect to see any other types of
		  ;;explores.
		  (t
		     (error "This test shouldn't reach here")))))
	 t))
   
   
   (  "Shows: Exactly the clauses of suite are run.
Situation: Suite has two clauses defined.  
Full exploration is used (Meaningless for now)
Response: Just two clauses are run.
Those suites have distinct IDs."
      (let-noprops '(dummy-sym)
	 (emt:deftest-3 dummy-sym
	    (() (progn (emt:doc "Clause 1")))
	    (() (progn (emt:doc "Clause 2"))))
	 (let
	    ((all-test-ids))
	    (emtp:eval
	       (emtt:th:run-suite 'dummy-sym #'ignore)
	       (tp* 
		  (
		     :id tp:798212b4-1abe-4779-beb1-baf53ff39a8c 
		     :count 2
		     :fallthru t)
		  (test-id)
		  (push test-id all-test-ids))
	       (tp* 
		  (  :id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 
		     :count nil
		     :fallthru t)
		  (test-id)
		  (typecase test-id
		     ;;Intercept clause, count it
		     (emt:test-ID:e-n:indexed-clause
			;;NOT emt:test-ID:e-n:form
			(emtp tp:798212b4-1abe-4779-beb1-baf53ff39a8c
			   (test-id))
			;;Don't try to run it, return instead.
			(throw 'emtp:tag-return nil))
		     ;;Let suite fall thru to handler
		     (emt:test-ID:e-n:suite t)
		     ;;We don't expect to see any other types of
		     ;;explores.
		     (t
			(error "This test shouldn't reach here")))))
	    (assert
	       (emt:util:all-different all-test-ids)
	       t))
	 t))
   

   (  "Situation: A clause has changed within a suite.
Operation: That test suite is run.
Behavior: The clause still gets the same test-ID as before."
      (let-noprops '(dummy-sym)
	 (emt:deftest-3 dummy-sym
	    (() (progn (emt:doc "Clause 1") (original-form))))

	 (let*
	    (  (old-test-id-list
		  (emtt:suite:th:get-id-list 'dummy-sym))
	       (old-test-id (car old-test-id-list)))
	    (assert (= (length old-test-id-list) 1) t)
	       
	    (emt:deftest-3 dummy-sym
	       (() (progn (emt:doc "Clause 1") (new-form))))

	       
	    (let*
	       (  (new-test-id-list
		     (emtt:suite:th:get-id-list 'dummy-sym))
		  (new-test-id (car new-test-id-list)))
	       (assert (= (length new-test-id-list) 1) t)

	       (assert 
		  (equal old-test-id new-test-id)
		  t)))
	 t)))

;;;_  . emtt:defun-at-point
;;emtt:defun-at-point is direct, it just joins other functions

;;;_   , emt:suite-sym-at-point

(emt:deftest-3 emt:suite-sym-at-point

   ;;Need example text, presumably from one or more example files.
   ;;They are in "t/examples/editor/find-names/1.el"

   ;;To set up point - mockbuf should provide mark conversion but it
   ;;doesn't yet.

   ;;Test situations: In a test definition.  In a function definition.
   ;;Between them.  After a definition (find that, not next).

   ;;So foo should be found for:
   ;;!Before all foo definitions.
   ;;!In function.
   ;;!Between function and test.
   ;;!In test definition.
   ;;!In test clause.

   ;;Something else or nothing (don't care) should be found for "After
   ;;all foo definitions" (not (eq (emt:suite-sym-at-point) 'foo))


   ;;Nothing should be found in empty file (Make from string)
   (()
      (with-buffer-containing-object 
	 (:string "\n\n\n\n")
	 (emt:doc "Situation: In empty file.")
	 (emt:doc "Response: Return `nil'.")
	 (emacs-lisp-mode)
	 (should
	    (null 
	       (emt:suite-sym-at-point))))))


;;;_   , Test data
(defconst emtt:launch:thd:examples
   (emt:eg:define+ ;;xmp:699d3b9f-cb26-4964-b23d-94b07a44d75d
      ((project emtest)(library tester)(section entry-points))
      (transparent-tags () (type))
      (group
	 ((count 2))
	 (item ((type name)) "example-2")
	 (item ((type count)) 2)
	 (item ((type suite-sym-list)) '(foo bar))
	 (item ((type file-load-history))
	    `(,(emt:eg (type name))
		,@(emt:eg (type suite-sym-list)) 
		(provide . example-2))))
   
      (group
	 ((count 1))
	 (item ((type name)) "example-1")
	 (item ((type count)) 1)
	 (item ((type suite-sym-list)) '(foo))
	 (item ((type file-load-history))
	    `(,(emt:eg (type name))
		,@(emt:eg (type suite-sym-list)) 
		(provide . example-1))))
   
      (item ((type load-history))
	 (emt:eg:map count nil
	    (emt:eg (type file-load-history))))))
;;;_   , emtt:lib-sym->suites

(rtest:deftest emtt:lib-sym->suites

   (  "Situation: There are two suites in the library.
Response: Return a list of those suites' symbols."
      (emtt:library:th ((count 2)) 
	 (let* 
	    ((syms (emtt:lib-sym->suites (emt:eg (type name)))))
	    (assert
	       (equal
		  (length syms)
		  (emt:eg (type count)))
	       t)))))

;;;_  . Tests
(rtest:deftest emtt:library

   (  "Situation: In the known (count 2) context of libraries and
suites.
Full exploration is used (Meaningless for now)
Operation: We launch a library
Response: The suite-handler part runs exactly twice
\(Not tested: Exactly those two suites are seen.)
\(Not tested: Those suites have distinct IDs.)
"
      (emt:eg:with emt:testral:thd:examples ()
	 (emtt:library:th ((count 2))
	    (emtp:eval
	       (emtt:library
		  (emt:eg (type name))
		  ;;$$Library does not return a result object yet.  Will
		  ;;type-check it.
		  ;;(emty:check x emt:result-group)
		  #'ignore)
	    
	       (tp-reached tp:798212b4-1abe-4779-beb1-baf53ff39a8c 
		  (emt:eg (type count)))
	       (tp* 
		  (  :id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 
		     :count nil
		     :fallthru t)
		  (test-id)
		  (typecase test-id
		     ;;Intercept suite
		     (emt:test-ID:e-n:suite
			;;A reached-point for counting invocations.
			(emtp tp:798212b4-1abe-4779-beb1-baf53ff39a8c
			   ())
			;;Don't try to explore its clauses, return
			;;instead.
			(throw 'emtp:tag-return nil))
		     ;;Let library fall thru to handler
		     (emt:test-ID:e-n:library:elisp-load t)
		     ;;We don't expect to see any other types of
		     ;;explores.
		     (t
			(error "This test shouldn't reach here")))))))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/launch/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/launch/rtest.el ends here
