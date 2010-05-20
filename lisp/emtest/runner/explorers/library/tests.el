;;;_ tests.el --- Tests for library

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

(require 'emtest/testhelp/eg)
(require 'emtest/runner/tester)
(require 'emtest/testhelp/standard)
(require 'emtest/runner/define)

;;;_. Body
;;;_  . Constants
;;;_   , emtt:launch:th:examples-dir
(defconst emtt:launch:th:examples-dir
      (emt:expand-filename-by-load-file "examples/find-libs/") 
      "Directory where find-libs examples are" )

;;;_   , emtt:launch:thd:examples
(defconst emtt:launch:thd:examples
   (emt:eg:define+
      ((project emtest)(library tester)(section entry-points))
      (transparent-tags () (type))
      (group
	 ((count 2))
	 (item ((type name)) "example-2")
	 (item ((type count)) 2)
	 (item ((type suite-sym-list)) '(foo bar))
	 (item ((type sym)) (intern-soft (emt:eg (type name))))
	 (item ((type file-load-history))
	    `( ,(concat
		   emtt:launch:th:examples-dir
		   (emt:eg (type name))
		   ".el")
		,@(emt:eg (type suite-sym-list)) 
		(provide . ,(emt:eg (type sym))))))
   
      (group
	 ((count 1))
	 (item ((type name)) "example-1")
	 (item ((type count)) 1)
	 (item ((type suite-sym-list)) '(foo))
	 (item ((type sym)) (intern-soft (emt:eg (type name))))
	 (item ((type file-load-history))
	    `( ,(concat
		   emtt:launch:th:examples-dir
		   (emt:eg (type name))
		   ".el")
		,@(emt:eg (type suite-sym-list)) 
		(provide . ,(emt:eg (type sym))))))


      (item ((type load-path-entry)(num 0))
	 emtt:launch:th:examples-dir)
      (item ((type load-path))
	 (emt:eg:map num nil
	    (emt:eg (type load-path-entry))))
      (item ((type load-history))
	 (emt:eg:map count nil
	    (emt:eg (type file-load-history))))))


;;;_   , emtt:library:th

(defmacro emtt:library:th 
   (+tagset &rest body)
   "Run BODY in an environment with a certain example library defined.
+TAGSET is a tagset narrowing, as for `eg'."
   
   `(emt:eg:with emtt:launch:thd:examples ,+tagset
       (let
	  ((suite-sym-list (emt:eg (type suite-sym-list)))
	     (load-path (emt:eg:value 
			   :narrow ((type load-path))
			   :ignore-tags (count num)))
	     (load-history 
		(emt:eg:value 
		   :narrow ((type load-history)) 
		   :ignore-tags (count))))

	  ;;Define the suites (protected by a noprops)
	  (let-noprops suite-sym-list
	     (dolist (sym suite-sym-list)
		(eval ,'`(emt:deftest-3 ,sym ())))
	     ;;Now do the tests
	     ,@body)
	  t)))

;;;_   , emtt:lib-sym->suites

(emt:deftest-3 emtt:lib-sym->suites
   ;;Could loop over more examples
   (nil
      (progn
	 (emt:doc "Situation: There are two suites in the library.")
	 (emt:doc "Response: Return a list of those suites' symbols.")
	 (emtt:library:th
	    ((count 2))
	    (let*
	       ((syms
		   (emtt:lib-sym->suites
		      (emt:eg
			 (type sym)))))
	       (assert
		  (equal
		     (length syms)
		     (emt:eg
			(type count)))
		  t)
	       t)))))

;;;_  . Tests
(emt:deftest-3 emtt:library
   (nil
      (progn
	 (emt:doc "Situation: In the known (count 2) context of libraries and
suites.
Full exploration is used (Meaningless for now)")
	 (emt:doc "Operation: We launch a library")
	 (emt:doc "Response: The suite-handler part runs exactly twice
\\(Not tested: Exactly those two suites are seen.)
\\(Not tested: Those suites have distinct IDs.)
")
	 (emtt:library:th
	    ((count 2))
	    (emtp:eval
	       (emtt:library
		  (emt:eg
		     (type sym))
		  ;;$$FIX ME Library does not return a result object yet.  Will
		  ;;type-check it.  But for now, we can't pass any
		  ;;argument. 
		  ;;(emty:check x emt:result-group)
		  #'ignore)
	       (tp-reached tp:798212b4-1abe-4779-beb1-baf53ff39a8c
		  (emt:eg
		     (type count)))
	       (tp*
		  (:id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 :count nil :fallthru t)
		  (test-id)
		  (typecase test-id
		     ;;Intercept suite
		     (emt:test-ID:e-n:suite
			;;A reached-point for counting invocations.
			(emtp tp:798212b4-1abe-4779-beb1-baf53ff39a8c nil)
			;;Don't try to explore its clauses, return
			;;instead.
			(throw 'emtp:tag-return nil))
		     ;;Make library itself fall thru to handler, whose
		     ;;behavior is what we're testing.
		     (emt:test-ID:e-n:library:elisp-load t)
		     ;;We don't expect to see any other types of
		     ;;explores.
		     (t
			(error "This test shouldn't reach here")))))))))





;;;_. Footers
;;;_ , Provides

(provide 'tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tests.el ends here
