;;;_ emtest/runner/explorers/library/tests.el --- Tests for library

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

(require 'emtest/testhelp/tagnames)
(require 'emtest/runner/tester)
(require 'emtest/testhelp/standard)
(require 'emtest/runner/define)

;;;_. Body
;;;_  . Constants
;;;_   , emtt:launch:th:examples-dir
(defconst emtt:launch:th:examples-dir
      (emtb:expand-filename-by-load-file "examples/find-libs/") 
      "Directory where find-libs examples are" )

;;;_   , emtt:launch:thd:examples
(defconst emtt:launch:thd:examples
   (emtg:define+
      ((project emtest)(library tester)(section entry-points))
      (transparent-tags () (type))
      (group
	 ((count 2))
	 (item ((type name)) "example-2")
	 (item ((type lib-path))
	    (concat
		   emtt:launch:th:examples-dir
		   (emtg (type name))
		   ".el"))
	 (item ((type count)) 2)
	 (item ((type suite-sym-list)) '(foo bar))
	 ;;$RENAME ME Maybe rename it feature-sym
	 (item ((type sym)) (intern (emtg (type name))))
	 (item ((type file-load-history))
	    `( ,(emtg (type lib-path))
		,@(emtg (type suite-sym-list)) 
		(provide . ,(emtg (type sym))))))
      (group
	 ((count 1))
	 (item ((type name)) "example-1")
	 (item ((type count)) 1)
	 (item ((type lib-path))
	    (concat
	       emtt:launch:th:examples-dir
	       (emtg (type name))
	       ".el"))
	 (item ((type suite-sym-list)) '(foo))
	 (item ((type sym)) (intern (emtg (type name))))
	 (item ((type file-load-history))
	    `( ,(emtg (type lib-path))
		,@(emtg (type suite-sym-list)) 
		(provide . ,(emtg (type sym))))))


      (item ((type load-path-entry)(num 0))
	 emtt:launch:th:examples-dir)
      (item ((type load-path))
	 (emtg:map num nil
	    (emtg (type load-path-entry))))
      (item ((type load-history))
	 (emtg:map count nil
	    (emtg (type file-load-history))))))


;;;_   , emt:library:th

(defmacro emt:library:th (+tagset &rest body)
   "Run BODY in an environment with a certain example library defined.
+TAGSET is a tagset narrowing, as for `eg'."
   
   `(emtg:with emtt:launch:thd:examples ,+tagset
       (let
	  ((suite-sym-list (emtg (type suite-sym-list)))
	     (load-path (emtg:value 
			   :narrow ((type load-path))
			   :ignore-tags (count num)))
	     (load-history 
		(emtg:value 
		   :narrow ((type load-history)) 
		   :ignore-tags (count))))

	  ;;Define the suites (protected by a noprops)
	  (emth:let-noprops suite-sym-list
	     (dolist (sym suite-sym-list)
		(eval ,'`(emt:deftest-3 ,sym ())))
	     ;;Now do the tests
	     ,@body)
	  t)))
;;;_   , emtt:lib-path->lib-sym
(emt:deftest-3 emtt:lib-path->lib-sym
   (nil
      (emt:library:th ((count 2))
	 (emt:doc "Response: We get the lib symbol we expected.")
	 (assert
	    (eq
	       (emtt:lib-path->lib-sym (emtg (type lib-path)))
	       (emtg (type sym)))
	    t))))


;;;_   , emtt:lib-path-own-suites

(emt:deftest-3 emtt:lib-path-own-suites
   ;;Could loop over more examples
   (nil
      (progn
	 (emt:doc "Situation: There are two suites in the library.")
	 (emt:doc "Response: Return a list of those suites' symbols.")
	 (emt:library:th ((count 2))
	    (let*
	       ((syms
		   (emtt:lib-path-own-suites
		      (emtg (type lib-path)))))
	       (assert
		  (equal
		     (length syms)
		     (emtg
			(type count)))
		  t)
	       t)))))

;;;_  . Tests
(emt:deftest-3 emtest/runner/explorers/library
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
	 (emt:library:th ((count 2))
	    (emtp:eval
	       (emt:library
		  (emtg (type lib-path))
		  ;;Punt for now.
;; 		  #'(lambda (x)
;; 		       (emty:check x emt:testral:report))
		  
		  )
	       (tp-reached tp:798212b4-1abe-4779-beb1-baf53ff39a8c
		  (emtg
		     (type count)))
	       (tp*
		  (:id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 :count nil :fallthru t)
		  (test-id)
		  (typecase test-id
		     ;;Intercept suite
		     (emthow:suite
			;;A reached-point for counting invocations.
			(emtp tp:798212b4-1abe-4779-beb1-baf53ff39a8c nil)
			;;Don't try to explore its clauses, return
			;;instead.
			(throw 'emtp:tag-return nil))
		     ;;Make library itself fall thru to handler, whose
		     ;;behavior is what we're testing.
		     (emthow:library:elisp-load t)
		     ;;We don't expect to see any other types of
		     ;;explores.
		     (t
			(assert (not test-id) t)
			(error "This test shouldn't reach here")))))))))





;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/explorers/library/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/explorers/library/tests.el ends here
