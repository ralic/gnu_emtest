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


(require 'emtest/runner/explorers/library)
(require 'emtest/runner/explorers/library/testhelp)
(require 'emtest/runner/launch)
(require 'emtest/runner/define)
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/testpoint)
(require 'emtest/testhelp/misc)

;;;_. Body

;;;_ , emtt:lib-path->lib-sym
(emt:deftest-3 emtt:lib-path->lib-sym
   (nil
      (emt:library:th ((count 2))
	 (emt:doc "Response: We get the lib symbol we expected.")
	 (emt:assert
	    (eq
	       (emtt:lib-path->lib-sym (emtg (type lib-path)))
	       (emtg (type sym)))))))


;;;_ , emtt:lib-path-own-suites

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
	       (emt:assert
		  (equal
		     (length syms)
		     (emtg
			(type count))))
	       t)))))

;;;_ , emtest/runner/explorers/library
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
			(emt:assert (not test-id))
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
