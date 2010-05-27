;;;_ emtest/viewer/emviewer/rtest.el --- Emviewer tests

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: 

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

;; This file may be slightly misnamed.  It is largely combined tests
;; of emviewer and tester.  This proved the most sensible way to test
;; tester.


;;;_ , Requires

(require 'emtest/viewer/emviewer/testhelp)
;;$$RETHINK ME
;;Maybe should require the testhelp instead.
(require 'emtest/common/persist) 
(require 'emtest/common/testral-types/testhelp)
;;;_. Body

;;Testing will be mostly separate for the formatters - maybe chewie
;;(and wookie) should support this sort of part comparison directly.
;;A test helper could take (object data formatter
;;persist-id-of-expectation)


(rtest:deftest emviewer

   (  "Situation: Just the test runner."
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emtg (type report)(name just-test-runner)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist 
	       "dbid:yygeryl0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/emtest/viewer/emviewer/persist")))))
      
   ("Situation: Report one test-1."
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emtg (type report)(role original-add)(what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:mf660gq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/emtest/viewer/emviewer/persist")))))


   ("Situation: Report test-2."
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emtg (type report)(what-test test-2)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:l7w6gjq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/emtest/viewer/emviewer/persist")))))

   ("Operation: Test-runner plus update
 * Receive just test-runner
 * Then receive another report."
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emtg (type report)(name just-test-runner)))

	 (emtve:tester-cb
	    (emtg 
	       (type report)
	       (role original-add)
	       (what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:o5w7zkc04xe0"
	       '(persist
		   "~/projects/emtest/lisp/emtest/viewer/emviewer/persist")))))

   ("Operation: Receives a report, then an update."
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emtg 
	       (type report)
	       (role original-add)
	       (what-test test-1)))

	 ;;Now receive the second one
	 (emtve:tester-cb
	    (emtg 
	       (type report)
	       (role replace)
	       (what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:35ni7vd03xe0"
	       '(persist
		   "~/projects/emtest/lisp/emtest/viewer/emviewer/persist")))))
   
   ("Operation: Multiple.  
 * Receive just test-runner
 * receive another report
 * report an update."
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emtg (type report)(name just-test-runner)))

	 (emtve:tester-cb
	    (emtg 
	       (type report)
	       (role original-add)
	       (what-test test-1)))

	 ;;Now receive the second one
	 (emtve:tester-cb
	    (emtg 
	       (type report)
	       (role replace)
	       (what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:a3elivb04xe0"
	       '(persist
		   "~/projects/emtest/lisp/emtest/viewer/emviewer/persist")))))
   
      

   ("End to end test"

      ;;And capture the intermediate result stuff - gotta rewrite,
      ;;basically like above or like rewriting "receive" and also
      ;;testing it with set=.
      (emtve:ts:with-mock-viewer
	 (emtve:ts:run-test
	    '("Situation: testing an example" 
		(error "An example error")))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:6y9kxjq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/emtest/viewer/emviewer/persist")))))
   )

;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emviewer/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer/rtest.el ends here
