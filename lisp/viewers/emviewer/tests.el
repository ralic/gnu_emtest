;;;_ viewers/emviewer/tests.el --- Emviewer tests

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

(require 'viewers/emviewer/testhelp) ;;For `emtest:ts:run-test'
;;$$RETHINK ME
;;Maybe should be the testhelp instead.
(require 'common/emt-persist) 
(require 'common/testral-types/testhelp)
;;;_. Body

;;Testing will be mostly separate for the formatters - maybe chewie
;;(and wookie) should support this sort of part comparison directly.
;;A test helper could take (object data formatter
;;persist-id-of-expectation)






;;End-to-end viewing.  
;; '
;; (emtt:ts:run-test 
;;    '("Situation: testing an example" 
;;        (error "An example error")) 
;;    #'emtest:viewer:receive)
;; '  ;;To show the result
;; (pp
;;    (let 
;;       ((l))
;;       (emtt:ts:run-test 
;; 	 '("Situation: testing an example" 
;; 	     (error "An example error")) 
;; 	 #'(lambda (r)
;; 	      (push r l)))
;;       l))


;; '
;; (emtest:ts:run-test
;;    '("Situation: testing an example" 
;;        (error "An example error")))


;;ABOUT PERSISTING


'
(emt:persist
   "dbid:5e14830f-e495-4d41-8fab-0fc9a1a9c4e9")

;;Generated automatically.
'  ;;Makes an id.
(emt:persist 
    "dbid:yygeryl0jwe0"
    '(persist
	"~/projects/emtest/lisp/viewers/emviewer/persist"))

;;To get the value (New function extracted from
;;`emt:funcall-handle-persistence-x')
'(emt:persist:value id)

;;How to manually set this to persist?  The old stuff was too hairy.
;;We're going to do it more simply now - the persist places a note and
;;either raises an error or returns the object.  Errors can cause
;;re-running if other values are available.  The error itself should
;;indicate that as far as the id can tell it.

;;They use an old idea of extracting from a "call" with multiple args,
;;and they want structured objects.




;;Could use use-local-map to set up a keymap for convenience.
;;Font lock - since it won't work non-interactively (?)
;;abort-recursive-edit - already on a key
;;accept buffer-string - an exploration of what we'll do later.
;;And then we need to store id, but since we're in a recursive edit,
;;it's available.


(defun emt:emviewer:th:check-buffer-string (id)
   ""
   ;;$$FIXME:  This still inserts each object twice.
   ;;Can set the current result to persist by:
   '(emt:db:set id 'correct-answer (buffer-string))

   ;;This works, after having set the persisting object.
   (let
      ((contents-matches-p
	  (equal
	     (buffer-string)
	     (condition-case err
		(emt:persist:value id)
		;;For now, can't be more specific than `error'
		(error
		   (message "Couldn't get persisting value")
		   (recursive-edit))))))

      (unless contents-matches-p
	 (message "Buffer string does not match")
	 ;;This is just for my manual handling.
      
	 ;;Font-locking via here doesn't work.
	 (recursive-edit))
   
      ;;Would like this + definition to be the whole form, but for now
      ;;we can't.
      (assert
	 (progn contents-matches-p)
	 t))
   t)

(rtest:deftest emviewer
   ;;Really a test of wookie interaction with LOAL.  Irrelevant until
   ;;chewie maker accepts a "data" param from the top.  
   ;;$$MOVE ME actually a chewie test
   '
   (  "Proves: Alist's value is available."
      (with-temp-buffer
	 (let
	    ((chewlist (chewie:2:make-list)))
	    ;;$$FIX ME Args here are wrong, out of date
	    (chewie:th:make-usual-chewie
	       ;;Format function ignores obj and returns a list of one
	       ;;string obtained from data.
	       #'(lambda (obj data)
		    (list 
		       (loal:val 'my-key data "Wrongwrong")))
	       ;;Dummy object
	       0
	       ;;$$SUPPORT ME Data parameter from the top
	       :data
	       (loal:acons 'my-key "abc" '()))
	 
	 
	    (emtb:buf-contents-matches
	       :string "abc"))))

   (  "Situation: Just the test runner."
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg (type report)(name just-test-runner)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist 
	       "dbid:yygeryl0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))
      
   ("Situation: Report one test-1."
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg (type report)(role original-add)(what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:mf660gq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))


   ("Situation: Report test-2."
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg (type report)(what-test test-2)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:l7w6gjq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))

   ("Operation: Test-runner plus update
 * Receive just test-runner
 * Then receive another report."
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg (type report)(name just-test-runner)))

	 (emtest:viewer:receive
	    (emt:eg 
	       (type report)
	       (role original-add)
	       (what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:o5w7zkc04xe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))

   ("Operation: Receives a report, then an update."
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg 
	       (type report)
	       (role original-add)
	       (what-test test-1)))

	 ;;Now receive the second one
	 (emtest:viewer:receive
	    (emt:eg 
	       (type report)
	       (role replace)
	       (what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:35ni7vd03xe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))
   
   ("Operation: Multiple.  
 * Receive just test-runner
 * receive another report
 * report an update."
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg (type report)(name just-test-runner)))

	 (emtest:viewer:receive
	    (emt:eg 
	       (type report)
	       (role original-add)
	       (what-test test-1)))

	 ;;Now receive the second one
	 (emtest:viewer:receive
	    (emt:eg 
	       (type report)
	       (role replace)
	       (what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:a3elivb04xe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))
   
      

   ("End to end test"

      ;;And capture the intermediate result stuff - gotta rewrite,
      ;;basically like above or like rewriting "receive" and also
      ;;testing it with set=.
      (emtve:ts:with-mock-viewer
	 (emtest:ts:run-test
	    '("Situation: testing an example" 
		(error "An example error")))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:6y9kxjq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))
   )

(emt:deftest-3
   ((of 'viewers/emviewer))

   ;;Tests of just the viewer.
   (()
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg (type report)(name just-test-runner)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist 
	       "dbid:yygeryl0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))
   
   (()
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg (type report)(role original-add)(what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:mf660gq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))


   (()
      (emtve:ts:with-mock-viewer
	 (emtest:viewer:receive
	    (emt:eg (type report)(what-test test-2)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:l7w6gjq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))
   
   ;;End to end test
   (()

      ;;And capture the intermediate result stuff - gotta rewrite,
      ;;basically like above or like rewriting "receive" and also
      ;;testing it with set=.
      (emtve:ts:with-mock-viewer
	 (emtest:ts:run-test
	    '("Situation: testing an example" 
		(error "An example error")))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:6y9kxjq0jwe0"
	       '(persist
		   "~/projects/emtest/lisp/viewers/emviewer/persist")))))
   


   )

;;;_. Purely testing testing

(emt:deftest-3 example-test-0
   ;;Clause 0, empty.
   (())
   
   )

(emt:deftest-3 example-test-1
   (()
      (error "An example error for `example-test-1'")
      )

   )

;;;_. Footers
;;;_ , Provides

(provide 'viewers/emviewer/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/emviewer/tests.el ends here
