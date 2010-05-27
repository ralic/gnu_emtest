;;;_ emtest/testhelp/standard/rtest.el --- Rtest tests for standard testhelp

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


(require 'rtest-define)
(require 'emtest/testhelp/tagnames)

;;Just because emth:wrap-form was tested with the persist examples.
(require 'emtest/testhelp/persist)  

;;;_. Body
;;;_ , emth:wrap-form
;;;_  . emth:wrap-form:thd:examples
'
(defconst emth:wrap-form:thd:examples
   (emtg:define+ ;;xmp:26ef7f1f-ce61-4284-8175-29a7fc7e4ef5
      ((project emtest)(library tester)(section emth:wrap-form))
      (group
	 ()
	 (item ((type form)(subtype original))
	    '(equal 1 2))
	 (item ((type form)(subtype wrapped))
	    '(emt:funcall #'equal 1 2)))))


;;;_  . emth:wrap-form ;; Obsolete
'
(rtest:deftest emth:wrap-form

   (  "Shows: It wraps the examples as expected."
      (emtg:with emth:wrap-form:thd:examples
	 ((project emtest)(library tester)(section emth:wrap-form))
	 (assert
	    (equal
	       (emth:wrap-form (emtg (type form)(subtype original)))
	        (emtg (type form)(subtype wrapped)))
	    t)
	 t))

   ("Shows: The wrapped examples behave as expected"
      (emtg:with emt:persist:thd:examples
	 ((project emtest)(library persist)(count 1))
	 (emt:db:internal:ts:mock
	    (emtg (type whole-db))

	    ;;A version that doesn't de-reference the placeholder. It
	    ;;fails.  Make sure this stays outside of "should" - but see
	    ;;redesign. 
	    (let
	       ((success
		   (equal
		      (emt:persist (emtg (type id)) 'dummy)
		      (car (emtg (type values))))))
	       ;;And pass both values.
	       (assert (not success) nil))
	 
	 
	    ;;A version that explicitly uses emt:funcall. It succeeds
	    (assert
	       (emt:funcall
		  #'equal
		  (emt:persist (emtg (type id)) 'dummy)
		  (car (emtg (type values)))))

	    ;;A version that implicitly uses emt:funcall.  `emth:wrap-form'
	    ;;adds it.

	    (assert
	       (eval
		  (emth:wrap-form
		     '(equal
			 (emt:persist (emtg (type id)) 'dummy)
			 (car (emtg (type values)))))))
	       
	    t))))
;;;_  . emth:should-f
(put 'emth:should-f 'rtest:test-thru
   'should)

;;;_  . emt:standard:thd:examples

(defconst emt:standard:thd:examples
   (emtg:define+ ;;xmp:bf4883a1-50ae-4fdf-815a-98ecdfc26a2a
      ((project emtest) (library tester))
      (group 
	 ((name pass))
	 (item ((type form)) t)
	 (item ((type grade)) 'pass)
	 (item ((type diag-trace)) ())
	 )
      ;;$$DISTINGUISH ME
      ;;$$USE ME
      ;;    (group 
      ;;       ((name pass))
      ;;       (item ((type form)) '(eq t t))
      ;;       (item ((type grade)) 'pass)
      ;;       (item ((type diag-trace)) ())
      ;;       )
      ;;    (group 
      ;;       ((name pass))
      ;;       (item ((type form)) '(null nil))
      ;;       (item ((type grade)) 'pass)
      ;;       (item ((type diag-trace)) ())
      ;;       )
      (group 
	 ((name fail))
	 (item ((type form)) nil)
	 (item ((type grade)) 'fail)
	 (item ((type diag-trace)) ())
	 )
      (group 
	 ((name ungraded))
	 (item ((type form)) '(error "I will signal an error"))
	 (item ((type grade)) 'ungraded)
	 (item ((type diag-trace)) ;;$$CHANGE CALLERS Obsolete
	    '
	    (list
	       (make-emt:result:diag:error
		  :error '(error "I will signal an error")))))
   
      ;;A group that has a simple diag trace.
      ;;Partly shared with emt:funcall:th:3.
      (group 
	 ((name fail-comparison))
	 (item ((type form)) ;;'(emt:funcall #'equal 1 2)
	    '(equal 1 2))
	 (item ((type grade)) 'fail)
	 (item ((type diag-trace)) ;;$$CHANGE CALLERS Obsolete
	    '(list
		(make-emt:result:diag:call
		   :status    nil
		   :call-sexp '(equal 1 2)))))
   
      (group 
	 ((name fail-double-comparison))
	 (item ((type form)) '(or
				 ;;(emt:funcall #'equal 1 2)
				 (equal 1 2)
				 ;;(emt:funcall #'equal 101 102)
				 (equal 101 102)))
	 (item ((type grade)) 'fail)
	 (item ((type diag-trace)) ;;$$CHANGE CALLERS Obsolete
	    '
	    (list
	       (make-emt:result:diag:call
		  :status    nil
		  :call-sexp '(equal 1 2))
	       (make-emt:result:diag:call
		  :status    nil
		  :call-sexp '(equal 101 102)))))))

;;;_   , should
(rtest:deftest should
   
   (  "Situation: A form that returns non-nil.
Response: Collect a passing grade."
      (emtg:with emt:standard:thd:examples
	 ((project emtest) (library tester)(name pass))
	 (let
	    ((stored-grades 
		(emth:should:th () 
		   (ignore-errors
		      (eval `(should ,(emtg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emtg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emtg (type form)))
		     ))))))
   
(  "Situation: A form that returns non-nil.
That forms is (null nil) which somehow has behaved oddly - different
than t.  So does (eq t t) Maybe forms behave differently, but they shouldn't.
Response: Collect a passing grade."
      (emtg:with emt:standard:thd:examples
	 ((project emtest) (library tester)(name pass))
	 (let
	    ((stored-grades 
		(emth:should:th () 
		   (ignore-errors
		      (eval `(should (null nil)))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emtg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emtg (type form)))
		     ))))))   
   
   
   
   (  "Situation: A form that returns nil.
Response: Collect a failing grade."
      (emtg:with emt:standard:thd:examples
	 ((project emtest) (library tester)(name fail))
	 (let
	    ((stored-grades 
		(emth:should:th () 
		   (ignore-errors
		      (eval `(should ,(emtg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emtg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emtg (type form)))
		     ))))))


   (  "Event: Something in the form errors.
Behavior: Collect an ungraded grade."
      (emtg:with emt:standard:thd:examples
	 ((project emtest) (library tester)(name ungraded))
	 (let
	    ((stored-grades 
		(emth:should:th () 
		   (ignore-errors
		      (eval `(should ,(emtg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emtg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emtg (type form)))
		     ))))))

   	 ;;$$`should' should not invoke the debugger inside it,
	 ;;except if deliberately made to do so.  Test
	 ;;and fix this. It does in the test of
         ;;`emtel:suite-sym-at-point', but that may be because that's in
         ;;rtest not emtest yet.  That may be considered acceptable.
         ;;However, emtest isn't yet handling those errors.  That's a
         ;;problem. 



   (  "Event: Something in the form provides diagnostic trace
Behavior: Collect the diagnostic trace.  Grade as usual"
      (emtg:with emt:standard:thd:examples
	 ((project emtest) (library tester)(name fail-comparison))
	 (let
	    ((stored-grades 
		(emth:should:th () 
		   (ignore-errors
		      (eval `(should ,(emtg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emtg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emtg (type form)))
		     '  ;;$$REWRITE ME for TESTRAL
		     (equal
			(emt:result:event:grade-diagnostic-info grade)
			(emtg (type diag-trace)))
		     
		     ))))))



   (  "Event: Something in the form errors.
Behavior: Collect the appropriate diagnostic trace."
      (emtg:with emt:standard:thd:examples
	 ((project emtest) (library tester)(name ungraded))
	 (let
	    ((stored-grades 
		(emth:should:th () 
		   (ignore-errors
		      (eval `(should ,(emtg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emtg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emtg (type form)))
		     '  ;;$$REWRITE ME for TESTRAL
		     (equal
			(emt:result:event:grade-diagnostic-info grade)
			(emtg (type diag-trace)))
		     
		     ))))))

   (  "Situation: Several things are traced
Response: Trace occurs in forward order, not reversed (ie, it gets
unreversed after all the pushing)"
      (emtg:with emt:standard:thd:examples
	 ((project emtest) (library tester)(name fail-double-comparison))
	 (let
	    ((stored-grades 
		(emth:should:th () 
		   (ignore-errors
		      (eval `(should ,(emtg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emtg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emtg (type form)))
		     '  ;;$$REWRITE ME for TESTRAL
		     (equal
			(emt:result:event:grade-diagnostic-info grade)
			(emtg (type diag-trace)))
		     
		     ))))))



   ;;We wrap `equal' etc in `emt:funcall', so it can understand
   ;;persists, etc.  But we actually test `emth:wrap-form', because
   ;;`should' doesn't have a nice test-disclosing return value nor
   ;;reliably throw an error.

   

   ;;Not yet:

   ;;When emt:trace:current-event-list is not bound, if form fails,
   ;;signal an error.


   ;;Analyze the form, if functor is `and', `or', `if'.
   ;;Pre-requisite: a tracer along the lines of emt:funcall, but for
   ;;logic forms.

   ;;Give style warning if it splits in top-like context (immediate
   ;;`and', etc) 

   ;;Errors get passed upwards as special "already processed" errors
   ;;that just stop the test.  `rtest:gives-error' is going to have to
   ;;understand that properly.  And that could mess up processing when
   ;;not in emtest.  "should" is only in emtest, but comparisons can
   ;;run anywhere.

   ;;If `doc' errors, that should not keep the form from running.
   )

;;;_  . emth:map&trap

(emt:deftest-3 emth:map&trap
   (nil
      (progn
	 (emt:doc "Proves: Maps over values.")
	 (assert
	    (equal
	       (emth:map&trap
		  #'eval
		  '(12 (+ 1 2)))
	       '(12 3)))))
   
   (nil
      (let ((a 0))
	 (emt:doc "Situation: First form errors. Second one increments
      a variable")
	 (assert
	    (emth:gives-error
	       (emth:map&trap
		  #'eval 
		  '((error "Example error")
		      (incf a)))))
	 
	 (emt:doc "Response: The error is raised, which shows that the
	 first clause ran.") 
	 (emt:doc "Response: The variable has been incremented, which
	 shows that the second clause ran.")
	 (assert
	    (equal a 1)
	    t))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/standard/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/standard/rtest.el ends here
