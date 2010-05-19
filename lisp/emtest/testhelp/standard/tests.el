;;;_ emtest/testhelp/standard/tests.el --- Tests for standard testhelp

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
(require 'emtest/testhelp/eg)

;;Just because emt:wrap-form was tested with the persist examples.
(require 'emtest/testhelp/persist)  

;;;_. Body
;;;_ , emt:wrap-form
;;;_  . emt:wrap-form:thd:examples

(defconst emt:wrap-form:thd:examples
   (emt:eg:define+ ;;xmp:26ef7f1f-ce61-4284-8175-29a7fc7e4ef5
      ((project emtest)(library tester)(section emt:wrap-form))
      (group
	 ()
	 (item ((type form)(subtype original))
	    '(equal 1 2))
	 (item ((type form)(subtype wrapped))
	    '(emt:funcall #'equal 1 2)))))


;;;_  . emt:wrap-form ;; Obsolete
(emt:deftest-3 emt:wrap-form
   (nil
      (progn
	 (emt:doc "Shows: It wraps the examples as expected.")
	 (emt:eg:with emt:wrap-form:thd:examples
	    ((project emtest)
	       (library tester)
	       (section emt:wrap-form))
	    (assert
	       (equal
		  (emt:wrap-form
		     (emt:eg
			(type form)
			(subtype original)))
		  (emt:eg
		     (type form)
		     (subtype wrapped)))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Shows: The wrapped examples behave as expected")
	 (emt:eg:with emt:persist:thd:examples
	    ((project emtest)
	       (library persist)
	       (count 1))
	    (emt:db:internal:ts:mock
	       (emt:eg
		  (type whole-db))
	       (let
		  ((success
		      (equal
			 (emt:persist
			    (emt:eg
			       (type id))
			    'dummy)
			 (car
			    (emt:eg
			       (type values))))))
		  (assert
		     (not success)
		     nil))
	       (assert
		  (emt:funcall #'equal
		     (emt:persist
			(emt:eg
			   (type id))
			'dummy)
		     (car
			(emt:eg
			   (type values)))))
	       (assert
		  (eval
		     (emt:wrap-form
			'(equal
			    (emt:persist
			       (emt:eg
				  (type id))
			       'dummy)
			    (car
			       (emt:eg
				  (type values)))))))
	       t)))))

;;;_  . emt:should-f
(put 'emt:should-f 'emt:test-thru 'should)


;;;_  . emt:standard:thd:examples

(defconst emt:standard:thd:examples
   (emt:eg:define+ ;;xmp:bf4883a1-50ae-4fdf-815a-98ecdfc26a2a
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
	 (item ((type form)) '(emt:funcall #'equal 1 2))
	 (item ((type grade)) 'fail)
	 (item ((type diag-trace)) ;;$$CHANGE CALLERS Obsolete
	    '(list
		(make-emt:result:diag:call
		   :status    nil
		   :call-sexp '(equal 1 2)))))
   
      (group 
	 ((name fail-double-comparison))
	 (item ((type form)) '(or
				 (emt:funcall #'equal 1 2)
				 (emt:funcall #'equal 101 102)))
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
(emt:deftest-3 should
   (nil
      (progn
	 (emt:doc "Situation: A form that returns non-nil.")
	 (emt:doc "Response: Collect a passing grade.")
	 (emt:eg:with emt:standard:thd:examples
	    ((project emtest)
	       (library tester)
	       (name pass))
	    (let
	       ((stored-grades
		   (emt:should:th nil
		      (ignore-errors
			 (eval
			    `(should ,(emt:eg
					 (type form))))))))
	       (and
		  (equal
		     (length stored-grades)
		     1)
		  (let
		     ((grade
			 (car stored-grades)))
		     (and
			(equal
			   (emt:result:event:grade-grade grade)
			   (emt:eg
			      (type grade)))
			(equal
			   (emt:result:event:grade-form grade)
			   (emt:eg
			      (type form))))))))))
   (nil
      (progn
	 (emt:doc "Situation: A form that returns non-nil.
That forms is (null nil) which somehow has behaved oddly - different
than t.  So does (eq t t) Maybe forms behave differently, but they shouldn't.")
	 (emt:doc "Response: Collect a passing grade.")
	 (emt:eg:with emt:standard:thd:examples
	    ((project emtest)
	       (library tester)
	       (name pass))
	    (let
	       ((stored-grades
		   (emt:should:th nil
		      (ignore-errors
			 (eval
			    `(should
				(null nil)))))))
	       (and
		  (equal
		     (length stored-grades)
		     1)
		  (let
		     ((grade
			 (car stored-grades)))
		     (and
			(equal
			   (emt:result:event:grade-grade grade)
			   (emt:eg
			      (type grade)))
			(equal
			   (emt:result:event:grade-form grade)
			   (emt:eg
			      (type form))))))))))
   (nil
      (progn
	 (emt:doc "Situation: A form that returns nil.")
	 (emt:doc "Response: Collect a failing grade.")
	 (emt:eg:with emt:standard:thd:examples
	    ((project emtest)
	       (library tester)
	       (name fail))
	    (let
	       ((stored-grades
		   (emt:should:th nil
		      (ignore-errors
			 (eval
			    `(should ,(emt:eg
					 (type form))))))))
	       (and
		  (equal
		     (length stored-grades)
		     1)
		  (let
		     ((grade
			 (car stored-grades)))
		     (and
			(equal
			   (emt:result:event:grade-grade grade)
			   (emt:eg
			      (type grade)))
			(equal
			   (emt:result:event:grade-form grade)
			   (emt:eg
			      (type form))))))))))
   (nil
      (progn
	 (emt:doc "Event: Something in the form errors.")
	 (emt:doc "Behavior: Collect an ungraded grade.")
	 (emt:eg:with emt:standard:thd:examples
	    ((project emtest)
	       (library tester)
	       (name ungraded))
	    (let
	       ((stored-grades
		   (emt:should:th nil
		      (ignore-errors
			 (eval
			    `(should ,(emt:eg
					 (type form))))))))
	       (and
		  (equal
		     (length stored-grades)
		     1)
		  (let
		     ((grade
			 (car stored-grades)))
		     (and
			(equal
			   (emt:result:event:grade-grade grade)
			   (emt:eg
			      (type grade)))
			(equal
			   (emt:result:event:grade-form grade)
			   (emt:eg
			      (type form))))))))))
   (nil
      (progn
	 (emt:doc "Event: Something in the form provides diagnostic trace")
	 (emt:doc "Behavior: Collect the diagnostic trace.  Grade as usual")
	 (emt:eg:with emt:standard:thd:examples
	    ((project emtest)
	       (library tester)
	       (name fail-comparison))
	    (let
	       ((stored-grades
		   (emt:should:th nil
		      (ignore-errors
			 (eval
			    `(should ,(emt:eg
					 (type form))))))))
	       (and
		  (equal
		     (length stored-grades)
		     1)
		  (let
		     ((grade
			 (car stored-grades)))
		     (and
			(equal
			   (emt:result:event:grade-grade grade)
			   (emt:eg
			      (type grade)))
			(equal
			   (emt:result:event:grade-form grade)
			   (emt:eg
			      (type form)))
			'(equal
			    (emt:result:event:grade-diagnostic-info grade)
			    (emt:eg
			       (type diag-trace))))))))))
   (nil
      (progn
	 (emt:doc "Event: Something in the form errors.")
	 (emt:doc "Behavior: Collect the appropriate diagnostic trace.")
	 (emt:eg:with emt:standard:thd:examples
	    ((project emtest)
	       (library tester)
	       (name ungraded))
	    (let
	       ((stored-grades
		   (emt:should:th nil
		      (ignore-errors
			 (eval
			    `(should ,(emt:eg
					 (type form))))))))
	       (and
		  (equal
		     (length stored-grades)
		     1)
		  (let
		     ((grade
			 (car stored-grades)))
		     (and
			(equal
			   (emt:result:event:grade-grade grade)
			   (emt:eg
			      (type grade)))
			(equal
			   (emt:result:event:grade-form grade)
			   (emt:eg
			      (type form)))
			'(equal
			    (emt:result:event:grade-diagnostic-info grade)
			    (emt:eg
			       (type diag-trace))))))))))
   (nil
      (progn
	 (emt:doc "Situation: Several things are traced")
	 (emt:doc "Response: Trace occurs in forward order, not reversed (ie, it gets
unreversed after all the pushing)")
	 (emt:eg:with emt:standard:thd:examples
	    ((project emtest)
	       (library tester)
	       (name fail-double-comparison))
	    (let
	       ((stored-grades
		   (emt:should:th nil
		      (ignore-errors
			 (eval
			    `(should ,(emt:eg
					 (type form))))))))
	       (and
		  (equal
		     (length stored-grades)
		     1)
		  (let
		     ((grade
			 (car stored-grades)))
		     (and
			(equal
			   (emt:result:event:grade-grade grade)
			   (emt:eg
			      (type grade)))
			(equal
			   (emt:result:event:grade-form grade)
			   (emt:eg
			      (type form)))
			'(equal
			    (emt:result:event:grade-diagnostic-info grade)
			    (emt:eg
			       (type diag-trace)))))))))))


;;;_  . emtt:map&trap

(emt:deftest-3 emtt:map&trap
   (nil
      (progn
	 (emt:doc "Proves: Maps over values.")
	 (assert
	    (equal
	       (emtt:map&trap
		  #'eval
		  '(12 (+ 1 2)))
	       '(12 3)))))
   
   (nil
      (let ((a 0))
	 (emt:doc "Situation: First form errors. Second one increments
      a variable")
	 (assert
	    (emt:gives-error
	       (emtt:map&trap
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

(provide 'emtest/testhelp/standard/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/standard/tests.el ends here
