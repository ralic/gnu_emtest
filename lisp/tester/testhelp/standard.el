;;;_ testhelp/standard.el --- Standard testhelp for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint

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
(require 'common/testral-types)

;;;_. Body
;;;_ , Doc

;;;_  . emt:doc
;;;###autoload
(defun emt:doc (str &rest r)
   ""
   (push
      (make-emt:testral:doc :str str)
      emt:testral:*events-seen*))

;;;_ , "should"
;;;_  . emt:wrap-form
(defun emt:wrap-form (form)
   ""

   (if
      (and
	 (listp form)
	 (eq (car form) 'equal))
      `(emt:funcall (function ,(car form)) ,@(cdr form))
      form))
;;;_   , Examples

(emt:eg:define xmp:26ef7f1f-ce61-4284-8175-29a7fc7e4ef5
   ((project emtest)(library tester)(section emt:wrap-form))
   (group
      ()
      (item ((type form)(subtype original))
	 '(equal 1 2))
      (item ((type form)(subtype wrapped))
	 '(emt:funcall #'equal 1 2))))



;;;_   , Tests
(rtest:deftest emt:wrap-form

   (  "Shows: It wraps the examples as expected."
      (emt:eg:narrow 
	 ((project emtest)(library tester)(section emt:wrap-form))
	 (assert
	    (equal
	       (emt:wrap-form (emt:eg (type form)(subtype original)))
	        (emt:eg (type form)(subtype wrapped)))
	    t)
	 t))

   ("Shows: The wrapped examples behave as expected"
      (emt:eg:narrow 
	 ((project emtest)(library persist)(count 1))
	 (emt:db:internal:ts:mock
	    (emt:eg (type whole-db))

	    ;;A version that doesn't de-reference the placeholder. It
	    ;;fails.  Make sure this stays outside of "should" - but see
	    ;;redesign. 
	    (let
	       ((success
		   (equal
		      (emt:persist (emt:eg (type id)) 'dummy)
		      (car (emt:eg (type values))))))
	       ;;And pass both values.
	       (assert (not success) nil))
	 
	 
	    ;;A version that explicitly uses emt:funcall. It succeeds
	    (assert
	       (emt:funcall
		  #'equal
		  (emt:persist (emt:eg (type id)) 'dummy)
		  (car (emt:eg (type values)))))

	    ;;A version that implicitly uses emt:funcall.  `emt:wrap-form'
	    ;;adds it.

	    (assert
	       (eval
		  (emt:wrap-form
		     '(equal
			 (emt:persist (emt:eg (type id)) 'dummy)
			 (car (emt:eg (type values)))))))
	       
	    t)))
   )

;;;_  . emt:should-f
(defun emt:should-f (form)
   ""
   ;;$ADDME When `emt:testral:*parent-id*' etc are not bound, act like
   ;;plain `assert'.  Or possibly push results onto a ring that can be
   ;;examined. 

   ;;$$CHANGEME.  This will change to just send notes to a listener
   (let
      ((report 
	  ;;Make&send a `emt:testral:check:push'.  Use
	  ;;(emt:testral:add-note start-note '(should))
	  (make-emt:result:event:grade
	     :form form))
	 (parent-id emt:testral:*parent-id*)
	 ;;Use a counter or a uuid.
	 (id (incf emt:testral:*id-counter*))
	 ;;Bind a new parent id
	 (emt:testral:*parent-id* id)
	 
	 ;;Create empty badnesses, just because `unwind-protect' must
	 ;;see it
	 (badnesses '()))
      (push
	 (make-emt:testral:check:push
	    :info (list (list 'form form))
	    :parent-id parent-id
	    :id id)
	 emt:testral:*events-seen*)
      


      ;;`unwind-protect' this, in place of the last form:
      ;;Make&send a `emt:testral:check:pop'.  To know
      ;;badnesses, if any: Each case assigns to the `badnesses'
      ;;variable, which we use in making that. 
      
      ;;$$CHANGEME This protect call is obsolete, assumes the old
      ;;style and special behavior isn't needed with the new design.
      ;;So build things normally and just use `unwind-protect'
      (emt:trace:protect
	 (condition-case err
	    (let*
	       (  
		  (form-x (emt:wrap-form form))
		  (retval
		     (eval form-x)))
	       
	       ;;Obsolete
	       (setf (emt:result:event:grade-grade report) 
		  (if retval 'pass 'fail))
	       ;;NEW
	       (unless retval
		  (push '(failed) badnesses))
	       retval)

	    ;;$$ADDME There'd be a clause to intercept a special
	    ;;"dormant" error too, for contributing checks that were
	    ;;disabled.

	    ;;And a `many-bads' error that directly held a collected
	    ;;list of badnesses?  In case `and' both fails and is
	    ;;dormant.  Maybe that's the special error to use always.

	    ;;Add to badnesses, unless already there.
	    ('emt:already-handled 
	       ;;NEW
	       (push '(ungraded) badnesses)
	       ;;Obsolete
	       (setf (emt:result:event:grade-grade report) 'ungraded)
	       (signal 'emt:already-handled ()))
	    ;;Add to badnesses, unless already there.
	    (error
	       ;;NEW
	       (push '(ungraded) badnesses)
	       ;;Obsolete
	       (setf (emt:result:event:grade-grade report) 'ungraded)
	       ;;Obsolete
	       (emt:trace:add-to-stored-diag
		  (make-emt:result:diag:error :error err))
	       (signal (car err)(cdr err))))

	 ;;$$REMOVEME These are obsolete
	 (setf (emt:result:event:grade-diagnostic-info report) 
	    diags)
	 (setf (emt:result:event:grade-info-about report) 
	    info-about)
	 (if
	    (boundp 'emt:trace:current-event-list)
	    (push report emt:trace:current-event-list)
	    ;;$$Want an error specific to emtest
	    (signal 'error report))

	 ;;This is now the meat of the operation.
	 (push
	    (make-emt:testral:check:pop
	       :parent-id parent-id
	       :id id
	       ;;Punt
	       :badnesses badnesses)
	    emt:testral:*events-seen*))))


;;;_   , Tests
(put 'emt:should-f 'rtest:test-thru
   'should)

;;;_  . should
;;;###autoload
(defmacro* should (form &key doc)
   ""

   `(emt:should-f ',form))

;;;_   , Test data

(emt:eg:define xmp:bf4883a1-50ae-4fdf-815a-98ecdfc26a2a
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
      (item ((type diag-trace)) 
	 (list
	    (make-emt:result:diag:error
	       :error '(error "I will signal an error")))))
   
   ;;A group that has a simple diag trace.
   ;;Partly shared with emt:funcall:th:3.
   (group 
      ((name fail-comparison))
      (item ((type form)) '(emt:funcall #'equal 1 2))
      (item ((type grade)) 'fail)
      (item ((type diag-trace)) 
	 (list
	    (make-emt:result:diag:call
	       :status    nil
	       :call-sexp '(equal 1 2)))))
   
   (group 
      ((name fail-double-comparison))
      (item ((type form)) '(or
			      (emt:funcall #'equal 1 2)
			      (emt:funcall #'equal 101 102)))
      (item ((type grade)) 'fail)
      (item ((type diag-trace)) 
	 (list
	    (make-emt:result:diag:call
	       :status    nil
	       :call-sexp '(equal 1 2))
	    (make-emt:result:diag:call
	       :status    nil
	       :call-sexp '(equal 101 102)))))
   
   )


;;;_   , Test helper

(defmacro* emt:should:th 
   ((&key 
       initial-stored 
       (report-control emt:report-control:thd:report-all))
      &rest body)
   ""
   
   `(let
       ((emt:trace:current-event-list ,initial-stored)
	  ;;Other control such as report-control is not supported yet,
	  ;;so can't control it here.
	  )
       
       (list
	  (progn ,@body)
	  emt:trace:current-event-list)
       ;;We aren't interested in "should"s return value
       emt:trace:current-event-list))

;;;_   , Tests
(rtest:deftest should
   
   (  "Situation: A form that returns non-nil.
Response: Collect a passing grade."
      (emt:eg:narrow 
	 ((project emtest) (library tester)(name pass))
	 (let
	    ((stored-grades 
		(emt:should:th () 
		   (ignore-errors
		      (eval `(should ,(emt:eg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emt:eg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emt:eg (type form)))
		     ))))))
   
(  "Situation: A form that returns non-nil.
That forms is (null nil) which somehow has behaved oddly - different
than t.  So does (eq t t) Maybe forms behave differently, but they shouldn't.
Response: Collect a passing grade."
      (emt:eg:narrow 
	 ((project emtest) (library tester)(name pass))
	 (let
	    ((stored-grades 
		(emt:should:th () 
		   (ignore-errors
		      (eval `(should (null nil)))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emt:eg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emt:eg (type form)))
		     ))))))   
   
   
   
   (  "Situation: A form that returns nil.
Response: Collect a failing grade."
      (emt:eg:narrow 
	 ((project emtest) (library tester)(name fail))
	 (let
	    ((stored-grades 
		(emt:should:th () 
		   (ignore-errors
		      (eval `(should ,(emt:eg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emt:eg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emt:eg (type form)))
		     ))))))


   (  "Event: Something in the form errors.
Behavior: Collect an ungraded grade."
      (emt:eg:narrow 
	 ((project emtest) (library tester)(name ungraded))
	 (let
	    ((stored-grades 
		(emt:should:th () 
		   (ignore-errors
		      (eval `(should ,(emt:eg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emt:eg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emt:eg (type form)))
		     ))))))

   	 ;;$$`should' should not invoke the debugger inside it,
	 ;;except if deliberately made to do so.  Test
	 ;;and fix this. It does in the test of
         ;;`emt:suite-sym-at-point', but that may be because that's in
         ;;rtest not emtest yet.  That may be considered acceptable.
         ;;However, emtest isn't yet handling those errors.  That's a
         ;;problem. 



   (  "Event: Something in the form provides diagnostic trace
Behavior: Collect the diagnostic trace.  Grade as usual"
      (emt:eg:narrow 
	 ((project emtest) (library tester)(name fail-comparison))
	 (let
	    ((stored-grades 
		(emt:should:th () 
		   (ignore-errors
		      (eval `(should ,(emt:eg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emt:eg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emt:eg (type form)))
		     (equal
			(emt:result:event:grade-diagnostic-info grade)
			(emt:eg (type diag-trace)))
		     
		     ))))))



   (  "Event: Something in the form errors.
Behavior: Collect the appropriate diagnostic trace."
      (emt:eg:narrow 
	 ((project emtest) (library tester)(name ungraded))
	 (let
	    ((stored-grades 
		(emt:should:th () 
		   (ignore-errors
		      (eval `(should ,(emt:eg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emt:eg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emt:eg (type form)))
		     (equal
			(emt:result:event:grade-diagnostic-info grade)
			(emt:eg (type diag-trace)))
		     
		     ))))))

   (  "Situation: Several things are traced
Response: Trace occurs in forward order, not reversed (ie, it gets
unreversed after all the pushing)"
      (emt:eg:narrow 
	 ((project emtest) (library tester)(name fail-double-comparison))
	 (let
	    ((stored-grades 
		(emt:should:th () 
		   (ignore-errors
		      (eval `(should ,(emt:eg (type form))))))))

	    (and
	       (equal (length stored-grades) 1)
	       (let
		  ((grade (car stored-grades)))
		  (and
		     (equal (emt:result:event:grade-grade grade) 
			(emt:eg (type grade)))
		     (equal 
			(emt:result:event:grade-form grade) 
			(emt:eg (type form)))
		     (equal
			(emt:result:event:grade-diagnostic-info grade)
			(emt:eg (type diag-trace)))
		     
		     ))))))



   ;;We wrap `equal' etc in `emt:funcall', so it can understand
   ;;persists, etc.  But we actually test `emt:wrap-form', because
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

;;;_ , "stage"
;;See  [[id:47ad9e14-9a38-40e2-a5ea-91cbc4dfb97f][redesign]]: Now this
;;just stores a note 
;;But there should also be a scoped form that stores a pop note when
;;it's done.  That can be dormant.


;;;_. Footers
;;;_ , Provides

(provide 'testhelp/standard)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; testhelp/standard.el ends here

