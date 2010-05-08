;;;_ emtest/testhelp/testpoint.el --- Creating transparent test-points in code

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
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

;; 

;;Instead writing a normal `require', use command
;;`emt:insert:require-tp' where you would normally write (require
;;'feature).  It will insert this special code
'
(eval-when-compile
   (require 'emtest/testhelp/testpoint/requirer)
   (emtp:require))

;;That means that if testpoint is not available, there is no error and
;;a testpoint will just eval the body forms.

;;;_ , Requires

(require 'utility/accumulator)

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(require 'rtest-util)  
(require 'rtest-define)

;;;_. Body

;;;_ , Globals
(defconst emtp:enable nil 
   "The global value is always `nil'." )

;;;_ , Non-global special variables
;;Might be made a global again if we want nested testpoints to be
;;progressive. 
'(defvar emtp:*handlers* nil "" )
;;;_ , Declarations
(declare (special emtp:*collected* emtp:*handlers*))

;;;_ , Test examples

(defun emtp:td:1 (branch &optional arg)
   ""
   
   (if (equal branch 0)
      (emtp id-0 (arg 'b-0)
	 'branch-0)
      (emtp id-1 (arg 'b-1)
	 'branch-1)))

;;;_ , emtp
;;;###autoload
(defmacro emtp (id args &rest body)
   "A testpoint.
When not enabled, this simply runs BODY as if in a progn.
When enabled and there is a handler that matches ID, calls that
handler with arglist ARGS.
That handler can either fall thru to BODY or throw a return value to
`emtp:tag-return'."
   (declare 
      (debug (symbolp (&rest form) body)))
   `(catch 'emtp:tag-return
       (progn 
	  (declare (special emtp:*handlers*))
	  (when emtp:enable
	     ;;If `emtp:handle-call' uses a handler, it throws and
	     ;;does not return. 
	     (emtp:handle-call
		emtp:*handlers* ',id (list ,@args)))
	  ,@body)))

;;;_  . Tests
(rtest:deftest emtp


   (  "Situation: Called normally, not in `emtp:eval'.
Response: The function behaves as if its testpoints were simply the
forms in them."
      (and
	 (equal
	    (emtp:td:1 0)
	    'branch-0)
	 (equal
	    (emtp:td:1 1)
	    'branch-1)))
   
   
   (  "Situation: Called nested inside something else and not in
`emtp:eval'. 
Response: The function behaves as if its testpoints were simply the
forms in them."
	 (equal
	    (funcall 
	       #'(lambda ()
		    (emtp:td:1 0)))
	    'branch-0))
   
   (  "Situation: Called in emtp:eval
The chosen branch's id matches a test clause's id
Response: It behaves as if its testpoints called the test forms"
      (equal
	 (emtp:eval
	    (emtp:td:1 0)
	    (tp id-0 (arg a) 'test-ret-0))
	 'test-ret-0))
   

   (  "Situation: Called in emtp:eval
The chosen branch's id does not match any test clause's id
Response: It behaves as if its testpoints called the test forms"
      (equal
	 (emtp:eval
	    (emtp:td:1 0)
	    (tp* 
	       (:id unknown-id :count nil) (arg a) 'test-ret-0))
	 'branch-0))


   (  "Situation: Test-function is called so that two branches are
visited.
Each clause's id matches one.
Response: Both branches are treated as their respective test clauses."
      (equal
	 (emtp:eval
	    (list
	       (emtp:td:1 0)
	       (emtp:td:1 1))
	    
	    (tp id-0 (arg a) 'test-ret-0)
	    (tp id-1 (arg a) 'test-ret-1))
	 '(test-ret-0 test-ret-1)))


   (  "Situation: The test-function passes `b-0' as an arg to the
first branch.
The test form returns the arg it gets
Response: The test form returns `b-0'
Proves: The test form sees the args that the main function passes to
the testpoint."
      (equal
	 (emtp:eval
	    (emtp:td:1 1 'foo)
	    (tp id-1 (arg a) arg))
	 'foo))

   (  "Situation: The testpoint falls thru on certain conditions -
ie just if the arg is 2
Response: Return value is as expected."
      (equal
	 (emtp:eval
	    (list
	       (emtp:td:1 1 1)
	       (emtp:td:1 1 2))
	    (tp* (:id id-1 :fallthru t :count 2) (arg a) 
	       (if (equal arg 1) 
		  ;;No throw, so fall thru to form.
		  'ignored 
		  ;;This throw makes `test-ret-1' the return value.
		  (throw 'emtp:tag-return 'test-ret-1))))
	 '(branch-1 test-ret-1)))

   (  "Shows: Miscount error is withheld if other errors occur.
Situation: Foo is mocked
During execution, an earlier error keeps foo from ever being called.
Response: Error is not an emtp:miscount error, it is the
earlier error." 

      (condition-case err
	 (emtp:eval
	    (progn
	       (signal 
		  'emt:example-error 
		  "This error keeps the testpoint from being run")
	       ;;NOT REACHED
	       (emtp:td:1 1 1))
	    (tp* (:id id-1 :count 1) (arg a)))
	 ('emtp:miscount nil)
	 ('emt:example-error t)))

   (  "Shows: Miscount error is not withheld if a throw occurs.
Situation: Foo is mocked.
There is a catch outside `with-mock'.
During execution, a throw keeps foo from ever being called.
Response: Error is an emtp:miscount error."
      (condition-case err
	 (catch 'example-tag
	    (emtp:eval
	       (progn
		  (throw 'example-tag nil)
		  ;;NOT REACHED
		  (emtp:td:1 1 1))
	       (tp* (:id id-1 :count 1) (arg a))))
	 ('emtp:miscount t)
	 (error nil)))


   ;;tp* can have bindings

   (  "Param: Keyword :bindings, syntax like `let'
Response: Inside the testpoint, values are bound as if by `let'."
      (emtp:eval
	 (list
	    (emtp:td:1 1))
	 (tp* (:id id-1 
		 :count 1 
		 :bindings ((twelve (+ 2 10)))) 
	    (arg a)
	    (assert
	       (equal twelve 12)
	       t)))
      )


   ;;The "finally" form
   (  "Situation: There is only a finally form.
No args. 
No bindings.
Param: `:return' flag is NOT passed.
Response: `emtp:eval's value is the main form's value."
      (equal
	 (emtp:eval
	    12
	    (finally () ()
	       13))
	 12))


   (  "Situation: There is only a finally form.
No args. 
No bindings.
Param: `:return' flag is passed non-nil
Response: `emtp:eval's value is the finally clause's value."
      (equal
	 (emtp:eval
	    nil
	    (finally (:return t) ()
	       (+ 2 10)))
	 12))

   (  "Situation: There are two finally forms.
Response: Error."
      (progn
	 (assert
	    (emt:gives-error
	       (emtp:eval
		  nil
		  (finally () ()
		     (+ 2 10))
		  (finally () ()
		     (+ 2 10)))))
	 t))

   (  "Situation: There is only a finally form.
No args. 
Binding `twelve' to a form that evals to 12.
Response: Normal - its return value is `emtp:eval's value."
      (equal
	 (emtp:eval
	    nil
	    (finally (:return t :bindings ((twelve (+ 2 10)))) ()
	       twelve))
	 12))

   (  "Situation: There is a finally clause and a test point.
The testpoint collects one arg.
Response: The list of collected args is passed to the finally clause."
      (progn
	 (emtp:eval
	    (list
	       (emtp:td:1 1 'x)
	       (emtp:td:1 1 'y))
	    (tp* (:id id-1 :count 2) (arg a) 
	       (emt:tp:collect arg))
	    (finally () (arg-list) 
	       (assert
		  (rtest:sets= arg-list
		     '(x y))
		  t)))
	 t))


   (  "Situation: There is a finally clause and a test point.
The testpoint collects multiple args - 2.
Response: The collected lists are passed to the finally clause."
      (progn
	 (emtp:eval
	    (list
	       (emtp:td:1 1 'x)
	       (emtp:td:1 1 'y))
	    (tp* (:id id-1 :count nil) (arg a) 
	       (emt:tp:collect arg a))
	    (finally () (arg-list a-list)
	       (assert
		  (rtest:sets= arg-list
		     '(x y))
		  t)

	       (assert
		  (rtest:sets= a-list
		     '(b-1 b-1))
		  t)))
	 t))

   
   (  "Shows: Results of clauses are passed to `finally' in the same
order that they occur.
Situation:  Two calls, each collecting a different value.
Response: Elements occur in the order the calls are made."
      (progn
	 (emtp:eval
	    (list
	       (emtp:td:1 1 'x)
	       (emtp:td:1 1 'y))
	    (tp* (:id id-1 :count nil) (arg a) 
	       (emt:tp:collect arg))
	    (finally () (arg-list)
	       (assert
		  (equal arg-list
		     '(x y))
		  t)))
	 t))
   



   ;;Supports mock

   (  "Situation: Foo is called the expected number of times.  
Response: No error.
The old version of foo is restored"
      (flet
	 ((foo (&rest r)))
	 (let
	    ((old-foo-func (symbol-function 'foo)))
	    (assert
	       (not
		  (emt:gives-error
		     (emtp:eval
			(list
			   (foo)
			   (foo))
			(mock* (:symbol foo :count 2))))))
	    (assert
	       (eq old-foo-func (symbol-function 'foo))))
	 t))
   
   (  "Situation: Foo is called the wrong number of times.  
Response: Error.
The old version of foo is restored"
      (flet
	 ((foo (&rest r)))
	 (let
	    ((old-foo-func (symbol-function 'foo)))
	    (assert
	       (emt:gives-error
		  (emtp:eval
		     (list
			(foo))
		     (mock* (:symbol foo :count 2)))))
	    (assert
	       (eq old-foo-func (symbol-function 'foo))))
	 t))
   
   )

;;;_ , emtp:handle-call
(defun emtp:handle-call (handlers id args)
   ""
   (let
      ((cell (assoc id handlers)))
      (if cell
	 (let
	    ((func (second cell)))
	    (apply func args)))))

;;;_  . Tests

(rtest:deftest emtp:handle-call
   (  "Situation: Passed an id that matches, 
Response: Call that form with args."
      (equal
	 (emtp:eval
	    ;;Here the form calls `emtp:handle-call'
	    ;;directly, in order to see its behavior without filtering
	    ;;thru `emtp'.
	    (catch 'emtp:tag-return
	       (emtp:handle-call
		  emtp:*handlers*
		  'id-0 
		  '(12)))
	    ;;The handler clause
	    (tp id-0 (a) a))
	 12))

   )

;;;_ , Error types
;;;_  . emtp:error
;;The more general one.
(put 'emtp:error 'error-conditions
   '(error emt:error emtp:error))
(put 'emtp:miscount 'error-message
   "emtp detected something wrong.")

;;;_  . emtp:miscount
(put 'emtp:miscount 'error-conditions
   '(error emt:error emtp:error emtp:miscount))
(put 'emtp:miscount 'error-message
   "emtp detected that something was not called exactly as
many times as expected")



;;;_ , Make clause forms
;;;_  . Accumulator type
'  ;;Old version
(defstruct (emtp:clause-formdata 
	      (:type list)
   	      (:copier nil)
	      (:conc-name emtp:clause-formdata->)
	      (:constructor emtp:make-clause-formdata)
	      (:constructor emtp:make-clause-formdata-2
		 (&key 
		    (handler-SINGLE () handler-SINGLE-seen) 
		    (handler-LIST
		       (if handler-SINGLE-seen
			  (list handler-SINGLE)))
		    (mock-func-SINGLE () mock-func-SINGLE-seen) 
		    (mock-func-LIST
		       (if mock-func-SINGLE-seen
			  (list mock-func-SINGLE)))
		    (reached-counter-SINGLE () reached-counter-seen)
		    (reached-counter-LIST 
		       (if reached-counter-seen 
			  (list reached-counter-SINGLE))))))
   
   "The type that a clause-maker returns"
   handler-LIST ;;Elements as (symbol lambda)
   mock-func-LIST
   (reached-counter-LIST () :type (repeat (list symbol integer)))
   final-form  ;;A lambda
   ;;Assumes we collect into a list.  For now we always do.
   collector-num-slots
   return-finally
   finally-data
   )


(emt:accumulator:define
   (emtp:clause-formdata 
      (:type list)
      (:copier nil)
      (:conc-name emtp:clause-formdata->)
      (:constructor emtp:make-clause-formdata))
   
   "The type that a clause-maker returns"
   ;;Elements as (symbol lambda-form)
   handler 
   mock-func
   (reached-counter () :type (list symbol integer))
   final-form ;;A lambda
   ;;Assumes we collect into a list.  For now we always do.
   collector-num-slot
   return-finally
   finally-data)


;;;_  . Worker
;;;_   , emtp:make-clause-form-x
(defun emtp:make-clause-form-x 
   (id args body &optional fallthru count bindings mock)
   ""
   (let
      ((counter
	  (when count (emtp:make-counter count id))))

      (emtp:make-clause-formdata
	 ;;Choose which field we put this into by choosing keyword.
	 (if mock :mock-func-SINGLE :handler-SINGLE)
	 (list  
	    id
	    (eval
	       `#'(lambda (,@args)
		     (let ,bindings
			,(when counter 
			    (emtp:counter:incf-form counter))
			,(if fallthru
			    `(progn ,@body)
			    `(throw 'emtp:tag-return
				(progn ,@body)))))))
	       
	 :reached-counter-LIST
	 (if counter (list counter) ()))))


;;;_  . Handle specific governors

;;;_   , emtp:make-clause-form-tp*
(defun* emtp:make-clause-form-tp*
   ((&key id fallthru (count 1) bindings) args &rest body)
   ""
   (emtp:make-clause-form-x id args body fallthru count bindings))

;;;_   , emtp:make-clause-form-tp
(defun emtp:make-clause-form-tp (id args &rest body)
   ""
   (emtp:make-clause-form-x id args body nil 1))

;;;_   , emtp:make-clause-form-tp-reached
(defun emtp:make-clause-form-tp-reached (id count)
   ""
   (emtp:make-clause-form-x id '(&rest dummy) nil t count))
;;;_   , emtp:make-clause-form-mock*
(defun* emtp:make-clause-form-mock* 
   ((&key symbol (count 1)) &optional args-dummy &rest body)
   ""
   (emtp:make-clause-form-x 
       symbol '(&rest rest) body t count () t))

;;;_   , emtp:make-clause-form-finally
(defun* emtp:make-clause-form-finally 
   ((&key bindings return) arglist &rest form)
   ""
   (emtp:make-clause-formdata
      :finally-data-SINGLE 
      (list
	 ;;Form
	 (eval
	    `#'(lambda (,@arglist) (let ,bindings ,@form)))
	 ;;number of slots
	 (length arglist)
	 ;;Whether to return the `finally' value
	 return)
      :return-finally-SINGLE 
      return
      :final-form-SINGLE
      (eval
	 `#'(lambda (,@arglist) (let ,bindings ,@form)))
      :collector-num-slot-SINGLE  
      (length arglist)))


;;;_  . Dispatch clause form maker
;;;_   , emtp:make-clause-form
(defun emtp:make-clause-form (governor &rest r)
   ""

   (case governor

      (tp*
	 (apply #'emtp:make-clause-form-tp* r))

      (tp
	 (apply #'emtp:make-clause-form-tp r))
      (tp-reached
	 (apply #'emtp:make-clause-form-tp-reached r))
      (mock*
	 (apply #'emtp:make-clause-form-mock* r))
      (finally
	 (apply #'emtp:make-clause-form-finally r))
      (t
	 (error "No such testpoint clause governor: %s" governor))))



;;;_  . emtp:get-combined-clause-forms
(defun emtp:get-combined-clause-forms (testpoint-forms)
   ""

   (emt:accumulator:collect 
      #'emtp:make-clause-form
      testpoint-forms
      'emtp:clause-formdata))


;;;_ , Counters
;;;_  . Structure (Implied)
;;;_  . Ctor emtp:make-counter
(defun emtp:make-counter (reached id)
   ""
   (list (gensym) reached id))

;;;_  . To increment form emtp:counter:incf-form
(defun emtp:counter:incf-form (counter)
   ""
   `(incf ,(car counter)))

;;;_  . To binding-form emtp:counter:binding-form
(defun emtp:counter:binding-form (counter)
   ""
   `(,(car counter) 0))

;;;_  . To check form emtp:counter:check-form
(defun emtp:counter:check-form (counter)
   ""
   `(unless (equal ,(car counter) ,(cadr counter))
       (signal 'emtp:miscount
	  (list
	     (format
		"Testpoint %s was reached %d times (expected %d)"
		',(caddr counter)
		,(car counter)
		,(cadr counter))))))



;;;_  . Tests of counter forms' behavior together.
(rtest:deftest emtp:counter

   (  "Situation: A counter to be visited once.
It is visited once
Response: No error."
      (let 
	 ;;A counter to be visited once
	 ((counter (emtp:make-counter 1 'id-0)))
	 (eval
	    `(let 
		;;Bind counter
		(,(emtp:counter:binding-form counter))
		;;Incr it once
		,(emtp:counter:incf-form counter)
		;;Check that it's OK
		,(emtp:counter:check-form counter)
		t))))
      
   (  "Situation: A counter to be visited once.
It is visited twice.
Response: The check signals an error."
      (let 
	 ;;A counter to be visited once
	 ((counter (emtp:make-counter 1 'id-0)))
	 (eval
	    `(let 
		;;Bind counter
		(,(emtp:counter:binding-form counter))
		;;Incr it twice
		,(emtp:counter:incf-form counter)
		,(emtp:counter:incf-form counter)
		;;Get an error
		(emt:gives-error
		   ,(emtp:counter:check-form counter))
		t))))
   
   (  "Situation: Inside condition-case.
The check form is evalled under conditions of certain error.
Response: The error is caught by condition-case as expected."
      (let 
	 ((counter (emtp:make-counter 1 'id-0)))
	 (condition-case err
	    (eval
	       `(let
		  (,(emtp:counter:binding-form counter))
		   ,(emtp:counter:check-form counter)))
	    ('emtp:miscount t)
	    (error nil)))
      
      )

   )
;;;_ , emtp:eval
;;;###autoload
(defmacro emtp:eval (form &rest testpoint-forms)
   ""
   (let*
      ((all-formdata
	  (emtp:get-combined-clause-forms testpoint-forms))
	 (counter-data
	    (emtp:clause-formdata->reached-counter-LIST
	       all-formdata))
	 (finally-data-list
	    (emtp:clause-formdata->finally-data-LIST
	       all-formdata))
	 (old-funcs-sym (gensym "emtp"))
	 (seen-err (gensym "emtp")))

      ;;Sanity checks
      (when (> (length finally-data-list) 1)
	 (error "Only one final form is allowed"))

      ;;The form we make.
      `(let (  (emtp:enable t)
	       (emtp:*collected* ())
	       (,seen-err nil)
	       (emtp:*handlers*
		  ',(emtp:clause-formdata->handler-LIST
		       all-formdata))
	       
	       ;;Rebind functions.
	       (,old-funcs-sym 
		  (emtp:bind-funcs
		     ',(emtp:clause-formdata->mock-func-LIST
			  all-formdata)))
	       
	       ;;Bind counters to initial values
	       ,@(mapcar
		    #'emtp:counter:binding-form
		    counter-data))

	  (unwind-protect
	     (condition-case err
		,(if finally-data-list
		    (destructuring-bind 
		       (final-form collector-num-slots return-finally)
		       (car finally-data-list)
		       (check-type collector-num-slots integer)
		       (let
			  ((finally-form
			      `(apply 
				  ,final-form
				  (emt:accumulator:transpose 
				     emtp:*collected*
				     ,collector-num-slots
				     #'nth))))
			  (if return-finally 
			     `(progn
				 ,form
				 ,finally-form)
			     `(prog1
				 ,form
				 ,finally-form))))
		    form)
		(error
		   (setq ,seen-err t)
		   ;;Re-raise the signal
		   (signal (car err)(cdr err))))

	     (emtp:bind-funcs ,old-funcs-sym)
	     (unless ,seen-err
		,@(mapcar
		     #'emtp:counter:check-form
		     counter-data))))))

;;;_  . Helper emtp:bind-funcs
(defun emtp:bind-funcs (data-list)
   "Rebind the functions in DATA-LIST.
Each element of DATA-LIST has the form (SYMBOL DEFINITION).
Return a list of the old bindings in that form."

   (mapcar
      #'(lambda (datum)
	   (destructuring-bind (symbol definition) datum
	      (prog1
		 (list 
		    symbol 
		    (if (fboundp symbol)
		       (symbol-function symbol)
		       nil))
		 (when definition
		    (ad-safe-fset symbol definition)))))
      data-list))



;;;_  . Tests
(put 'emtp:eval 'rtest:test-thru
   'emtp)
;;;_ , emt:tp:collect
;;;###autoload
(defun emt:tp:collect (&rest args)
   "Only meaningful inside `emtp:eval'"
   (callf append emtp:*collected* (list (mapcar #'list args))))

;;;_  . Tests
;;It's direct

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/testpoint)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/testpoint.el ends here
