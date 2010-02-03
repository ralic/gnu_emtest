;;;_ tp.el --- Test-points from other code

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

;;Instead of a bare `require', put this before code that uses this.
'
(eval-and-compile
   (when (not (require 'tp nil t))
      (defmacro emt:testpoint (id args &rest rest)
	 `(progn ,@rest))))

;;;_ , Requires

(require 'emt-accumulator)
(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

;;;_. Body

;;;_ , Globals
(defconst emt:testpoint:enable nil 
   "The global value is always `nil'." )

;;;_ , Non-global special variables
;;Defined just to quiet the byte-compiler
(defvar emt:testpoint:handlers nil "" )

;;;_ , Test examples

(defun emt:testpoint:td:1 (branch &optional arg)
   ""
   
   (if (equal branch 0)
      (emt:testpoint id-0 (arg 'b-0)
	 'branch-0)
      (emt:testpoint id-1 (arg 'b-1)
	 'branch-1)))

;;;_ , emt:testpoint
;;;###autoload
(defmacro emt:testpoint (id args &rest body)
   "A testpoint.
When not enabled, this simply runs BODY as if in a progn.
When enabled and there is a handler that matches ID, calls that
handler with arglist ARGS.
That handler can either fall thru to BODY or throw a return value to
`emt:testpoint:tag-return'."
   `(catch 'emt:testpoint:tag-return
       (progn 
	  (when emt:testpoint:enable
	     ;;If `emt:testpoint:handle-call' uses a handler, it throws and
	     ;;does not return. 
	     (emt:testpoint:handle-call
		emt:testpoint:handlers ',id (list ,@args)))
	  ,@body)))

;;;_  . Tests
(rtest:deftest emt:testpoint


   (  "Situation: Called normally, not in `emt:testpoint:eval'.
Response: The function behaves as if its testpoints were simply the
forms in them."
      (and
	 (equal
	    (emt:testpoint:td:1 0)
	    'branch-0)
	 (equal
	    (emt:testpoint:td:1 1)
	    'branch-1)))
   
   
   (  "Situation: Called nested inside something else and not in
`emt:testpoint:eval'. 
Response: The function behaves as if its testpoints were simply the
forms in them."
	 (equal
	    (funcall 
	       #'(lambda ()
		    (emt:testpoint:td:1 0)))
	    'branch-0))
   
   (  "Situation: Called in emt:testpoint:eval
The chosen branch's id matches a test clause's id
Response: It behaves as if its testpoints called the test forms"
      (equal
	 (emt:testpoint:eval
	    (emt:testpoint:td:1 0)
	    (tp id-0 (arg a) 'test-ret-0))
	 'test-ret-0))
   

   (  "Situation: Called in emt:testpoint:eval
The chosen branch's id does not match any test clause's id
Response: It behaves as if its testpoints called the test forms"
      (equal
	 (emt:testpoint:eval
	    (emt:testpoint:td:1 0)
	    (tp* 
	       (:id unknown-id :count nil) (arg a) 'test-ret-0))
	 'branch-0))


   (  "Situation: Test-function is called so that two branches are
visited.
Each clause's id matches one.
Response: Both branches are treated as their respective test clauses."
      (equal
	 (emt:testpoint:eval
	    (list
	       (emt:testpoint:td:1 0)
	       (emt:testpoint:td:1 1))
	    
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
	 (emt:testpoint:eval
	    (emt:testpoint:td:1 1 'foo)
	    (tp id-1 (arg a) arg))
	 'foo))

   (  "Situation: The testpoint falls thru on certain conditions -
ie just if the arg is 2
Response: Return value is as expected."
      (equal
	 (emt:testpoint:eval
	    (list
	       (emt:testpoint:td:1 1 1)
	       (emt:testpoint:td:1 1 2))
	    (tp* (:id id-1 :fallthru t :count 2) (arg a) 
	       (if (equal arg 1) 
		  ;;No throw, so fall thru to form.
		  'ignored 
		  ;;This throw makes `test-ret-1' the return value.
		  (throw 'emt:testpoint:tag-return 'test-ret-1))))
	 '(branch-1 test-ret-1)))

   (  "Shows: Miscount error is withheld if other errors occur.
Situation: Foo is mocked
During execution, an earlier error keeps foo from ever being called.
Response: Error is not an emt:testpoint:miscount error, it is the
earlier error." 

      (condition-case err
	 (emt:testpoint:eval
	    (progn
	       (signal 
		  'emt:example-error 
		  "This error keeps the testpoint from being run")
	       ;;NOT REACHED
	       (emt:testpoint:td:1 1 1))
	    (tp* (:id id-1 :count 1) (arg a)))
	 ('emt:testpoint:miscount nil)
	 ('emt:example-error t)))

   (  "Shows: Miscount error is not withheld if a throw occurs.
Situation: Foo is mocked.
There is a catch outside `with-mock'.
During execution, a throw keeps foo from ever being called.
Response: Error is an emt:testpoint:miscount error."
      (condition-case err
	 (catch 'example-tag
	    (emt:testpoint:eval
	       (progn
		  (throw 'example-tag nil)
		  ;;NOT REACHED
		  (emt:testpoint:td:1 1 1))
	       (tp* (:id id-1 :count 1) (arg a))))
	 ('emt:testpoint:miscount t)
	 (error nil)))


   ;;tp* can have bindings

   (  "Param: Keyword :bindings, syntax like `let'
Response: Inside the testpoint, values are bound as if by `let'."
      (emt:testpoint:eval
	 (list
	    (emt:testpoint:td:1 1))
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
Response: `emt:testpoint:eval's value is the main form's value."
      (equal
	 (emt:testpoint:eval
	    12
	    (finally () ()
	       13))
	 12))


   (  "Situation: There is only a finally form.
No args. 
No bindings.
Param: `:return' flag is passed non-nil
Response: `emt:testpoint:eval's value is the finally clause's value."
      (equal
	 (emt:testpoint:eval
	    nil
	    (finally (:return t) ()
	       (+ 2 10)))
	 12))

   (  "Situation: There are two finally forms.
Response: Error."
      (progn
	 (assert
	    (emt:gives-error
	       (emt:testpoint:eval
		  nil
		  (finally () ()
		     (+ 2 10))
		  (finally () ()
		     (+ 2 10)))))
	 t))

   (  "Situation: There is only a finally form.
No args. 
Binding `twelve' to a form that evals to 12.
Response: Normal - its return value is `emt:testpoint:eval's value."
      (equal
	 (emt:testpoint:eval
	    nil
	    (finally (:return t :bindings ((twelve (+ 2 10)))) ()
	       twelve))
	 12))

   (  "Situation: There is a finally clause and a test point.
The testpoint collects one arg.
Response: The list of collected args is passed to the finally clause."
      (progn
	 (emt:testpoint:eval
	    (list
	       (emt:testpoint:td:1 1 'x)
	       (emt:testpoint:td:1 1 'y))
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
	 (emt:testpoint:eval
	    (list
	       (emt:testpoint:td:1 1 'x)
	       (emt:testpoint:td:1 1 'y))
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
	 (emt:testpoint:eval
	    (list
	       (emt:testpoint:td:1 1 'x)
	       (emt:testpoint:td:1 1 'y))
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
		     (emt:testpoint:eval
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
		  (emt:testpoint:eval
		     (list
			(foo))
		     (mock* (:symbol foo :count 2)))))
	    (assert
	       (eq old-foo-func (symbol-function 'foo))))
	 t))
   
   )

;;;_ , emt:testpoint:handle-call
(defun emt:testpoint:handle-call (handlers id args)
   ""
   (let
      ((cell (assoc id handlers)))
      (if cell
	 (let
	    ((func (second cell)))
	    (apply func args)))))

;;;_  . Tests

(rtest:deftest emt:testpoint:handle-call
   (  "Situation: Passed an id that matches, 
Response: Call that form with args."
      (equal
	 (emt:testpoint:eval
	    ;;Here the form calls `emt:testpoint:handle-call'
	    ;;directly, in order to see its behavior without filtering
	    ;;thru `emt:testpoint'.
	    (catch 'emt:testpoint:tag-return
	       (emt:testpoint:handle-call
		  emt:testpoint:handlers
		  'id-0 
		  '(12)))
	    ;;The handler clause
	    (tp id-0 (a) a))
	 12))

   )

;;;_ , Error types
;;;_  . emt:testpoint:error
;;The more general one.
(put 'emt:testpoint:error 'error-conditions
   '(error emt:error emt:testpoint:error))
(put 'emt:testpoint:miscount 'error-message
   "Emt:testpoint detected something wrong.")

;;;_  . emt:testpoint:miscount
(put 'emt:testpoint:miscount 'error-conditions
   '(error emt:error emt:testpoint:error emt:testpoint:miscount))
(put 'emt:testpoint:miscount 'error-message
   "Emt:testpoint detected that something was not called exactly as
many times as expected")



;;;_ , Make clause forms
;;;_  . Accumulator type
'  ;;Old version
(defstruct (emt:testpoint:clause-formdata 
	      (:type list)
   	      (:copier nil)
	      (:conc-name emt:testpoint:clause-formdata->)
	      (:constructor emt:testpoint:make-clause-formdata)
	      (:constructor emt:testpoint:make-clause-formdata-2
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
   (emt:testpoint:clause-formdata 
      (:type list)
      (:copier nil)
      (:conc-name emt:testpoint:clause-formdata->)
      (:constructor emt:testpoint:make-clause-formdata))
   
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
;;;_   , emt:testpoint:make-clause-form-x
(defun emt:testpoint:make-clause-form-x 
   (id args body &optional fallthru count bindings mock)
   ""
   (let
      ((counter
	  (when count (emt:testpoint:make-counter count id))))

      (emt:testpoint:make-clause-formdata
	 ;;Choose which field we put this into by choosing keyword.
	 (if mock :mock-func-SINGLE :handler-SINGLE)
	 (list  
	    id
	    (eval
	       `#'(lambda (,@args)
		     (let ,bindings
			,(when counter 
			    (emt:testpoint:counter:incf-form counter))
			,(if fallthru
			    `(progn ,@body)
			    `(throw 'emt:testpoint:tag-return
				(progn ,@body)))))))
	       
	 :reached-counter-LIST
	 (if counter (list counter) ()))))


;;;_  . Handle specific governors

;;;_   , emt:testpoint:make-clause-form-tp*
(defun* emt:testpoint:make-clause-form-tp*
   ((&key id fallthru (count 1) bindings) args &rest body)
   ""
   (emt:testpoint:make-clause-form-x id args body fallthru count bindings))

;;;_   , emt:testpoint:make-clause-form-tp
(defun emt:testpoint:make-clause-form-tp (id args &rest body)
   ""
   (emt:testpoint:make-clause-form-x id args body nil 1))

;;;_   , emt:testpoint:make-clause-form-tp-reached
(defun emt:testpoint:make-clause-form-tp-reached (id count)
   ""
   (emt:testpoint:make-clause-form-x id '(&rest dummy) nil t count))
;;;_   , emt:testpoint:make-clause-form-mock*
(defun* emt:testpoint:make-clause-form-mock* 
   ((&key symbol (count 1)) &optional args-dummy &rest body)
   ""
   (emt:testpoint:make-clause-form-x 
       symbol '(&rest rest) body t count () t))

;;;_   , emt:testpoint:make-clause-form-finally
(defun* emt:testpoint:make-clause-form-finally 
   ((&key bindings return) arglist &rest form)
   ""
   (emt:testpoint:make-clause-formdata
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
;;;_   , emt:testpoint:make-clause-form
(defun emt:testpoint:make-clause-form (governor &rest r)
   ""

   (case governor

      (tp*
	 (apply #'emt:testpoint:make-clause-form-tp* r))

      (tp
	 (apply #'emt:testpoint:make-clause-form-tp r))
      (tp-reached
	 (apply #'emt:testpoint:make-clause-form-tp-reached r))
      (mock*
	 (apply #'emt:testpoint:make-clause-form-mock* r))
      (finally
	 (apply #'emt:testpoint:make-clause-form-finally r))
      (t
	 (error "No such testpoint clause governor: %s" governor))))



;;;_  . emt:testpoint:get-combined-clause-forms
(defun emt:testpoint:get-combined-clause-forms (testpoint-forms)
   ""

   (emt:accumulator:collect 
      #'emt:testpoint:make-clause-form
      testpoint-forms
      'emt:testpoint:clause-formdata))


;;;_ , Counters
;;;_  . Structure (Implied)
;;;_  . Ctor emt:testpoint:make-counter
(defun emt:testpoint:make-counter (reached id)
   ""
   (list (gensym) reached id))

;;;_  . To increment form emt:testpoint:counter:incf-form
(defun emt:testpoint:counter:incf-form (counter)
   ""
   `(incf ,(car counter)))

;;;_  . To binding-form emt:testpoint:counter:binding-form
(defun emt:testpoint:counter:binding-form (counter)
   ""
   `(,(car counter) 0))

;;;_  . To check form emt:testpoint:counter:check-form
(defun emt:testpoint:counter:check-form (counter)
   ""
   `(unless (equal ,(car counter) ,(cadr counter))
       (signal 'emt:testpoint:miscount
	  (list
	     (format
		"Testpoint %s was reached %d times (expected %d)"
		',(caddr counter)
		,(car counter)
		,(cadr counter))))))



;;;_  . Tests of counter forms' behavior together.
(rtest:deftest emt:testpoint:counter

   (  "Situation: A counter to be visited once.
It is visited once
Response: No error."
      (let 
	 ;;A counter to be visited once
	 ((counter (emt:testpoint:make-counter 1 'id-0)))
	 (eval
	    `(let 
		;;Bind counter
		(,(emt:testpoint:counter:binding-form counter))
		;;Incr it once
		,(emt:testpoint:counter:incf-form counter)
		;;Check that it's OK
		,(emt:testpoint:counter:check-form counter)
		t))))
      
   (  "Situation: A counter to be visited once.
It is visited twice.
Response: The check signals an error."
      (let 
	 ;;A counter to be visited once
	 ((counter (emt:testpoint:make-counter 1 'id-0)))
	 (eval
	    `(let 
		;;Bind counter
		(,(emt:testpoint:counter:binding-form counter))
		;;Incr it twice
		,(emt:testpoint:counter:incf-form counter)
		,(emt:testpoint:counter:incf-form counter)
		;;Get an error
		(emt:gives-error
		   ,(emt:testpoint:counter:check-form counter))
		t))))
   
   (  "Situation: Inside condition-case.
The check form is evalled under conditions of certain error.
Response: The error is caught by condition-case as expected."
      (let 
	 ((counter (emt:testpoint:make-counter 1 'id-0)))
	 (condition-case err
	    (eval
	       `(let
		  (,(emt:testpoint:counter:binding-form counter))
		   ,(emt:testpoint:counter:check-form counter)))
	    ('emt:testpoint:miscount t)
	    (error nil)))
      
      )

   )
;;;_ , emt:testpoint:eval
;;;###autoload
(defmacro emt:testpoint:eval (form &rest testpoint-forms)
   ""
   (let*
      ((all-formdata
	  (emt:testpoint:get-combined-clause-forms testpoint-forms))
	 (counter-data
	    (emt:testpoint:clause-formdata->reached-counter-LIST
	       all-formdata))
	 (finally-data-list
	    (emt:testpoint:clause-formdata->finally-data-LIST
	       all-formdata))
	 (old-funcs-sym (gensym "emt:testpoint"))
	 (seen-err (gensym "emt:testpoint")))

      ;;Sanity checks
      (when (> (length finally-data-list) 1)
	 (error "Only one final form is allowed"))

      ;;The form we make.
      `(let (  (emt:testpoint:enable t)
	       (emt:testpoint:collected ())
	       (,seen-err nil)
	       (emt:testpoint:handlers
		  ',(emt:testpoint:clause-formdata->handler-LIST
		       all-formdata))
	       
	       ;;Rebind functions.
	       (,old-funcs-sym 
		  (emt:testpoint:bind-funcs
		     ',(emt:testpoint:clause-formdata->mock-func-LIST
			  all-formdata)))
	       
	       ;;Bind counters to initial values
	       ,@(mapcar
		    #'emt:testpoint:counter:binding-form
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
				     emt:testpoint:collected
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

	     (emt:testpoint:bind-funcs ,old-funcs-sym)
	     (unless ,seen-err
		,@(mapcar
		     #'emt:testpoint:counter:check-form
		     counter-data))))))

;;;_  . Helper emt:testpoint:bind-funcs
(defun emt:testpoint:bind-funcs (data-list)
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
(put 'emt:testpoint:eval 'rtest:test-thru
   'emt:testpoint)
;;;_ , emt:tp:collect
;;;###autoload
(defun emt:tp:collect (&rest args)
   "Only meaningful inside `emt:testpoint:eval'"
   (callf append emt:testpoint:collected (list (mapcar #'list args))))

;;;_  . Tests
;;It's direct

;;;_. Footers
;;;_ , Provides

(provide 'tp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tp.el ends here
