;;;_ emtest/testhelp/testpoint/rtest.el --- Rtest tests for testpoint

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

(require 'rtest-util)  
(require 'rtest-define)

;;;_. Body

;;;_  . emtp
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
		  'emth:example-error 
		  "This error keeps the testpoint from being run")
	       ;;NOT REACHED
	       (emtp:td:1 1 1))
	    (tp* (:id id-1 :count 1) (arg a)))
	 ('emtp:miscount nil)
	 ('emth:example-error t)))

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
	    (emth:gives-error
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
		  (emth:gives-error
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
	       (emth:gives-error
		  (emtp:eval
		     (list
			(foo))
		     (mock* (:symbol foo :count 2)))))
	    (assert
	       (eq old-foo-func (symbol-function 'foo))))
	 t))
   
   )

;;;_  . emtp:handle-call

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

;;;_  . emtp:counter
;;Tests of counter forms' behavior together.
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
		(emth:gives-error
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
;;;_  . emtp:eval
(put 'emtp:eval 'rtest:test-thru
   'emtp)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/testpoint/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/testpoint/rtest.el ends here
