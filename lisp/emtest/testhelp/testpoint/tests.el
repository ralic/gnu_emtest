;;;_ emtest/testhelp/testpoint/tests.el --- Tests for testpoint

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



;;;_. Body

;;;_  . emtp
(emt:deftest-3 emtp
   (nil
      (progn
	 (emt:doc "Situation: Called normally, not in `emtp:eval'.")
	 (emt:doc "Response: The function behaves as if its testpoints were simply the
forms in them.")
	 (and
	    (equal
	       (emtp:td:1 0)
	       'branch-0)
	    (equal
	       (emtp:td:1 1)
	       'branch-1))))
   (nil
      (progn
	 (emt:doc "Situation: Called nested inside something else and not in
`emtp:eval'.")
	 (emt:doc "Response: The function behaves as if its testpoints were simply the
forms in them.")
	 (equal
	    (funcall
	       #'(lambda nil
		    (emtp:td:1 0)))
	    'branch-0)))
   (nil
      (progn
	 (emt:doc "Situation: Called in emtp:eval
The chosen branch's id matches a test clause's id")
	 (emt:doc "Response: It behaves as if its testpoints called the test forms")
	 (equal
	    (emtp:eval
	       (emtp:td:1 0)
	       (tp id-0
		  (arg a)
		  'test-ret-0))
	    'test-ret-0)))
   (nil
      (progn
	 (emt:doc "Situation: Called in emtp:eval
The chosen branch's id does not match any test clause's id")
	 (emt:doc "Response: It behaves as if its testpoints called the test forms")
	 (equal
	    (emtp:eval
	       (emtp:td:1 0)
	       (tp*
		  (:id unknown-id :count nil)
		  (arg a)
		  'test-ret-0))
	    'branch-0)))
   (nil
      (progn
	 (emt:doc "Situation: Test-function is called so that two branches are
visited.
Each clause's id matches one.")
	 (emt:doc "Response: Both branches are treated as their respective test clauses.")
	 (equal
	    (emtp:eval
	       (list
		  (emtp:td:1 0)
		  (emtp:td:1 1))
	       (tp id-0
		  (arg a)
		  'test-ret-0)
	       (tp id-1
		  (arg a)
		  'test-ret-1))
	    '(test-ret-0 test-ret-1))))
   (nil
      (progn
	 (emt:doc "Situation: The test-function passes `b-0' as an arg to the
first branch.
The test form returns the arg it gets")
	 (emt:doc "Response: The test form returns `b-0'")
	 (emt:doc "Proves: The test form sees the args that the main function passes to
the testpoint.")
	 (equal
	    (emtp:eval
	       (emtp:td:1 1 'foo)
	       (tp id-1
		  (arg a)
		  arg))
	    'foo)))
   (nil
      (progn
	 (emt:doc "Situation: The testpoint falls thru on certain conditions -
ie just if the arg is 2")
	 (emt:doc "Response: Return value is as expected.")
	 (equal
	    (emtp:eval
	       (list
		  (emtp:td:1 1 1)
		  (emtp:td:1 1 2))
	       (tp*
		  (:id id-1 :fallthru t :count 2)
		  (arg a)
		  (if
		     (equal arg 1)
		     'ignored
		     (throw 'emtp:tag-return 'test-ret-1))))
	    '(branch-1 test-ret-1))))
   (nil
      (progn
	 (emt:doc "Shows: Miscount error is withheld if other errors occur.")
	 (emt:doc "Situation: Foo is mocked
During execution, an earlier error keeps foo from ever being called.")
	 (emt:doc "Response: Error is not an emtp:miscount error, it is the
earlier error.")
	 (condition-case err
	    (emtp:eval
	       (progn
		  (signal 'emth:example-error "This error keeps the testpoint from being run")
		  (emtp:td:1 1 1))
	       (tp*
		  (:id id-1 :count 1)
		  (arg a)))
	    ('emtp:miscount nil)
	    ('emth:example-error t))))
   (nil
      (progn
	 (emt:doc "Shows: Miscount error is not withheld if a throw occurs.")
	 (emt:doc "Situation: Foo is mocked.
There is a catch outside `with-mock'.
During execution, a throw keeps foo from ever being called.")
	 (emt:doc "Response: Error is an emtp:miscount error.")
	 (condition-case err
	    (catch 'example-tag
	       (emtp:eval
		  (progn
		     (throw 'example-tag nil)
		     (emtp:td:1 1 1))
		  (tp*
		     (:id id-1 :count 1)
		     (arg a))))
	    ('emtp:miscount t)
	    (error nil))))
   (nil
      (progn
	 (emt:doc "Param: Keyword :bindings, syntax like `let'")
	 (emt:doc "Response: Inside the testpoint, values are bound as if by `let'.")
	 (emtp:eval
	    (list
	       (emtp:td:1 1))
	    (tp*
	       (:id id-1 :count 1 :bindings
		  ((twelve
		      (+ 2 10))))
	       (arg a)
	       (assert
		  (equal twelve 12)
		  t)))))
   (nil
      (progn
	 (emt:doc "Situation: There is only a finally form.
No args. 
No bindings.
Param: `:return' flag is NOT passed.")
	 (emt:doc "Response: `emtp:eval's value is the main form's value.")
	 (equal
	    (emtp:eval 12
	       (finally nil nil 13))
	    12)))
   (nil
      (progn
	 (emt:doc "Situation: There is only a finally form.
No args. 
No bindings.
Param: `:return' flag is passed non-nil")
	 (emt:doc "Response: `emtp:eval's value is the finally clause's value.")
	 (equal
	    (emtp:eval nil
	       (finally
		  (:return t)
		  nil
		  (+ 2 10)))
	    12)))
   (nil
      (progn
	 (emt:doc "Situation: There are two finally forms.")
	 (emt:doc "Response: Error.")
	 (progn
	    (assert
	       (emth:gives-error
		  (emtp:eval nil
		     (finally nil nil
			(+ 2 10))
		     (finally nil nil
			(+ 2 10)))))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: There is only a finally form.
No args. 
Binding `twelve' to a form that evals to 12.")
	 (emt:doc "Response: Normal - its return value is `emtp:eval's value.")
	 (equal
	    (emtp:eval nil
	       (finally
		  (:return t :bindings
		     ((twelve
			 (+ 2 10))))
		  nil twelve))
	    12)))
   (nil
      (progn
	 (emt:doc "Situation: There is a finally clause and a test point.
The testpoint collects one arg.")
	 (emt:doc "Response: The list of collected args is passed to the finally clause.")
	 (progn
	    (emtp:eval
	       (list
		  (emtp:td:1 1 'x)
		  (emtp:td:1 1 'y))
	       (tp*
		  (:id id-1 :count 2)
		  (arg a)
		  (emt:tp:collect arg))
	       (finally nil
		  (arg-list)
		  (assert
		     (emth:sets= arg-list
			'(x y))
		     t)))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: There is a finally clause and a test point.
The testpoint collects multiple args - 2.")
	 (emt:doc "Response: The collected lists are passed to the finally clause.")
	 (progn
	    (emtp:eval
	       (list
		  (emtp:td:1 1 'x)
		  (emtp:td:1 1 'y))
	       (tp*
		  (:id id-1 :count nil)
		  (arg a)
		  (emt:tp:collect arg a))
	       (finally nil
		  (arg-list a-list)
		  (assert
		     (emth:sets= arg-list
			'(x y))
		     t)
		  (assert
		     (emth:sets= a-list
			'(b-1 b-1))
		     t)))
	    t)))
   (nil
      (progn
	 (emt:doc "Shows: Results of clauses are passed to `finally' in the same
order that they occur.")
	 (emt:doc "Situation:  Two calls, each collecting a different value.")
	 (emt:doc "Response: Elements occur in the order the calls are made.")
	 (progn
	    (emtp:eval
	       (list
		  (emtp:td:1 1 'x)
		  (emtp:td:1 1 'y))
	       (tp*
		  (:id id-1 :count nil)
		  (arg a)
		  (emt:tp:collect arg))
	       (finally nil
		  (arg-list)
		  (assert
		     (equal arg-list
			'(x y))
		     t)))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Foo is called the expected number of times.")
	 (emt:doc "Response: No error.
The old version of foo is restored")
	 (flet
	    ((foo
		(&rest r)))
	    (let
	       ((old-foo-func
		   (symbol-function 'foo)))
	       (assert
		  (not
		     (emth:gives-error
			(emtp:eval
			   (list
			      (foo)
			      (foo))
			   (mock*
			      (:symbol foo :count 2))))))
	       (assert
		  (eq old-foo-func
		     (symbol-function 'foo))))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Foo is called the wrong number of times.")
	 (emt:doc "Response: Error.
The old version of foo is restored")
	 (flet
	    ((foo
		(&rest r)))
	    (let
	       ((old-foo-func
		   (symbol-function 'foo)))
	       (assert
		  (emth:gives-error
		     (emtp:eval
			(list
			   (foo))
			(mock*
			   (:symbol foo :count 2)))))
	       (assert
		  (eq old-foo-func
		     (symbol-function 'foo))))
	    t))))


;;;_  . emtp:handle-call

(emt:deftest-3 emtp:handle-call
   (nil
      (progn
	 (emt:doc "Situation: Passed an id that matches,")
	 (emt:doc "Response: Call that form with args.")
	 (equal
	    (emtp:eval
	       (catch 'emtp:tag-return
		  (emtp:handle-call emtp:*handlers* 'id-0
		     '(12)))
	       (tp id-0
		  (a)
		  a))
	    12))))


;;;_  . emtp:counter
;;Tests of counter forms' behavior together.
(emt:deftest-3 emtp:counter
   (nil
      (progn
	 (emt:doc "Situation: A counter to be visited once.
It is visited once")
	 (emt:doc "Response: No error.")
	 (let
	    ((counter
		(emtp:make-counter 1 'id-0)))
	    (eval
	       `(let
		   (,(emtp:counter:binding-form counter))
		   ,(emtp:counter:incf-form counter)
		   ,(emtp:counter:check-form counter)
		   t)))))
   (nil
      (progn
	 (emt:doc "Situation: A counter to be visited once.
It is visited twice.")
	 (emt:doc "Response: The check signals an error.")
	 (let
	    ((counter
		(emtp:make-counter 1 'id-0)))
	    (eval
	       `(let
		   (,(emtp:counter:binding-form counter))
		   ,(emtp:counter:incf-form counter)
		   ,(emtp:counter:incf-form counter)
		   (emth:gives-error ,(emtp:counter:check-form counter))
		   t)))))
   (nil
      (progn
	 (emt:doc "Situation: Inside condition-case.
The check form is evalled under conditions of certain error.")
	 (emt:doc "Response: The error is caught by condition-case as expected.")
	 (let
	    ((counter
		(emtp:make-counter 1 'id-0)))
	    (condition-case err
	       (eval
		  `(let
		      (,(emtp:counter:binding-form counter))
		      ,(emtp:counter:check-form counter)))
	       ('emtp:miscount t)
	       (error nil))))))

;;;_  . emtp:eval
(put 'emtp:eval 'emt:test-thru 'emtp)


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/testpoint/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/testpoint/tests.el ends here
