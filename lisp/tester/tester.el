;;;_ tester/tester.el --- Emtest main tester file

;;;_. Headers
;;;_ , License
;; Copyright (C) 2008,2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: maint, tools

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


;;;_ , Requires

(when (not (fboundp 'emt:deftest))
    (defmacro emt:deftest (&rest dummy))
    (defmacro emt:if-avail (&rest dummy)))
(eval-and-compile
   (when (not (require 'tp nil t))
      (defmacro emt:testpoint (id args &rest rest)
	 `(progn ,@rest))))

(eval-when-compile
   (require 'cl))
(require 'result-types)
(require 'testral)


(emt:if-avail
   (require 'deep-type-checker)
   (require 'emt-util)
   (require 'el-mock)
   (require 'tp) ;;`tp' is soft-required above but hard-required to
		 ;;run certain tests.
   (require 'eg))
;;;_. Body

;;;_ , Test manager

(defvar emt:the-manager nil 
   "The manager.  Only one instance is contemplated." )
;;;_  . Interface

'  ;;Not used yet
(define-interface emt:test-finder
   ;;Not right
   #'(lambda (accessor-form rest-form)
	(list
	   () ;;No extra args
	   ;;This form does what?
	   ;;
	   ))
   "The interface of the test manager"
   (receive-check)
   (receive-info)
   )

;;;_  . The class itself  (Obsolete, redesigned)
(defstruct (emt:test-manager (:constructor make-emt:test-manager))
   ""
   result-collector
   context
   test-parameters ;;As passed in
   db
   what-to-record
   )

;;;_  . Classes of sub-parts of test-manager
;;Not quite obsolete.  `get-outside-events' is still relevant.
(defstruct emt:result-collector
   ""
   results
   current-stage
   get-outside-events
   )
;;Obsolete
'
(defstruct emt:test-manager:context-manager
   ""
   db-ID
   test-ID
   )

;;Format TBD.  Info to indicate what results to collect.  Possibly a
;;kv list.
(defstruct emt:test-manager:what-to-record
   ""
   )

;;More limited than a full connection.  Maybe just exists?, read, and
;;write.  For now just punt.
(defstruct emt:test-manager:db-client
   ""
   ;;??
   )

;;;_  . Its functions
(defun emt:test-manager:receive-check ()
   ""
   
   (let*
      ()
      
      ))

(defun emt:test-manager:receive-info ()
   ""
   
   (let*
      ()
      
      ))

;;;_ , Objects passed around in test-finder (check vs redesign)
;;Obsolete.  `emt:test:top-data' does this now.
(defstruct emt:test-finder:top-data
   ""
   test-IDs-info 
   db-IDs-info
   skip-control
   progress-reports-p
   what-to-record
   connections
   )

;;;_ , test-launch
;;;_  . List of the usual test-protectors

(defconst emt:tester:usual-test-protectors 
   '(
       test-message-trap
       test-error-trap
       save-window-excursion
       with-temp-buffer
       ;;mock-protect ;;Maybe.  Add it here if it's loaded.
       )
   "" )
;;;_  . make emt:result-collector

;;tests-own-args control-args db-IDs-info what-to-record connections
(defun make-emt:result-collector (tests-own-args top-data)
   ""
   
   
   (let*
      ()
      
      ))
;;;_   , Tests
'
(rtest:deftest make-emt:result-collector

   ;;Docstring is in info-about.
   ;;Initial stage is in place
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   
   )

;;;_  . Make test manager
;;This bunch of layers here is to comb out transforming tests-own-args *
;;top-data to test-manager.
;;In fact, this is potentially a RPC.

;;We know to-db from top-data.  But now we get it from connections.

;;This is the layer underneath the connection-maker, so we don't need
;;connections here.
(defun make-emt:test-manager (tests-own-args control-args db-IDs-info
				what-to-record) 
   ""
   
   (let*
      ()
      
      ))

;;;_   , Tests
'
(rtest:deftest make-emt:test-manager
   
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   
   )

;;;_  . emt:run

;;Previous layer is code within the clause explore method.


(defun emt:run (form test-ID tests-own-args control-args top-data
		  &optional debug)
   "Returns (form-result test-result)"

   (let*
      (  (tm
	    ;;Check the type of return value.
	    (make-emt:test-manager tests-own-args top-data))
	 (value
	    (emt:run-2 tm form tests-own-args debug))
	 (test-results
	    ;;Tell test-manager it's done.
	    ;;Get results from tm
	    ;;emt:test-manager-result-collector
	    ))
      (list value test-results)))
;;;_  . Test helper

;;Assumes tests-own-args can have the form of an empty list.
(defconst emt:run:thd:simplest-tests-own-args () "" )
(defconst emt:run:thd:simplest-top-data 
   (make-emt:test-finder:top-data) 
   "" )

;;;_  . Tests
'
(rtest:deftest emt:run
   
   ;;And a test of `emt:with-test-manager' shows that with no
   ;;test-manager available, it errors.
   ( "Situation: Form needs test-manager to be present.
Response: No error."
      (not
	 (rtest:gives-error
	    (emt:run
	       '(emt:with-test-manager x t)
	       emt:run:thd:simplest-tests-own-args
	       emt:run:thd:simplest-top-data))))
   
   
   ( "Situation: The form returns a recognized value.

Response: The right value is returned."
      (equal
	 (emt:run
	    'the-right-value
	    emt:run:thd:simplest-tests-own-args
	    emt:run:thd:simplest-top-data)
	 'the-right-value))
   
   
   )

;;;_  . Place form within test-protectors

(defun emt:tester:surround (form protectors)
   ""
   (let
      ((rv-protectors (reverse protectors)))
      
      (dolist (i rv-protectors form)
	 (setq form (list i form)))))

;;;_   , Tests

(rtest:deftest emt:tester:surround

   (  "Args: Empty list of protectors.
Result: Just form."
      (equal
	 (emt:tester:surround 'foo ())
	 'foo))
   
   (  "Args: List of one protector.
Result: Form is surrounded by that protector."
      (equal
	 (emt:tester:surround 'foo '(progn))
	 '(progn foo)))

   (  "Args: List of one protector, more complicated form.
Result: More complicated form is correctly surrounded."
      (equal
	 (emt:tester:surround '(let (a b) foo) '(progn))
	 '(progn (let (a b) foo))))

   (  "Args: List of three protectors.
Result: Form is surrounded by all three in order, first outermost."
      (equal
	 (emt:tester:surround 'foo '(progn save-excursion with-temp-buffer))
	 '(progn (save-excursion (with-temp-buffer foo)))))
   
   )
;;;_  . Figure out any extra test-protectors

(defun emt:tester:get-extra-protectors (tests-own-args)
   "Return the list of test-protectors in TESTS-OWN-ARGS."
   
   (let*
      (
	 (got-protectors
	    (memq 'protectors tests-own-args))
	 (protectors 
	    (if got-protectors
	       (car got-protectors)
	       ())))
      (unless (listp protectors)
	 (error "protectors is not a list"))
      protectors))

;;;_   , Tests
'
(rtest:deftest emt:tester:get-extra-protectors

   ;;Test of emt:tester:get-extra-protectors.  Requires a bogus
   ;;tests-own-args object -- but syntax is immature right now.
   (  "Situation: protectors is not a list
Response: Error."
      (progn) ;;Test-form
      )
   )

;;;_  . Figure out surrounders

(defun emt:tester:get-surrounders (tests-own-args debug)
   ""
   (append
      emt:tester:usual-test-protectors
      (emt:tester:get-extra-protectors tests-own-args)
      ;;Debugging, if present, is innermost.
      (if debug
	 '(emt:with-debugging)
	 ())))

;;;_   , Tests
'
(rtest:deftest emt:tester:get-surrounders
   
   
   (  "Demonstrate: emt:tester:get-surrounders returns a list of symbols."
      
      (every
	 #'symbolp
	 (emt:tester:get-surrounders 
	    emt:run:thd:simplest-tests-own-args
	    nil)))
   

   (  "Demonstrate: emt:tester:get-surrounders returns a list of symbols."
      
      (every
	 #'symbolp
	 (emt:tester:get-surrounders 
	    emt:run:thd:simplest-tests-own-args
	    t)))
   
   ;;Since tester's always-surrounders is not controlled by this, we
   ;;can't test proper contents, but since we just append, should be
   ;;no problem.
   )

;;;_  . Call with a test-manager

;;$$TBD.  Maybe form be a lambda instead?
(defun emt:run-2 (test-manager form tests-own-args &optional debug)
   "Call a form, protectedly.  Returns the form's return value."
   (let
      (  (form-2
	    (emt:tester:surround form
	       (emt:tester:get-surrounders tests-own-args debug)))
	 (emt:the-manager test-manager))
      (eval form-2)))

;;;_   , Tests

;;It's fairly direct, and tested through `emt:run'
;;;_ , explore-methods

;;All later except explore-clause
;;;_  . Interface

'
(define-interface emt:explore-method
   ;;Not right
   #'(lambda (accessor-form rest-form)
	(list
	   '(vtable)
	   `(apply
	       (funcall
		  #',accessor-form
		  vtable)
	       ,@rest-form)))
   "The interface of the test finder" 
   (run form test-ID tests-own-args control-args top-data)

   )

;;For now just define these as forwarders to the "clause" method.  All
;;the mechanism is dormant for now.
'
(defun emt:explore-method-run (vtable &rest r)
   ""
   (apply #'emt:explore-method:clause:run r))


;;;_  . Match method

;;For now, always the `clause' method, which relates to
;;`emt:test-ID:e-n:form'. 
'
(defun emt:explore-method:find ()
   ""
   emt:explore-method:clause:vtable)

;;;_  . explore-clause

;;;_   , vtable
'
(defconst emt:explore-method:clause:vtable 
   (make-emt:explore-method
      :emt:run #'emt:explore-method:clause:run
      )
   "" )

;;;_   , emt:explore-method:clause:run

(defun emt:explore-method:clause:run (form test-ID tests-own-args
					control-args top-data) 
   ""


   (let*
      ((debug
	  (memq 'debug control-args)))

      (emt:run form test-ID tests-own-args
	 control-args top-data debug)))



;;;_ , test finder
;;;_  . Interface

;;For now, local and punt.
'
(define-interface emt:test-finder
   ;;Not right
   #'(lambda (accessor-form rest-form)
	(list
	   () ;;No extra args
	   ;;$$Call that part of connection-manager, but we have yet
	   ;;to define a connection-manager object.
	   ))
   "The interface of the test finder" 
   (emt:explore-tests
      info-from-top ;;A `emt:test-finder:top-data'
      ))


;;;_  . Pending list
(defvar emt:test-finder:pending-list () "" )
;;;_   , Type spec
(defstruct emt:test:pending
   ""
   ;;This is correct: Descendants are of `emt:test-ID:e-n', not of
   ;;this class.
   (id () :type emt:test-ID:e-n)  ;;Was emt:test-ID

   (path-prefix () :type emt:testral:partial-suite-id)

   ;;This may be a mistake.  Outside should not control this.
   ;;Props are only used for exploring clauses, but they are passed
   ;;down suites.  Could be a subtype, but it's fairly general.  Let's
   ;;let it be in the base type.
   (properties () :type (repeat (list symbol *))))

;;;_   , emt:test-finder:pending-list:check-type (Validation)
(defun emt:test-finder:pending-list:check-type ()
   ""
   (require 'deep-type-checker)
   (emt:deep-type:check
      emt:test-finder:pending-list
      (list emt:test:pending)))


;;;_  . Entry

;;;_   , emt:test-finder:local:entry:push-tests
'
(defun emt:test-finder:local:entry:push-tests (test-IDs-info)
   ""
   
   (let
      ((list ()))
      (dolist (test test-IDs-info (nreverse list))
	 (push test list))))

;;;_    . Tests
'
(rtest:deftest emt:test-finder:local:entry:push-tests

   ;;PUNT for now.
   ;;Some test-IDs-info say not to push them on.  Indicated by having
   ;;the `advisory' key non-nil in -control.
   (  "Situation: Two items are advisory, three are normal.
Response: Only the normal ones are included."
      ;;Check with `set='
      (progn) ;;Test-form
      )
   
   )


;;;_   , emt:test-finder:local:entry
;;Obsolete; use `emt:test-finder:top'
'
(defun emt:test-finder:local:entry (info-from-top)
   "Entry point for test-finder"
   
   (setq
      emt:test-finder:pending-list
      (emt:test-finder:local:entry:push-tests 
	 (emt:test-finder:top-data-test-IDs-info info-from-top)))
   
   (while (not (null emt:test-finder:pending-list))
      ;;$$Send value back via connection-manager
      (emt:test-finder:explore-one
	 (pop emt:test-finder:pending-list)
	 info-from-top)))

;;;_    . Tests

;;Situation:  Forced Error.  List is not a list
;;Response: Does not give error.  Calls back with a group report with
;;appropriate status. 

;;;_  . Explore one popped off the pending list
;;NOT obsoleted by `emt:test:explore-one'; they will probably be merged.
(defun emt:test-finder:explore-one (test info-from-top)
   ""
   ;;Old way of doing this.


   ;;Don't touch `test' until we've made a group-result, which will
   ;;be used to express any outside errors here.
   '
   (let*
      (
	 (info ())
	 ;;A callback that adds to `info'
	 (cb-set-info
	    (lambda (el)
	       (push el info))))

      ;;Will be error trapping too.  On error, send back an
      ;;appropriate result
      (let
	 (
	    ;;Check types.  Check that `test' is a `emt:test-info' and
	    ;;test-ID is a `emt:test-ID'.
	    (test-ID
	       (emt:test-info-test-ID test)))

	 ;;Will be error trapping too.  On error, send back an
	 ;;appropriate result
	 (let
	    (  (status
		  (catch 'emt:status 
		     (emt:test-finder:explore-one-2 test cb-set-info
			info-from-top))))
	    
	    ;;Normal return
	    (make-emt:result-group
	       :grouping test-ID
	       :info info
	       :status status)))))


(defun emt:test-finder:explore-one-2 (test cb-set-info info-from-top)
   ""

   ;;To report a failure, (throw 'emt:status Bad-status-object)
   (let*
      (
	 
	 ;;Find method.  
	 ;;If `nil' (not found), 
	 ;;(throw 'emt:status make-emt:result:status:bad-method)
	 (explore-method
	    (emt:explore-method:find))
	 
	 )

      ;;Punt most of this for now, add it as needed.

   ;;  * Determine specific info about the test, from list.  If that has a
;;    problem, eg key-value list is not a list, report a problem
;;    (bad-before-test::bad-control-data)

;;  * If test-ID was dormantized in this session: status is
;;    not-explored/session.  Done.
   
      ;;Find the exploration-control info for this method, before
      ;;test.  

      ;;Pass that info and current threshhold values to method.  It
      ;;may do (re)loading at this point.  It tells us whether to
      ;;proceed.  If not, status is not-explored/command-stops-here.
      ;;Done.

;;  * Find the node's test-parameters (Resp of test-finder's
;;    explore-methods) Also find any defaults that might apply (project,
;;    library).  

;;    If this search fails or errors:
;;      * trap the error (maybe only if a flag is set)
;;      * status is bad-before-test:bad-precondition:Couldn't find
;;        data(The-error).  Done.

;;  * Decide dormancy.  The work is responsibility of dormancy decider,
;;    informed by test-parameters info.  It returns whether we are to
;;    proceed.

;;    * Retain the return value (the boolean whether we are to proceed).
;;      If it is "no", status is already set to Dormant(specific-reason)

;;    * Decide whether the computation aborted.  How:
;;      * Get test-manager's result.  
;;      * Summarize. (Responsibility of light summarizer)
;;      * If it aborted or the result is not all-passed, 
;;        * it aborted
;;        * Set whether-to-proceed to "no".
;;        * Set status to bad-before-test:bad-precondition:Computation
;;          failed.  Its stage info is the results collected as normal by
;;          test-manager (Which may or not have been marked aborted).
;;          Done.

	  ;;$$WRITEME
      ;;Call explore-method to proceed.  Haven't defined the call
      ;;yet.  

;;  * If were are to proceed, explore in the respective idiom.  If the
;;    explore method fails, the method itself has the responsibility to
;;    set the status to bad-before-test:bad-body.

;;    * If it's a test-clause, 
;;      * Ready a was-run result
;;      * launch it.
;;      * use the results as the status field
;;    * Otherwise 
;;      * ready a group-explored result
;;      * find the contents
;;      * for each element
;;        * Calculate test-ID
;;        * If we already explored it, skip it (responsibility of
;; 	 pending-list adder)
;;        * put the relevant info on the list of pending things to
;; 	 explore.
;;        * Put a placeholder of test-ID in the group-explored status
;;      * When it's done, that's an element of the status's group.

      
      ))

;;;_   , Tests
'
(rtest:deftest emt:test-finder:explore-one-2

   ;;Return satisfies emt:result-group-p for any input.
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )

   ;;Dormant examples are dormant status
   
   )

;;;_  . Find the relevant explore method

;;For now, there's only the one.

;;;_ , At test-call time (May go into standard library)

;;;_  . emt:with-debugging

(defmacro emt:with-debugging (&rest form)
   ""
   
   `(progn
       (debug) 
       ,@form))

;;;_   , Tests

;;Can't easily automatically test that it in fact debugs.

;;;_  . emt:with-test-manager

;;Used in testing this.
(defmacro emt:with-test-manager (name &rest body)
   ""
   
   `(if
       (null emt:the-manager)
       (error "There is no test manager")
       (let
	  ((,name emt:the-manager))
	  ,@body)))

;;;_   , Tests
'
(rtest:deftest emt:with-test-manager

   ;;This is a strange test but it has to be.  
   (  "Situation: There is no test manager.
Response: Error."
      (rtest:gives-error
	 (let ;;Cancel the test-manager.  No checks can exist in this
	    ;;scope.
	    ((emt:the-manager nil))
	    (emt:with-test-manager x t))))

   ;;When interface is more mature, test that we can affect an outside
   ;;test manager from inside emt:with-test-manager.  Probably use
   ;;(emt:run-2 The-form emt:run:thd:simplest-tests-own-args)
   
   )

;;;_ , Persist functions 
;;These must be visible to tests.  They may be moved, but not into
;;emt-persist.el which is implementation.

;;;_  . emt:persist
;;
;;;###autoload
(defun emt:persist (id &optional backend)
   "Return a persisting object or a placeholder"
   (let
      ((backend
	  (or
	     backend
	     (let
		((cell (assoc 'db-id emt:trace:properties)))
		(when cell
		   (second cell)))
	     ;;Here add any other ways of learning the backend
	     (error "No backend was provided"))))
      (make-emt:db:id-index.
	 :id id
	 :backend backend)))
;;;_   , Tests
(rtest:deftest emt:persist

   (  "Situation: `emt:trace:properties' has the db-id property bound.
Param: The backend param is not given.
Response: The value of the `db-id' property is used as backend."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    (props (db-id "my-db"))
	    ("Clause is not used" ()))
	 (emt:test:destructure-suite 'dummy-sym
	    (let* ((emt:trace:properties props)
		     (placeholder (emt:persist 'id-0)))
	       (assert
		  (equal 
		     (emt:db:id-index.-backend placeholder) 
		     "my-db")
		  t))
	    t)))
   
   )

;;;_  . emt:persist:loop  (Not yet)
;;Takes (ID FILTER)



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

   ;;$$CHANGEME.  This will change to just send notes to a listener
   (let
      ((report 
	  ;;Make an ID.  Can be just by counting.
	  ;;Need parent's ID.
	  ;;Make&send a `emt:testral:check:push'.  Use
	  ;;(emt:testral:add-note start-note '(should))
	  (make-emt:result:event:grade
	     :form form))
	 ;;Create empty badnesses, just because `unwind-protect' must
	 ;;see it
	 badnesses)

      ;;`unwind-protect' this, in place of the last form:
      ;;Make&send a `emt:testral:check:pop'.  To know
      ;;badnesses, if any: Each case assigns to the `badnesses'
      ;;variable, which we use in making that. 
      
      (emt:trace:protect
	 (condition-case err
	    (let*
	       (  
		  (form-x (emt:wrap-form form))
		  (retval
		     (eval form-x)))
	       
	       (setf (emt:result:event:grade-grade report) 
		  (if retval 'pass 'fail))
	       retval)

	    ;;$$ADDME There'd be a clause to intercept a special
	    ;;"dormant" error too, for contributing checks that were
	    ;;disabled.

	    ;;And a `many-bads' error that directly held a collected
	    ;;list of badnesses?  In case `and' both fails and is
	    ;;dormant.  Maybe that's the special error to use always.

	    ;;Add to badnesses, unless already there.
	    ('emt:already-handled 
	       (setf (emt:result:event:grade-grade report) 'ungraded)
	       (signal 'emt:already-handled ()))
	    ;;Add to badnesses, unless already there.
	    (error
	       (setf (emt:result:event:grade-grade report) 'ungraded)
	       (emt:trace:add-to-stored-diag
		  (make-emt:result:diag:error :error err))
	       (signal (car err)(cdr err))))

	 (setf (emt:result:event:grade-diagnostic-info report) 
	    diags)
	 (setf (emt:result:event:grade-info-about report) 
	    info-about)
	 (if
	    (boundp 'emt:trace:current-event-list)
	    (push report emt:trace:current-event-list)
	    ;;$$Want an error specific to emtest
	    (signal 'error report)))))


;;;_   , Tests
(put 'emt:should-f 'rtest:test-thru
   'should)

;;;_  . should
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

;;;_ , emt:test:trap-errors

(defmacro emt:test:trap-errors (&rest body)
   ""
   
   `(condition-case err
       (progn ,@body)
       ('emt:already-handled
	  ;;For now, do nothing.
	  nil)
       (error 
	  (error "Not handling unexpected errors yet"))))

;;;_  . Tests

(rtest:deftest emt:test:trap-errors

   (  "Shows: Works with `emt:tester:surround'."
      (equal
	 (eval
	    (emt:tester:surround 12 '(emt:test:trap-errors)))
	 
	 12))
   
   (  "Situation: Body throws an emt:already-handled error.
Response: (Punt) Marks the event-group aborted.
Adds no error report.
Does not signal error."
      (progn
	 (assert
	    (not
	       (emt:gives-error
		  (emt:test:trap-errors
		     (signal 'emt:already-handled ())))))
	 t))
   
   ;;More to add.  See [[id:ca903ca0-bd5d-4985-8cd3-a5a4dd998b5c][]]
   
   )

;;;_ , Run tests
;;;_  . Counter (Associated with receive, not with tester)
(defvar emt:test:testrun-counter 0 
   "A counter used to make testrun-id.
With `cl' loaded, use it as (incf emt:test:testrun-counter)." )

;;;_  . Helper emt:test:form->test-id
;;Obsolescent
(defun emt:test:form->test-id (form)
   ""
   (make-emt:test-ID
      :context ()
      :explore-next
      (make-emt:test-ID:e-n:form
	 :test-form form)))

;;;_  . emt:test:sexp-at-point->result
;;A test helper.  Obsolete.  No longer makes sense, because
;;the output is what is tested.
'
(defun emt:test:sexp-at-point->result (form)
   ""
   (emt:test-finder:top 
      (make-emt:test-ID:e-n:form
	 :test-form form)
      (list "form")
      (prin1-to-string (incf emt:test:testrun-counter))
      ;;$$FIXME  This no longer makes sense.  Now we should test thru
      ;;receive, which puts results into a more orderly form.
      #'identity))

;;;_  . emt:test:ts:run-test
;;Meant to support view-tests
(defun emt:test:ts:run-test (test-form callback &optional prefix testrun-id)
   "
NB, TEST-FORM is a *test-form*, which should begin with a docstring."
   (emt:test-finder:top 
      (make-emt:test-ID:e-n:form
	 :test-form test-form)
      (or prefix (list "test-form"))
      (or testrun-id "0")
      callback))

;;;_  . emt:test:dispatch-normal
;;Cheat for now: Always know to use emviewer.  Later maybe use a
;;customizable variable.
(require 'emviewer "viewers/emviewer")
(defun emt:test:dispatch-normal (what-to-run &optional prefix)
   ""
   (emt:test-finder:top 
      what-to-run 
      prefix  ;;Default is the empty list.
      (prin1-to-string (incf emt:test:testrun-counter))
      #'emtest:viewer:receive))

;;;_  . emt:test:sexp-at-point

(defun emt:test:sexp-at-point (form)
   ""
   (interactive 
      (list 
	 (save-excursion (read (current-buffer)))))
   
   (emt:test:dispatch-normal
      (make-emt:test-ID:e-n:form
	 :test-form form)
      (list "form")))
 
;;;_   , Tests

(rtest:deftest emt:test:sexp-at-point

   ;;This can only be tested fully-manually.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:test:sexp-at-point
	 '("Situation: While executing a dummy test with no checks"
	     (progn))))

   '  
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:test:sexp-at-point
	 '("Situation: While executing a dummy test
with a successful check.   The test is fully written out." 
	     (emt:should-f t))))
   
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:test:sexp-at-point
	 '("Situation: While executing a dummy test with a successful
check.  The test is a macro.
"
	     (should t))))

   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:test:sexp-at-point
	 '("Situation: While executing a dummy test with a failing check"
	     (should nil))))
   '
   (  "Situation: Body raises an error of type `emt:already-handled'.
Response: Essentially nothing special is reported."
      (emt:test:sexp-at-point
	 '("Situation: While executing a dummy test that raises
`emt:already-handled'."
	     (signal 'emt:already-handled ()))))
   

   )

;;;_  . emt:test:suite
;;Obsolete.  Use `emt:test:defun-at-point'

;;;_   , Test supporter emt:test:th:run-suite
(defun emt:test:th:run-suite (suite-sym func)
   ""
   
   (emt:test-finder:top 
      (make-emt:test-ID:e-n:suite
	 :suite-ID suite-sym) 
      '()
      "0" 
      func))

;;;_   , Test helper emt:test:suite:th:get-id-list

(defun emt:test:suite:th:get-id-list (sym) 
   ""
   (let
      ((test-id-list ()))
      (emt:testpoint:eval
	 (emt:test:th:run-suite sym #'ignore)
	 (tp*
	    (:id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 
	       :count nil 
	       :fallthru t)
	    (test-id)  ;;Removed e-n
	    (typecase test-id  ;;Was e-n
	       ;;Intercept clause
	       (emt:test-ID:e-n:indexed-clause
		  (push test-id test-id-list)
		  ;;Don't try to run it, return instead.
		  (throw 'emt:testpoint:tag-return nil))
	       ;;Let suite fall thru to handler
	       (emt:test-ID:e-n:suite t)
	       ;;We don't expect to see any other types of
	       ;;explores.
	       (t
		  (error "This test shouldn't reach here")))))
      test-id-list))


;;;_   , Tests
(rtest:deftest emt:test:suite

   (  "Situation: SUITE-SYM has a test suite defined.
Response: That test-suite is run.  A result is returned."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    ("Docstring" ()))
	 (emt:testpoint:eval
	    (emt:test:th:run-suite 'dummy-sym 
	       #'(lambda (x)
		    (emt:deep-type:check x emt:result-group)))
	    (tp-reached tp:798212b4-1abe-4779-beb1-baf53ff39a8c 
	       1)
	    (tp* 
	       (  :id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 
		  :count nil
		  :fallthru t)
	       (test-id)
	       (typecase test-id
		  ;;Intercept clause, count it
		  (emt:test-ID:e-n:indexed-clause
		     ;;NOT emt:test-ID:e-n:form
		     ;;A reached-point for counting invocations.
		     (emt:testpoint tp:798212b4-1abe-4779-beb1-baf53ff39a8c
			())
		     ;;Fall thru
		     t)
		  ;;Let suite fall thru to handler
		  (emt:test-ID:e-n:suite t)
		  ;;We don't expect to see any other types of
		  ;;explores.
		  (t
		     (error "This test shouldn't reach here")))))
	 t))
   
   
   (  "Shows: Exactly the clauses of suite are run.
Situation: Suite has two clauses defined.  
Full exploration is used (Meaningless for now)
Response: Just two clauses are run.
Those suites have distinct IDs."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    ("Clause 1" ())
	    ("Clause 2" ()))
	 (let
	    ((all-test-ids))
	    (emt:testpoint:eval
	       (emt:test:th:run-suite 'dummy-sym #'ignore)
	       (tp* 
		  (
		     :id tp:798212b4-1abe-4779-beb1-baf53ff39a8c 
		     :count 2
		     :fallthru t)
		  (test-id)
		  (push test-id all-test-ids))
	       (tp* 
		  (  :id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 
		     :count nil
		     :fallthru t)
		  (test-id)
		  (typecase test-id
		     ;;Intercept clause, count it
		     (emt:test-ID:e-n:indexed-clause
			;;NOT emt:test-ID:e-n:form
			(emt:testpoint tp:798212b4-1abe-4779-beb1-baf53ff39a8c
			   (test-id))
			;;Don't try to run it, return instead.
			(throw 'emt:testpoint:tag-return nil))
		     ;;Let suite fall thru to handler
		     (emt:test-ID:e-n:suite t)
		     ;;We don't expect to see any other types of
		     ;;explores.
		     (t
			(error "This test shouldn't reach here")))))
	    (assert
	       (emt:util:all-different all-test-ids)
	       t))
	 t))
   

   (  "Situation: A clause has changed within a suite.
Operation: That test suite is run.
Behavior: The clause still gets the same test-ID as before."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    ("Clause 1" (original-form)))

	 (let*
	    (  (old-test-id-list
		  (emt:test:suite:th:get-id-list 'dummy-sym))
	       (old-test-id (car old-test-id-list)))
	    (assert (= (length old-test-id-list) 1) t)
	       
	    (emt:deftest-2 dummy-sym
	       ("Clause 1" (new-form)))
	       
	    (let*
	       (  (new-test-id-list
		     (emt:test:suite:th:get-id-list 'dummy-sym))
		  (new-test-id (car new-test-id-list)))
	       (assert (= (length new-test-id-list) 1) t)

	       (assert 
		  (equal old-test-id new-test-id)
		  t)))
	 t))
   
   )
;;;_  . emt:test:defun-at-point
;;;###autoload
(defun emt:test:defun-at-point (arg)
  "Run tests on the function or suite under point.

If prefix ARG is non-nil, eval it first.

Does nothing if the buffer is not in a known lisp mode."

   (interactive "P")
   ;;Only proceed if we know how to run tests
   (when (eq major-mode 'emacs-lisp-mode)

      ;;If `arg', eval that defun (or w/e) first.
      (when arg (eval-defun nil))
      (let
	 ((suite-name
	     (emt:suite-sym-at-point)))
	 (check-type suite-name symbol)
	 ;;Why was this circumlocution neccessary before?
	 '(funcall #'emt:test:suite suite-name)
	 (emt:test:dispatch-normal 
	    (make-emt:test-ID:e-n:suite
	       :suite-ID suite-name)))))


;;;_   , Tests
;;This should be direct; just join other functions

;;;_   , Helpers (Lisp-syntax-reading stuff)

(defconst emt:defun-types 
  '(defun defun* defsubst defsubst* defmacro defmacro* defmethod
      deftype defadvice
      emt:deftest-2)
   
  "List of defun-variant symbols we might see" )

(defun emt:suite-sym-at-point-x (arg)
   "Return the symbol that names the definition at point.
With `nil' ARG, look backwards for it.
With non-nil ARG, look forwards for it."
   (condition-case err
      (save-excursion
	 (beginning-of-defun (if arg -1 nil))
	 (down-list 1)
	 (let*
	    (  (type
		  (read
		     (current-buffer)))
	       (symbol
		  (if
		     (memq type emt:defun-types)
		     (read
			(current-buffer))))
	       ;;This transformation belongs in an explore-method instead
	       ;;of here.
	       (proxy
		  (get symbol 'rtest:test-thru)))
	    (or proxy symbol)))
      (scan-error nil)
      ))

(defun emt:suite-sym-at-point () 
   "Return the symbol of the test suite relevant to the definition at point"
   
   (or
      ;;First try to find it backwards
      (emt:suite-sym-at-point-x nil)
      ;;If that fails, try to find it forwards
      (emt:suite-sym-at-point-x -1)))


;;;_    . Tests
(emt:deftest-2 emt:suite-sym-at-point

   ;;Need example text, presumably from one or more example files.
   ;;They are in "t/examples/editor/find-names/1.el"

   ;;To set up point - mockbuf should provide mark conversion but it
   ;;doesn't yet.

   ;;Test situations: In a test definition.  In a function definition.
   ;;Between them.  After a definition (find that, not next).

   ;;So foo should be found for:
   ;;!Before all foo definitions.
   ;;!In function.
   ;;!Between function and test.
   ;;!In test definition.
   ;;!In test clause.

   ;;Something else or nothing (don't care) should be found for "After
   ;;all foo definitions" (not (eq (emt:suite-sym-at-point) 'foo))


   ;;Nothing should be found in empty file (Make from string)
   ("Situation: In empty file
Response: Return `nil'."
      (with-buffer-containing-object 
	 (:string "\n\n\n\n")
	 (emacs-lisp-mode)
	 (should
	    (null 
	       (emt:suite-sym-at-point))))))



;;;_  . emt:test:library

;;;###autoload
(defun emt:test:library (library)
   "Run the test suites of LIBRARY"
   
   (interactive
      (list
	 (completing-read 
	    "Run test suites of which library: "
	    load-history
	    nil	;;No narrowing provided yet.
	    t)))

   (let*
      (
	 (test-id
	    (make-emt:test-ID:e-n:library:elisp-load
	       ;;$$INSPECTME Should this by symbol or string?  Or
	       ;;allow both?
	       :load-name (intern-soft library))))
      (emt:test:dispatch-normal test-id)))
;;;_   , Helper emt:test:lib-sym->suites

(defun emt:test:lib-sym->suites (lib-sym)
   ""
   (let*
      (
	 (lib-data (assoc lib-sym load-history))
	 ;;Or could (remove* :test-not)
	 ;;List of symbols.
	 (suites
	    (loop
	       for x in (cdr lib-data)
	       if 
	       (and
		  (symbolp x)
		  (get x 'emt:suite))
	       collect x)))
      suites))
;;;_    . Test data
(emt:eg:define xmp:699d3b9f-cb26-4964-b23d-94b07a44d75d
   ((project emtest)(library tester)(section entry-points))
   (transparent-tags () (type))
   (group
      ((count 2))
      (item ((type name)) "example-2")
      (item ((type count)) 2)
      (item ((type suite-sym-list)) '(foo bar))
      (item ((type file-load-history))
	 `(,(emt:eg (type name))
	     ,@(emt:eg (type suite-sym-list)) 
	     (provide . example-2))))
   
   (group
      ((count 1))
      (item ((type name)) "example-1")
      (item ((type count)) 1)
      (item ((type suite-sym-list)) '(foo))
      (item ((type file-load-history))
	 `(,(emt:eg (type name))
	     ,@(emt:eg (type suite-sym-list)) 
	     (provide . example-1))))
   
   (item ((type load-history))
      (emt:eg:map count nil
	 (emt:eg (type file-load-history)))))
;;;_    . Test setup helper

(defmacro emt:test:library:th 
   (+tagset &rest body)
   "Run BODY in an environment with a certain example library defined.
+TAGSET is a tagset narrowing, as for `eg'."
   
   `(emt:eg:narrow ;;Narrow the examples.
       ((project emtest)
	  (library tester)
	  (section entry-points)
	  ,@+tagset)
	 
       (let
	  ((suite-sym-list (emt:eg (type suite-sym-list)))
	     ;;Temporarily bind load-history to known value.
	     (load-history 
		(emt:eg:value 
		   :narrow ((type load-history)) 
		   :ignore-tags (count))))

	  ;;Define the suites (inside a noprops)
	  (let-noprops suite-sym-list
	     (dolist (sym suite-sym-list)
		(eval ,'`(emt:deftest-2 ,sym ())))
		,@body)
	  t)))


;;;_    . Tests


(rtest:deftest emt:test:lib-sym->suites

   (  "Situation: There are two suites in the library.
Response: Return a list of those suites' symbols."
      (emt:test:library:th ((count 2)) 
	 (let* 
	    ((syms (emt:test:lib-sym->suites (emt:eg (type name)))))
	    (assert
	       (equal
		  (length syms)
		  (emt:eg (type count)))
	       t)))))


;;;_   , Tests
(rtest:deftest emt:test:library

   (  "Situation: In the known (count 2) context of libraries and
suites.
Full exploration is used (Meaningless for now)
Operation: We launch a library
Response: The suite-handler part runs exactly twice
\(Not tested: Exactly those two suites are seen.)
\(Not tested: Those suites have distinct IDs.)
"
      (emt:test:library:th ((count 2))
	 (emt:testpoint:eval
	    (emt:test:library
	       (emt:eg (type name))
	       ;;$$Library does not return a result object yet.  Will
	       ;;type-check it.
	       ;;(emt:deep-type:check x emt:result-group)
	       #'ignore)
	    
 	    (tp-reached tp:798212b4-1abe-4779-beb1-baf53ff39a8c 
	       (emt:eg (type count)))
	    (tp* 
	       (  :id tp:a084136e-8f02-49a5-ac0d-9f65509cedf2 
		  :count nil
		  :fallthru t)
	       (test-id)
	       (typecase test-id
		  ;;Intercept suite
		  (emt:test-ID:e-n:suite
		     ;;A reached-point for counting invocations.
		     (emt:testpoint tp:798212b4-1abe-4779-beb1-baf53ff39a8c
			())
		     ;;Don't try to explore its clauses, return
		     ;;instead.
		     (throw 'emt:testpoint:tag-return nil))
		  ;;Let library fall thru to handler
		  (emt:test-ID:e-n:library:elisp-load t)
		  ;;We don't expect to see any other types of
		  ;;explores.
		  (t
		     (error "This test shouldn't reach here"))))))
      )
   
   
   )

;;;_  . Type `emt:test:top-data' (Obsolete?)
(defstruct emt:test:top-data
   ""
   (report-func () :type (satisfies #'functionp)))


;;;_   , Examples
;;None yet.
;;;_  . emt:test:explore-clause

(defun emt:test:explore-clause (clause func)
   ""
   (let
      (
	 ;;(emt:trace:current-event-list ())
	 (emt:testral:events-seen (emt:testral:create))
	 ;;Only for problems that manifest right here, not lower down.
	 (badnesses '()))
      

      (emt:test:destructure-clause clause
	 (let
	    (
	       (emt:trace:properties props)
	       ;;Parameterize the list of surrounders.
	       ;;`emt:test:trap-errors' is no longer used here, but it
	       ;;may yet be used again.
	       (form-1
		  ;;(emt:tester:surround form '(emt:test:trap-errors))
		  form
		  ))
	    (condition-case err
	       (eval form-1)
	       (error
		  (push
		     (make-emt:testral:error-raised
			:err err
			:badnesses '(ungraded))
		     emt:testral:events-seen)
		  (push
		     'ungraded
		     badnesses)))))
      
      (make-emt:testral:suite
	 :contents
	 (make-emt:testral:note-list
	    :notes
	    ;;Reverse the note list so it's in the order that it
	    ;;occured in.
	    (nreverse emt:testral:events-seen))
	 ;;Need to acquire this.  At least errors that we
	 ;;handle here - which may be just overall abort.
	 ;;See the call to `emt:test:trap-errors'
	 :badnesses badnesses
	 ;;$$WRITEME Use `emt:trace:properties' for this?  But change
	 ;;its name?  (And watch the scoping)
	 :info '())))

;;;_   , Tests
(rtest:deftest emt:test:explore-clause
   
   ;;Check that it stores top-level badnesses.
   ;;Check that it stores top-level info.

   )

;;;_  . emt:test:explore-one
(defun emt:test:explore-one (next func)
   ""
   (let*
      (
	 (test-id  ;;This is now of type `emt:test-ID:e-n' and
	    ;;contains all the relevant info.
	    (emt:test:pending-id next))
	 ;;Passed around, but only used a little, and then only as a cache.
	 (props
	    (emt:test:pending-properties next))

	 (one-report
	    (emt:testpoint tp:a084136e-8f02-49a5-ac0d-9f65509cedf2
	       (test-id)  ;;No more `e-n'
	       (typecase test-id  ;;Was e-n, which went away.
		  (emt:test-ID:e-n:form
		     (list
			test-id
			'("literal form") ;;Punt the name for now
			(emt:test:explore-clause
			   (emt:test-ID:e-n:form-test-form test-id) 
			   func)))
		  
		  (emt:test-ID:e-n:indexed-clause
		     (let*
			(
			   (suite-sym 
			      (emt:test-ID:e-n:indexed-clause-suite-sym
				 test-id))
			   (index
			      (emt:test-ID:e-n:indexed-clause-clause-index
				 test-id)))
			
			(list
			   test-id
			   (list (format "Clause %d" index))
			   (emt:test:destructure-suite suite-sym
			      (emt:test:explore-clause 
				 (nth index clause-list)
				 func)))))

		  (emt:test-ID:e-n:suite
		     (let* 
			((suite-sym
			    (emt:test-ID:e-n:suite-suite-ID test-id))
			   (path
			      (list (symbol-name suite-sym))))
			(emt:test:destructure-suite suite-sym
			   (let
			      ((n 0)
				 (list-to-run '()))
			      (dolist (clause clause-list)
				 (let
				    ((test-id
					(make-emt:test-ID:e-n:indexed-clause
					   :clause-index n
					   :suite-sym suite-sym)))
				    (push test-id list-to-run)
				    (push 
				       (make-emt:test:pending
					  :id test-id 
					  :path-prefix path
					  ;;Each clause has the
					  ;;properties of the suite
					  ;;(and for now, only those)
					  :properties props)
				       emt:test-finder:pending-list)
				    (incf n)))

			      (list
				 test-id
				 path
				 (make-emt:testral:suite
				    :contents 
				    (make-emt:testral:runform-list
				       :els list-to-run)
				    :badnesses '() ;;Punt - anyways, only
				    ;;meaningful if it crapped out right
				    ;;here.
				    :info '() ;;Punt info for now.
				    ))))))
		
		  (emt:test-ID:e-n:library:elisp-load
		     (let* 
			(  (lib-sym
			      (emt:test-ID:e-n:library:elisp-load-load-name test-id))
			   (suite-list
			      (emt:test:lib-sym->suites lib-sym))
			   (path
			      (list "library" (symbol-name lib-sym)))
			   (list-to-run '()))
			
			(dolist (suite-sym suite-list)
			   (let
			      ((test-id
				  (make-emt:test-ID:e-n:suite
				     :suite-ID suite-sym)))
			      (push test-id list-to-run)
			      (push 
				 (make-emt:test:pending
				    :id test-id 
				    :path-prefix path
				    ;;For now, libraries have no
				    ;;properties. 
				    :properties ())
				 emt:test-finder:pending-list)))
			(list
			   test-id
			   path
			   (make-emt:testral:suite
			      :contents list-to-run
			      :badnesses '() ;;Punt - only if it crapped
			      ;;out right here.
			      :info '()	;;Punt info for now.
			      ))))
		  
		  ;;Tell receiver about this tester
		  (emt:test-ID:e-n:hello
		     (list
			test-id
			(list "Emtest tester")
			(make-emt:testral:test-runner-info
			   :name "Emtest"
			   :explore-methods-supported
			   (mapcar #'car emt:test-finder:conversions))))

		  (t
		     (list
			(make-emt:test-ID:e-n:invalid)
			(list "Bad explore type")
			(make-emt:testral:suite
			   :contents '()
			   ;;TEMPORARY.  This isn't the right form in the
			   ;;long term.
			   :badnesses '((error 
					   "Unrecognized internal explore type"))
			   :info '() ;;Punt info for now.
			   :method-relaunch ""))

		     )))))

      (funcall func
	 (make-emt:testral:report
	    :testrun-id "0" ;;Punt
	    :tester-id "0" ;;Punt
	    :test-id-prefix (emt:test:pending-path-prefix next)
	    :suites 
	    (list one-report)))))

;;;_   , Test helpers
(defun emt:test:th:explore-one (form callback &optional testrun-id)
   ""
   ;;Test thru top call for now.
   (emt:test-finder:top 
      (make-emt:test-ID:e-n:form
	 :test-form form)
      (list "form")
      (or testrun-id "0")
      callback))

;;;_   , Tests

(put 'emt:test:explore-one 'rtest:test-thru
   'emt:test-finder:top)

;;;_  . emt:test:get-properties
;;Yet to be used.
(defun emt:test:get-properties (prop-sym)
   ""
   
   (let
      ((cell (assoc prop-sym emt:trace:properties)))
      (when cell
	 (second cell))))

;;;_   , Tests
(rtest:deftest emt:test:get-properties

   (  "Situation: A test is defined with properties.
That test is now being run.
Behavior: `emt:test:get-properties' returns the relevant property."
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    (props (db-id "my-db")(example-dir "examples/"))
	    ("Docstring" 
	       (emt:testpoint tp:531a913c-aa10-4730-9be5-30c1cb02b7c4
		  ()
		  t)))
	 
	 (emt:testpoint:eval
	    (emt:test:th:run-suite 'dummy-sym #'ignore)
	    (tp:531a913c-aa10-4730-9be5-30c1cb02b7c4
	       ()
	       (progn
		  (assert
		     (equal
			(emt:test:get-properties 'db-id)
			"my-db")
		     t)
		  (assert
		     (equal
			(emt:test:get-properties 'example-dir)
			"examples/")
		     t))))
	 t))
   
   )
;;;_  . emt:test-finder:conversions (Obsolete)
'  ;;Obsolete, now these are very external to here
(defconst emt:test-finder:conversions 
   (list
      (list "hello"
	 #'(lambda (args)
	      (make-emt:test-ID:e-n:hello)))

      (list "run"
	 #'(lambda (args)
	      (make-emt:test-ID:e-n:suite
		 :suite-ID args)))
      
      (list "literal"
	 #'(lambda (args)
	      (make-emt:test-ID:e-n:form
		 ;;Assume it's in native form
		 (first args))))
      )
   "Alist from explore-method name to constructor function.
Each entry should be
 * A string naming an explore-method.
 * A quoted function taking one arg, returning an `emt:test-ID'.
   THAT'S TEMPORARY.  Later these will return an `emt:test:pending'.
" )

;;;_  . emt:test-finder:convert (Obsolete)
;;We no longer need internal conversions
'
(defalias 'emt:test-finder:convert 'identity)
'(defun emt:test-finder:convert (what-to-run)
   "Convert WHAT-TO-RUN to an pending object.
WHAT-TO-RUN must be a `emt:testral:explore-id'.
Pending objects are used internally."
   (check-type what-to-run emt:testral:explore-id)
   (let*
      (  (explore-method (car what-to-run))
	 (args (cdr what-to-run))
	 (cell (assoc explore-method emt:test-finder:conversions)))
      (when cell
	 (funcall (second cell) args))))


;;;_  . emt:test-finder:top
(defun emt:test-finder:top (what-to-run path-prefix testrun-id report-cb)
   ""
   
   (let
      (  (emt:test-finder:pending-list ()))

      (push
	 (make-emt:test:pending
	    :id what-to-run
	    :path-prefix path-prefix
	    :properties ())
	 emt:test-finder:pending-list)

      (while emt:test-finder:pending-list
	 ;;Careful: `pop' seems to have a problem if called in
	 ;;something that sets the value of the list, as
	 ;;`emt:test:explore-one' sometimes did.
	 (let
	    ((next (pop emt:test-finder:pending-list)))
	    ;;$$PASS parms so it can ct what it returns.  testrun-id.
	    (emt:test:explore-one next report-cb)))))

;;;_   , Tests

(rtest:deftest emt:test-finder:top

   (  "Shows: It passes callback an `emt:testral:report'."
      (progn
	 (emt:test:th:explore-one '(error "An example error") 
	    #'(lambda (report)
		 (check-type report emt:testral:report)
		 (let
		    ((emt:deep-type:use t))
		    (check-type report emt:testral:report))))
	 t))


   '  ;;Obsolete.  This no longer demonstrates anything useful.
   (  "Param: A test-ID that only indicates a form.
Response: Get one result."
      (let
	 ((results ()))
	 (emt:test-finder:top
	    (make-emt:test-ID:e-n:form
	       :test-form '("Docstring" ()))
	    '()
	    "0"
	    ;;$$FIXME This is shaky.  Instead, gather results and
	    ;;match patterns.
	    #'(lambda (x)
		 (push x results)))
	 (assert
	    (equal (length results) 1))
	 ;;$$WRONG TYPE
	 '(emt:deep-type:check (car results) emt:result-group)
	 t))
   


   ;;This could do more examples.
   '  ;;Don't try exploring the result here.
   (  "Situation: The form contains a `should' that fails.
Response: Summarizer reports a single failure."
      (progn) ;;Test-form
      )

   ;;$$ Situation: The form contains an examples loop
   ;;Each iteration is reported separately.

   
   '
   (  "Situation: There is a suite.
Param: TEST-ID indicates to explore that suite next.
Behavior: The whole suite is run.
Response: Gets as many results as there are clauses in the suite."
      (progn) ;;Test-form
      )

   )


;;;_. Footers
;;;_ , Provides

(provide 'tester/tester)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/tester.el ends here
