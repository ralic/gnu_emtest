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
   (when (not (require 'tester/testhelp/testpoint nil t))
      (defmacro emtp (id args &rest rest) `(progn ,@rest))))

(eval-when-compile
   (require 'cl))
(require 'common/result-types)
(require 'common/testral-types)
(require 'tester/testral)


(emt:if-avail
   (require 'tester/testhelp/deep-type-checker)
   (require 'tester/testhelp/misc)
   (require 'el-mock)
   (require 'tester/testhelp/testpoint) ;;`tp' is soft-required above
   ;;but hard-required to run certain tests.
   (require 'tester/testhelp/eg))
;;;_. Body


;;;_ , test surrounders (Not used yet)
;;;_  . List of the usual test-protectors

(defconst emtts:usual-test-protectors 
   '(
       test-message-trap
       test-error-trap
       save-window-excursion
       with-temp-buffer
       ;;Add other *standard* ones here.  This should not be
       ;;customized.
       )
   "" )
;;;_  . Test helper

;;Assumes tests-own-args can have the form of an empty list.
(defconst emtts:thd:simplest-tests-own-args () "" )

;;;_  . Place form within test-protectors

(defun emtts:surround (form protectors)
   ""
   (let
      ((rv-protectors (reverse protectors)))
      
      (dolist (i rv-protectors form)
	 (setq form (list i form)))))

;;;_   , Tests

(rtest:deftest emtts:surround

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

(defun emtts:get-extra-protectors (tests-own-args)
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
(rtest:deftest emtts:get-extra-protectors

   ;;Test of emt:tester:get-extra-protectors.  Requires a bogus
   ;;tests-own-args object -- but syntax is immature right now.
   (  "Situation: protectors is not a list
Response: Error."
      (progn) ;;Test-form
      )
   )

;;;_  . Figure out surrounders (Not used (yet?))

(defun emtts:get-surrounders (tests-own-args debug)
   ""
   (append
      emtts:usual-test-protectors
      (emtts:get-extra-protectors tests-own-args)
      ;;Debugging, if present, is innermost.
      (if debug
	 '(emtts:with-debugging)
	 ())))

;;;_   , Tests
'
(rtest:deftest emtts:get-surrounders
   
   
   (  "Demonstrate: emt:tester:get-surrounders returns a list of symbols."
      
      (every
	 #'symbolp
	 (emt:tester:get-surrounders 
	    emtts:thd:simplest-tests-own-args
	    nil)))
   

   (  "Demonstrate: emt:tester:get-surrounders returns a list of symbols."
      
      (every
	 #'symbolp
	 (emt:tester:get-surrounders 
	    emtts:thd:simplest-tests-own-args
	    t)))
   
   ;;Since tester's always-surrounders is not controlled by this, we
   ;;can't test proper contents, but since we just append, should be
   ;;no problem.
   )

;;;_  . Some surrounders

;;;_   , emtts:with-debugging
;;Exists just to make `emtts:get-surrounders' neater.

(defmacro emtts:with-debugging (&rest form)
   ""
   
   `(progn
       (debug) 
       ,@form))

;;;_    . Tests

;;Can't easily automatically test that it in fact debugs.

;;;_   , emtt:trap-errors (May be obsolete or may get used again)

(defmacro emtt:trap-errors (&rest body)
   ""
   
   `(condition-case err
       (progn ,@body)
       ('emt:already-handled
	  ;;For now, do nothing.
	  nil)
       (error 
	  (error "Not handling unexpected errors yet"))))

;;;_    . Tests

(rtest:deftest emtt:trap-errors

   (  "Shows: Works with `emtts:surround'."
      (equal
	 (eval
	    (emt:tester:surround 12 '(emtt:trap-errors)))
	 
	 12))
   
   (  "Situation: Body throws an emt:already-handled error.
Response: (Punt) Marks the event-group aborted.
Adds no error report.
Does not signal error."
      (progn
	 (assert
	    (not
	       (emt:gives-error
		  (emtt:trap-errors
		     (signal 'emt:already-handled ())))))
	 t))
   
   ;;More to add.  See [[id:ca903ca0-bd5d-4985-8cd3-a5a4dd998b5c][]]
   
   )


;;;_ , Info available to tests (Not used yet)
;;;_  . Type `emtt:top-data' 
(defstruct emtt:top-data
   ""
   (report-func () :type (satisfies #'functionp)))


;;;_   , Examples
;;None yet.
;;;_  . emtt:get-properties
;;Yet to be used.
(defun emtt:get-properties (prop-sym)
   ""
   
   (let
      ((cell (assoc prop-sym emt:trace:properties)))
      (when cell
	 (second cell))))

;;;_ , test finder

;;;_  . Pending list
(defvar emt:test-finder:pending-list () "" )
;;;_   , Type spec
(defstruct emtt:pending
   ""
   ;;This is correct: Descendants are of `emt:test-ID:e-n', not of
   ;;this class.
   (id () :type emt:test-ID:e-n)

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
   (emty:check
      emt:test-finder:pending-list
      (list emtt:pending)))


;;;_ , Run tests
;;;_  . emtt:explore-clause

(defun emtt:explore-clause (clause)
   ""
   (let
      (
	 ;;(emt:trace:current-event-list ())
	 (emt:testral:*events-seen* (emt:testral:create))
	 (emt:testral:*parent-id* 0)
	 ;;Counter to make unique IDs.  Although UUIDs are appealing,
	 ;;they are slower to make.
	 (emt:testral:*id-counter* 1)
	 
	 ;;Only for problems that manifest right here, not lower down.
	 (badnesses '()))
      

      (emtt:destructure-clause-3 clause
	 (let
	    (
	       (emt:trace:properties props)
	       ;;Parameterize the list of surrounders.
	       ;;`emtt:trap-errors' is no longer used here, but it
	       ;;may yet be used again.  But it shouldn't be optional.
	       (form-1
		  ;;(emtts:surround form '(emtt:trap-errors))
		  form
		  ))
	    (condition-case err
	       (eval form-1)
	       (error
		  (push
		     (make-emt:testral:error-raised
			:err err
			:badnesses '(ungraded))
		     emt:testral:*events-seen*)
		  (push
		     'ungraded
		     badnesses)))))
      
      (make-emt:testral:suite
	 :contents
	 (make-emt:testral:note-list
	    :notes
	    ;;Reverse the note list so it's in the order that it
	    ;;occured in.
	    (nreverse emt:testral:*events-seen*))
	 ;;Need to acquire this.  At least errors that we
	 ;;handle here - which may be just overall abort.
	 ;;See the call to `emtt:trap-errors'
	 :badnesses badnesses
	 ;;$$WRITEME Use `emt:trace:properties' for this?  But change
	 ;;its name?  (And watch the scoping)
	 :info '())))

;;;_  . emtt:explore-one
(defun emtt:explore-one (next func)
   ""
   (let*
      (
	 (test-id  ;;This is now of type `emt:test-ID:e-n' and
	    ;;contains all the relevant info.
	    (emtt:pending-id next))
	 ;;Passed around, but only used a little, and then only as a cache.
	 (props
	    (emtt:pending-properties next))

	 (one-report
	    (emtp tp:a084136e-8f02-49a5-ac0d-9f65509cedf2
	       (test-id)  ;;No more `e-n'
	       (typecase test-id  ;;Was e-n, which went away.
		  (emt:test-ID:e-n:form
		     (list
			test-id
			'("literal form") ;;Punt the name for now
			(emtt:explore-clause
			   (emt:test-ID:e-n:form-test-form test-id))))
		  ;;`clause-list' from where?
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
			   (emtt:destructure-suite-3 suite-sym
			      (emtt:explore-clause 
				 (nth index clause-list))))))

		  (emt:test-ID:e-n:suite
		     (let* 
			((suite-sym
			    (emt:test-ID:e-n:suite-suite-ID test-id))
			   (path
			      (list (symbol-name suite-sym))))
			(emtt:destructure-suite-3 suite-sym
			   (let
			      (  ;;(n 0)  ;;$$REMOVEME Obsolete
				 (rv-list-to-run '()))

			      '  ;;$$REMOVEME Obsolete
			      (dolist (clause clause-list)
				 (let
				    ((test-id
					(make-emt:test-ID:e-n:indexed-clause
					   :clause-index n
					   :suite-sym suite-sym)))
				    (push test-id rv-list-to-run)
				    (push 
				       (make-emtt:pending
					  :id test-id 
					  :path-prefix path
					  ;;Each clause has the
					  ;;properties of the suite
					  ;;(and for now, only those)
					  :properties props)
				       emt:test-finder:pending-list)
				    (incf n)))

			      (dotimes (n (length clause-list))
				 (push  
				    (make-emt:test-ID:e-n:indexed-clause
				       :clause-index n
				       :suite-sym suite-sym)
				    rv-list-to-run))
			      (dolist (test-id rv-list-to-run)
				 ;;Reverses the already-reversed order
				 ;;of rv-list-to-run, so pending-list
				 ;;is explored in the expected order.
				 (push 
				    (make-emtt:pending
				       :id test-id 
				       :path-prefix path
				       ;;Each clause has the
				       ;;properties of the suite
				       ;;(and for now, only those)
				       :properties props)
				    emt:test-finder:pending-list))
			      
			      (list
				 test-id
				 path
				 (make-emt:testral:suite
				    :contents 
				    (make-emt:testral:runform-list
				       :els (reverse rv-list-to-run))
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
			      (emtt:lib-sym->suites lib-sym))
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
				 (make-emtt:pending
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
			;;$$DESIGNME - this design is ugly, has parts
			;;that overlap.
			(make-emt:testral:suite
			   :contents 
			   (make-emt:testral:note-list
			      :notes 
			      (list
				 (make-emt:testral:error-raised
				    :err 
				    '(error 
					"Unrecognized internal explore type")
				    :badnesses 
				    '((ungraded 'error 
					 "Unrecognized internal explore type"))
				    )))
			   ;;Actual form is TBD.
			   :badnesses 
			   '((ungraded 'error 
				"Unrecognized internal explore type"))
			   :info '() ;;Punt info for now.
			   :method-relaunch ""))

		     )))))

      (funcall func
	 (make-emt:testral:report
	    :testrun-id "0" ;;Punt
	    :tester-id "0" ;;Punt
	    :test-id-prefix (emtt:pending-path-prefix next)
	    :suites 
	    (list one-report)))))

;;;_  . emt:test-finder:top
(defun emt:test-finder:top (what-to-run path-prefix testrun-id report-cb)
   ""
   
   (let
      (  (emt:test-finder:pending-list ()))

      (push
	 (make-emtt:pending
	    :id what-to-run
	    :path-prefix path-prefix
	    :properties ())
	 emt:test-finder:pending-list)

      (while emt:test-finder:pending-list
	 ;;Careful: `pop' seems to have a problem if called in
	 ;;something that sets the value of the list, as
	 ;;`emtt:explore-one' sometimes did.
	 (let
	    ((next (pop emt:test-finder:pending-list)))
	    ;;$$PASS parms so it can ct what it returns.  testrun-id.
	    (emtt:explore-one next report-cb)))))


;;;_. Footers
;;;_ , Provides

(provide 'tester/tester)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/tester.el ends here
