;;;_ emtest/runner/tester.el --- Emtest main tester file

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

(eval-and-compile
   (when (not (require 'emtest/testhelp/testpoint nil t))
      (defmacro emtp (id args &rest rest) `(progn ,@rest))))

(eval-when-compile
   (require 'cl))
(require 'emtest/common/result-types)
(require 'emtest/common/testral-types)
(require 'emtest/runner/testral)

'  ;;Required for tests, but that's no longer here.
(emt:if-avail
   (require 'emtest/testhelp/deep-type-checker)
   (require 'emtest/testhelp/misc)
   (require 'el-mock)
   (require 'emtest/testhelp/testpoint) ;;`tp' is soft-required above
   ;;but hard-required to run certain tests.
   (require 'emtest/testhelp/eg))
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


;;;_ , Info available to tests (Not used yet)
;;;_  . Type `emtt:top-data' 
(defstruct emtt:top-data
   ""
   (report-func () :type (satisfies #'functionp)))


;;;_   , Examples
;;None yet.
;;;_  . Special variables
;;;_   , emt:trace:properties
;;Only needs to be visible to suite-handling.
(declare (special emt:trace:properties))
;;;_   , emt:testral:*events-seen*
;;This belongs somewhere that both runner and testhelp can see.
(declare (special emt:testral:*events-seen*))
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
			      (  
				 (rv-list-to-run '()))

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
			   ;;See [[id:li6i8qd0xxe0][Refactoring dispatchers]]
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
			   ;;$$REDESIGN ME - we no longer have
			   ;;`emt:test-finder:conversions'.  Use a
			   ;;list?  Are these still strings?  Perhaps
			   ;;the tester defines these and groups them.
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
			   ))
		     )))))

      (funcall func
	 ;;For the format, see [[file:~/projects/emtest/lisp/emtest/common/testral-types.el]]
	 (make-emt:testral:report
	    :testrun-id "0" ;;Punt
	    :tester-id "0" ;;Punt
	    :test-id-prefix (emtt:pending-path-prefix next)
	    :suites 
	    (list one-report)))))

;;;_  . Helper emtt:lib-sym->suites

(defun emtt:lib-sym->suites (lib-sym)
   ""
   (let*
      (
	 (lib-data (assoc lib-sym load-history))
	 ;;Or could (remove* :test-not)
	 ;;$$CHANGE ME  Also allow the lib's symbol as a test-suite
	 ;;symbol.
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

(provide 'emtest/runner/tester)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/tester.el ends here
