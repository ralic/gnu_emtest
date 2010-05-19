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
      (defmacro emtp (id args &rest rest) 
	 (declare 
	    (debug (symbolp (&rest form) body)))
	 `(progn ,@rest))))
(eval-when-compile
   (require 'cl))
(require 'emtest/common/result-types)
(require 'emtest/common/testral-types)
(require 'emtest/runner/testral)
(require 'emtest/runner/define)
(require 'emtest/runner/surrounders) ;;Just used by the clause explorer
;;;_. Body


;;;_ , Info available to tests (Not used yet)
;;;_  . Type `emtt:top-data'
;;$$USE ME
(defstruct emtt:top-data
   ""
   (report-func () :type (satisfies #'functionp)))


;;;_ , Special variables
;;;_  . emt:trace:properties
;;This belongs somewhere that both runner and testhelp can see.
;;Right now, it's used for the `emt:persist' testhelp.
;;OBSOLESCENT.  
(declare (special emt:trace:properties))
;;;_  . emt:testral:*events-seen*
;;This belongs somewhere that both runner and testhelp can see.
(declare (special emt:testral:*events-seen*))

;;;_ , test finder

;;;_  . Pending list
(defvar emt:test-finder:pending-list () 
   "List of pending tests (etc) to explore.
Each one must be a `emtt:explorable'" )


;;;_ , Run tests
;;;_  . emtt:explore-one
(defun emtt:explore-one (next func)
   ""
   (let*
      (
	 (test-id  ;;This is now of type `emt:test-ID:e-n'
	    (emtt:explorable->id next))
	 ;;Not used - the one time it's apparently used below is
	 ;;actually something else.
;; 	 (props
;; 	    (emtt:explorable->properties next))
	 ;;This largely uses the original `next' as the launch
	 ;;path-prefix usually shouldn't even be changed.  It should
	 ;;be inserted with the right value, not changed here.
	 (one-report
	    (emtp tp:a084136e-8f02-49a5-ac0d-9f65509cedf2
	       (test-id)
	       (typecase test-id
		  ;;This will be a separate explorer.
		  (emt:test-ID:e-n:indexed-clause
		     (let*
			(
			   (suite-sym 
			      (emt:test-ID:e-n:indexed-clause-suite-sym
				 test-id))
			   (index
			      (emt:test-ID:e-n:indexed-clause-clause-index
				 test-id)))
			(emtt:destructure-suite-3 suite-sym
			      (emtt:explore-clause 
				 (nth index clause-list)))))

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
				    (emtt:make-explorable
				       :id
				       (make-emt:test-ID:e-n:indexed-clause
					  :clause-index n
					  :suite-sym suite-sym)
				       :path-prefix 
				       (append 
					  path
					  (list (format "Clause %d" n)))
				       
				       ;;Each clause has the
				       ;;properties of the suite (and
				       ;;for now, only those)
				       :properties props)
				    rv-list-to-run))
			      (dolist (test-id rv-list-to-run)
				 ;;Reverses the already-reversed order
				 ;;of rv-list-to-run, so pending-list
				 ;;is explored in the expected order.
				 (push 
				    test-id
				    emt:test-finder:pending-list))
			      (make-emt:testral:suite
				    :contents 
				    (emt:testral:make-runform-list
				       :els (reverse rv-list-to-run))
				    :badnesses '() ;;Punt - anyways, only
				    ;;meaningful if it crapped out right
				    ;;here.
				    :info '() ;;Punt info for now.
				    )))))
		
		  (emt:test-ID:e-n:library:elisp-load
		     (let* 
			(  (lib-sym
			      (emt:test-ID:e-n:library:elisp-load-load-name test-id))
			   ;;See [[id:li6i8qd0xxe0][Refactoring dispatchers]]
			   (suite-list
			      (emtt:lib-sym->suites lib-sym))
			   (path
			      (list "library" (symbol-name lib-sym)))
			   (list-to-run
			      (mapcar
				 #'(lambda (suite-sym)
				      (emtt:make-explorable
					 :id
					 (make-emt:test-ID:e-n:suite
					    :suite-ID suite-sym)
					 ;;CHANGED to append lib name.
					 :path-prefix 
					 (append 
					    path
					    (list (symbol-name suite-sym)))
					 ;;For now, libraries have no
					 ;;properties. 
					 :properties ()))
				 suite-list)))
			
			(callf2 append 
			   list-to-run
			   emt:test-finder:pending-list) 
			(make-emt:testral:suite
			   :contents list-to-run
			   :badnesses '() ;;Punt - only if it crapped
			   ;;out right here.
			   :info '() ;;Punt info for now.
			   )))
		  
		  ;;Tell receiver about this tester
		  (emt:test-ID:e-n:hello
		     (make-emt:testral:test-runner-info
			   :name "Emtest"
			   ;;:version "4.1"
			   ;;$$REDESIGN ME - we no longer have
			   ;;`emt:test-finder:conversions'.  Use a
			   ;;list?  Are these still strings?  Perhaps
			   ;;the tester defines these and groups them.
			   :explore-methods-supported
			   (mapcar #'car emt:test-finder:conversions)))

		  (t


		     (if (emt:test-ID:e-n:form-p test-id)
			(emtt:explore-clause
			   (emt:test-ID:e-n:form-test-form test-id))

			;;Not clear that this answers at a sufficiently
			;;high level.  It must indicate that there's no
			;;such method.
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
	    ;;Not used by this tester
	    :test-id-prefix '()
	    :suites 
	    (list 
	       (list
		  next
		  nil
		  one-report))))))

;;;_  . Helper emtt:lib-sym->suites
;;$$MOVE ME And reorganize.  Each how-to-run type should go into its own
;;subdirectory.
;;$$RETHINK ME Should this take symbol or string?  We seemed to
;;convert it and then convert it back.

;;$$CHANGE ME Also offer everything in the /tests library, and try to
;;load it.  Add tests for this behavior.

;;$$CHANGE ME Could also offer tests on every library that this
;;library requires.  Gotta control execution, though.  Don't want to
;;run them too eagerly.  So this would have to return more than just a
;;symbol for each.  In fact, it could become the workhorse for the
;;`emt:test-ID:e-n:library:elisp-load' case
(defun emtt:lib-sym->suites (lib-sym)
   ""
   (let*
      (
	 (lib-path
	    (locate-library
	       (symbol-name lib-sym)))
	 (lib-data (assoc lib-path load-history))
	 ;;$$CHANGE ME  Also allow the lib's symbol as a test-suite
	 ;;symbol.
	 ;;List of symbols.
	 (suites
	    (delq nil
	       (mapcar
		  #'(lambda (x)
		       (let
			  ((sym (emtl:ldhst-el->symbol x)))
			  (when (get sym 'emt:suite) sym)))
		  (cdr lib-data)))))
      
      suites))


;;;_  . emt:test-finder:top

;;$$RECONSIDER MY INTERFACE what-to-run may become an
;;`emtt:explorable' and be pushed as itself.  Callers must take notice.
(defun emt:test-finder:top (what-to-run path-prefix testrun-id report-cb)
   ""
   
   (let
      (  (emt:test-finder:pending-list ()))
      (push
	 (emtt:make-explorable
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
