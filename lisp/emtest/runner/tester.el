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
(progn
   (eval-when-compile
      (require 'emtest/testhelp/testpoint/requirer))
   (emtp:require))
(eval-when-compile
   (require 'cl))
(require 'emtest/common/result-types)
(require 'emtest/common/testral-types)
(require 'emtest/runner/testral)
(require 'emtest/runner/define)
(require 'emtest/runner/explorers/all)

;;$$OBSOLETE 
;;(require 'emtest/runner/explorers/clause)
;;(require 'emtest/runner/explorers/suite)
;;(require 'emtest/runner/explorers/library)

;;;_. Body

;;;_ , Constants
(defconst emtt:version "4.4" 
   "Current version of Emtest" )

;;;_ , Info available to tests (Not used yet)
;;;_  . Type `emtt:top-data'
;;$$RETHINK ME
(defstruct (emtt:top-data
	    (:constructor emtt:make-top-data)
	    (:conc-name emtt:top-data->))
   ""
   (report-func () :type (satisfies #'functionp)))

;;;_ , Data

;;;_  . Pending list
(defvar emtt:test-finder:pending-list () 
   "List of pending tests (etc) to explore.
Each one must be a `emtt:explorable'" )


;;;_ , Run tests
;;;_  . emtt:explore-one
(defun emtt:explore-one (explorable func report-f)
   ""
   ;;(check-type test-id emthow)
   (let*
      (
	 (test-id
	    (emtt:explorable->how-to-run explorable))
	 (props
	    (emtt:explorable->properties explorable))
	 (path
	    (emtt:explorable->prestn-path explorable))
	 (local-report-f
	    `(lambda (report &optional tests prefix)
		(funcall 
		   ,report-f
		   (list 
		      (list ,explorable nil report))
		   tests
		   prefix))))

      ;;$$IMPROVE ME condition-case this and report bad test if we
      ;;miss.
      (emtp tp:a084136e-8f02-49a5-ac0d-9f65509cedf2
	 (test-id)
	 (typecase test-id
	    (emthow:form
	       (emtt:explore-literal-clause
		  test-id props path local-report-f))

	    (emthow:indexed-clause
	       (emtt:explore-indexed-clause
		  test-id props path local-report-f))
		  
	    (emthow:suite
	       (emtt:explore-suite 
		  test-id props path local-report-f))
		  
	    (emthow:library:elisp-load
	       (emtt:explore-library 
		  test-id props path local-report-f))
		  
	    ;;Tell receiver about this tester
	    (emthow:hello
	       (funcall local-report-f
		  (emt:testral:make-test-runner-info
		     :name "Emtest"
		     :version emtt:version
		     :explore-methods-supported
		     (mapcar #'car emtt:test-finder:method-list))))

	    ;;Fallback case
	    (t
	       ;;Not clear that this answers at a sufficiently
	       ;;high level.  It must indicate that there's no
	       ;;such method.
	       (funcall local-report-f
		  (emt:testral:make-suite
		     :contents 
		     (emt:testral:make-note-list
			:notes 
			(list
			   (emt:testral:make-error-raised
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
		     ;;Punt info for now.
		     :info '())))))))

;;;_  . emtt:test-finder:top

;;$$RECONSIDER MY INTERFACE what-to-run may become an
;;`emtt:explorable' and be pushed as itself.  Callers must take notice.
(defun emtt:test-finder:top (what-to-run path-prefix testrun-id report-cb)
   ""
   
   (let*
      (  (emtt:test-finder:pending-list ())
	 ;; Poor-man's closures.
	 (report-f
	    `(lambda (suites tests &optional prefix)
		(when tests
		   (callf2 append 
		      tests
		      emtt:test-finder:pending-list))
		(funcall #',report-cb
		   (emt:testral:make-report
		      :run-done-p nil ;;$$OBSOLESCENT
		      :testrun-id ,testrun-id
		      :tester-id "" ;;$$OBSOLESCENT
		      :test-id-prefix prefix
		      :suites suites)))))

      ;;Enqueue the root test. 
      (funcall report-f
	 '()
	 (list
	    (emtt:make-explorable
	       :how-to-run  what-to-run
	       :prestn-path path-prefix
	       :properties ())))
      
      ;;Loop thru the pending list.
      (while emtt:test-finder:pending-list
	 ;;Careful: `pop' seems to have a problem if called in
	 ;;something that sets the value of the list, as
	 ;;`emtt:explore-one' sometimes did.
	 (let
	    ((next (pop emtt:test-finder:pending-list)))
	    (emtt:explore-one next report-cb report-f)))

      ;;$$OBSOLESCENT This will be replaced by reports of how many are
      ;;enqueued. 
      (funcall report-cb
	 (emt:testral:make-report
	    :run-done-p t
	    :testrun-id testrun-id
	    :tester-id "Emtest"
	    :test-id-prefix '()	    
	    :suites '()))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/tester)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/tester.el ends here
