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

;;Require these for now, until we set up to insinuate the explorers
;;from their collective autoloads file.
(require 'emtest/runner/explorers/clause)
(require 'emtest/runner/explorers/suite)
(require 'emtest/runner/explorers/library)

;;;_. Body


;;;_ , Info available to tests (Not used yet)
;;;_  . Type `emtt:top-data'
;;$$USE ME
(defstruct (emtt:top-data
	    (:constructor emtt:make-top-data)
	    (:conc-name emtt:top-data->))
   ""
   (report-func () :type (satisfies #'functionp)))

;;;_ , test finder

;;;_  . Pending list
(defvar emt:test-finder:pending-list () 
   "List of pending tests (etc) to explore.
Each one must be a `emtt:explorable'" )


;;;_ , Run tests
;;;_  . emtt:explore-one
(defun emtt:explore-one (next func)
   ""
   ;;(check-type test-id emthow)
   (let*
      (
	 (test-id
	    (emtt:explorable->how-to-run next))
	 (props
	    (emtt:explorable->properties next))
	 (path
	    (emtt:explorable->prestn-path next))
	 (one-report
	    (emtp tp:a084136e-8f02-49a5-ac0d-9f65509cedf2
	       (test-id)
	       (typecase test-id
		  (emthow:form
		     (emtt:explore-literal-clause
			test-id props path))

		  (emthow:indexed-clause
		     (emtt:explore-indexed-clause
			test-id props path))
		  
		  (emthow:suite
		     (emtt:explore-suite test-id props path))
		  
		  (emthow:library:elisp-load
		     (emtt:explore-library test-id props path))
		  
		  ;;Tell receiver about this tester
		  (emthow:hello
		     (list
			nil
			(emt:testral:make-test-runner-info
			   :name "Emtest"
			   ;;:version "4.2"
			   ;;$$COLLECT ME from the explorers.  This
			   ;;will be from a config list, written to by
			   ;;autoload files.
			   :explore-methods-supported
			   (mapcar #'car emt:test-finder:conversions))))

		  ;;Fallback case
		  (t
		     ;;Not clear that this answers at a sufficiently
		     ;;high level.  It must indicate that there's no
		     ;;such method.
		     (list
			nil
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
			   :info '() ;;Punt info for now.
			   ))
		     )))))

      ;;Maybe schedule more.
      (when (first one-report)
	 (callf2 append 
	    (first one-report)
	    emt:test-finder:pending-list))

      (funcall func
	 (emt:testral:make-report
	    :testrun-id "0" ;;Punt
	    :tester-id "0"  ;;Punt
	    :test-id-prefix '()	    ;;Not used by this tester
	    :suites 
	    (list 
	       (list
		  next
		  nil
		  (second one-report)))))))


;;;_  . emtt:test-finder:top

;;$$RECONSIDER MY INTERFACE what-to-run may become an
;;`emtt:explorable' and be pushed as itself.  Callers must take notice.
(defun emtt:test-finder:top (what-to-run path-prefix testrun-id report-cb)
   ""
   
   (let
      (  (emt:test-finder:pending-list ()))
      (push
	 (emtt:make-explorable
	    :how-to-run  what-to-run
	    :prestn-path path-prefix
	    :properties ())
	 emt:test-finder:pending-list)

      (while emt:test-finder:pending-list
	 ;;Careful: `pop' seems to have a problem if called in
	 ;;something that sets the value of the list, as
	 ;;`emtt:explore-one' sometimes did.
	 (let
	    ((next (pop emt:test-finder:pending-list)))
	    ;;$$PASS parms so it can ct what it returns.  testrun-id.
	    (emtt:explore-one next report-cb)))

      (funcall report-cb
	 (emt:testral:make-report
	    :run-done-p t
	    :testrun-id "0" ;;Punt
	    :tester-id "0"  ;;Punt
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
