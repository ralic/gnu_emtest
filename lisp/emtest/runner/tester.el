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

;;;_ , Info available to tests (Not used yet)
;;;_  . Type `emtt:top-data'
;;$$RETHINK ME
(defstruct (emtt:top-data
	    (:constructor emtt:make-top-data)
	    (:conc-name emtt:top-data->))
   ""
   (report-func () :type (satisfies #'functionp)))


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
      ;;$$IMPROVE ME Find it in `emtt:test-finder:method-list' - try
      ;;each predicate.  Only fallback will remain here.  Use
      ;;`emtt:get-explore-func' in all.el, which is yet to be written.
      (emtp tp:a084136e-8f02-49a5-ac0d-9f65509cedf2
	 (test-id)
	 (funcall (emtt:get-explore-func test-id)
	    test-id props path local-report-f)
	 
	 '
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

	    ;;Fallback case - encap me.
	    (t
	       ;;Not clear that this answers at a sufficiently
	       ;;high level.  It must indicate that there's no
	       ;;such method.
	       (funcall local-report-f
		  (emt:testral:make-suite
		     :contents nil
		     ;;Actual form is TBD.
		     :grade 
		     (emt:testral:make-grade:ungraded
			:contents
			"Unrecognized internal explore type"))))))))

;;;_  . emtt:test-finder:top

;;$$RECONSIDER MY INTERFACE what-to-run may become an
;;`emtt:explorable' and be pushed as itself.  Callers must take notice.
(defun emtt:test-finder:top (what-to-run path-prefix testrun-id report-cb)
   ""
   
   (let*  
      (  (emtt:pending-list (list '()))
	 ;; Poor-man's closures.
	 (report-f
	    `(lambda (suites tests &optional prefix)
		(when tests
		   (callf2 append 
		      tests
		      (car ',emtt:pending-list)))
		(funcall #',report-cb
		   (emt:testral:make-report
		      :newly-pending (length tests)
		      :testrun-id ,testrun-id
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
      (while (car emtt:pending-list)
	 ;;Careful: `pop' seems to have a problem if called in
	 ;;something that sets the value of the list, as
	 ;;`emtt:explore-one' sometimes did.
	 (let
	    ((next (pop (car emtt:pending-list))))
	    (emtt:explore-one next report-cb report-f)))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/tester)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/tester.el ends here
