;;;_ emtest/main/find-tests.el --- Emtest main tester file

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
(require 'emtest/types/run-types)
(require 'emtest/types/testral-types)
(require 'emtest/main/all-explorers)
(require 'emtest/support/individual)
;;;_. Body
;;;_ , Structure
(defstruct (emt:testrun
	      (:copier nil)
	      (:conc-name emt:testrun->)
	      (:constructor emt:make-testrun))
   "State data about a given testrun"
   (pending () :type (repeat emtt:explorable))
   (has-run () 
      :type (repeat emthow))
   report-cb
   testrun-id
   (properties () :type (repeat (cons symbol t))))



;;;_ , Run tests
;;;_  . emtt:explore-one
(defun emtt:explore-one (explorable testrun)
   ""
   (let* 
      (	    
	 (test-id
	    (emtt:explorable->how-to-run explorable))
	 (has-run
	    (member test-id (emt:testrun->has-run testrun))))

      (when (not has-run)
	 (push test-id (emt:testrun->has-run testrun))
	 (let*
	    (
	       (props
		  (append
		     (emt:testrun->properties testrun)
		     (emtt:explorable->properties explorable)))
	       (path
		  (emtt:explorable->prestn-path explorable))
	       ;; Poor-man's closure
	       (local-report-f
		  `(lambda (report &optional tests prefix skipped)
		      (funcall 
			 #'emt:report 
			 ',testrun
			 (if report
			    (list 
			       (list ,explorable nil report))
			    ())
			 tests
			 prefix 
			 skipped))))

	    ;;$$IMPROVE ME condition-case this and report bad test if we
	    ;;miss.
	    (emtp tp:a084136e-8f02-49a5-ac0d-9f65509cedf2
	       (test-id)
	       (funcall (emt:exps:get-func test-id)
		  test-id props path local-report-f))))))

;;;_  . emt:report-nosched
(defun emt:report-nosched (testrun count suites &optional prefix)
   "Report SUITES as results"
   (funcall (emt:testrun->report-cb testrun)
      (emt:testral:make-report
	 :newly-pending count
	 :testrun-id (emt:testrun->testrun-id testrun)
	 :test-id-prefix prefix
	 :suites suites)))
;;;_  . emt:report
(defun emt:report (testrun suites tests &optional prefix skipped)
   "Report SUITES as results and schedule TESTS to run"
   (let ((count (- (length tests) (or skipped 0))))
      (when tests
	 (callf2 append tests (emt:testrun->pending testrun)))
      (emt:report-nosched testrun count suites)))
;;;_  . emtt:test-finder:top
;;;###autoload
(defun emtt:test-finder:top 
   (explorable-list testrun-id report-cb properties)
   "Explore WHAT-TO-RUN, sending its results to REPORT-CB"
   (let
      (  
	 (testrun 
	    (emt:make-testrun
	       :pending    (copy-list explorable-list)
	       :report-cb  report-cb
	       :testrun-id testrun-id
	       :properties properties)))

      (emt:report-nosched testrun (length explorable-list) '())
      
      ;;Loop thru the pending list.
      (while
	 (emt:testrun->pending testrun)
	 ;;Careful: `pop' seems to have a problem if called in
	 ;;something that sets the value of the list, as
	 ;;`emtt:explore-one' sometimes did.
	 (let*
	    ((next (pop (emt:testrun->pending testrun))))
	    (emtt:explore-one next testrun)))))


;;;_ , Launch tests
;;;_  . Counter

(defvar emt:lch:testrun-counter 0 
   "A counter used to make testrun-id." )

;;;_  . Convenient property-lists
(defconst emt:lch:proplist:vanilla
   '()
   "Property list for a normal run")
(defconst emt:lch:proplist:restrained 
   '((no-redo-passes t))
   "Property list for a restrained run: No redoing already-passed tests" )
(defconst emt:lch:proplist:alist
   '()
   "Alist of property lists" )
;;;_  . emt:lch:get-prop-list
(defun emt:lch:get-prop-list (&optional restrained)
   "Get a suitable property list.
If RESTRAINED, the property list won't redo tests that passed"
   ;;$$IMPROVE ME If it's neither t nor nil, interactively pick from
   ;;the alist
   (if restrained
      emt:lch:proplist:restrained
      emt:lch:proplist:vanilla))

;;;_  . emt:lch:run
;;;###autoload
(defun emt:lch:run (what-to-run props &optional prefix receiver testrun-id)
   "Run a single test"
   (emtt:test-finder:top
      (list
	 (emtt:make-explorable
	    :how-to-run  what-to-run
	    :prestn-path prefix  ;;Default is the empty list.
	    :properties  '()))
      (or testrun-id
	 (prin1-to-string (incf emt:lch:testrun-counter)))
      (or receiver emtl:receiver-f)
      props))
;;;_  . emt:lch:run-explist
;;$$WRITE ME

;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/find-tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/main/find-tests.el ends here
