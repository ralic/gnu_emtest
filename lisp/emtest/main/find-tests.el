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
   ;;The integer is the lowest score it accepted last time it ran.
   ;;nil if none.
   (has-run () 
      :type (repeat emthow))
   (min-score 0 :type integer)
   testrun-id
   prefix)


;;;_ , Run tests
;;;_  . emtt:explore-one
(defun emtt:explore-one (explorable report-f testrun)
   ""
   (let* 
      (	    
	 (test-id
	    (emtt:explorable->how-to-run explorable))
	 (has-run
	    (member test-id (emt:testrun->has-run testrun)))
	 (score (emt:ind:get-score test-id)))

      (when (not has-run)
	 (push test-id (emt:testrun->has-run testrun))
	 (let*
	    (
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
	       (funcall (emt:exps:get-func test-id)
		  test-id props path local-report-f))))))

;;;_  . emt:report-nosched
(defun emt:report-nosched (report-cb testrun count suites)
   "Report SUITES as results"
   (funcall report-cb
      (emt:testral:make-report
	 :newly-pending count
	 :testrun-id testrun-id
	 :test-id-prefix prefix
	 :suites suites)))
;;;_  . emt:report
(defun emt:report (testrun report-cb testrun-id suites tests &optional prefix)
   "Report SUITES as results and schedule TESTS to run"
   (let ((count 0))
      (dolist (explorable tests)
	 (let*
	    ((test-id
		(emtt:explorable->how-to-run explorable))
	       (score (emt:ind:get-score test-id)))
	    (when (>= score (emt:testrun->min-score testrun))
	       (incf count)
	       (push explorable
		  (emt:testrun->pending testrun)))))
      (emt:report-nosched report-cb testrun count suites)))

;;;_  . emtt:test-finder:top
(defun emtt:test-finder:top (what-to-run path-prefix testrun-id
			       report-cb &optional min-score)
   "Explore WHAT-TO-RUN, sending its results to REPORT-CB"
   
   (let*  
      (  
	 (testrun (emt:make-testrun :min-score (or min-score 0)))
	 ;; Poor-man's closures.
	 (report-f
	    `(lambda (suites tests &optional prefix)
		(funcall #'emt:report ',testrun #',report-cb ,testrun-id 
		   suites tests prefix))))
      
      ;;Enqueue the root test. 
      (push
	 (emtt:make-explorable
	    :how-to-run  what-to-run
	    :prestn-path path-prefix
	    :properties ())
	 (emt:testrun->pending testrun))
      (emt:report-nosched report-cb testrun 1 '())
      
      ;;Loop thru the pending list.
      (while
	 (emt:testrun->pending testrun)
	 ;;Careful: `pop' seems to have a problem if called in
	 ;;something that sets the value of the list, as
	 ;;`emtt:explore-one' sometimes did.
	 (let*
	    ((next (pop (emt:testrun->pending testrun))))
	    (emtt:explore-one next report-f testrun)))))


;;;_ , Launch tests
;;;_  . Counter

(defvar emt:lch:testrun-counter 0 
   "A counter used to make testrun-id." )

;;;_  . emt:lch:run
(defun emt:lch:run (what-to-run &optional prefix receiver)
   ""
   (emtt:test-finder:top 
      what-to-run 
      prefix  ;;Default is the empty list.
      (prin1-to-string (incf emt:lch:testrun-counter))
      (or receiver emtl:receiver-f)))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/find-tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/main/find-tests.el ends here
