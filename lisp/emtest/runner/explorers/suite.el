;;;_ emtest/runner/explorers/suite.el --- Suite explorer for Emtest

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

(require 'emtest/common/testral-types)
(require 'emtest/runner/define)
(require 'emtest/runner/keepup)
(require 'emtest/common/result-types)


;;;_. Body

;;;_ , emtt:explore-suite
(defun emtt:explore-suite (test-id props-unused path report-f)
   ""
   (let* 
      (
	 (suite-sym
	    (emthow:suite->suite-ID test-id)))
      
      (emtd:update-for-sym suite-sym)
      (emtd:destructure-suite-3 suite-sym
	 (let
	    (  
	       (rv-list-to-run '()))
	    (dotimes (n (length clause-list))
	       (push  
		  (emtt:make-explorable
		     :how-to-run
		     (emthow:make-indexed-clause
			:clause-index n
			:suite-sym suite-sym)
		     :prestn-path 
		     (append 
			path
			(list (format "Clause %d" n)))
		     ;;Each clause has the properties of the suite
		     ;;(and for now, only those).  `props' comes from
		     ;;`emtd:destructure-suite-3', not from arglist.
		     :properties props)
		  rv-list-to-run))

	    (funcall report-f
	       (emt:testral:make-suite
		  :contents 
		  (emt:testral:make-runform-list
		     :els (reverse rv-list-to-run))
		  :badnesses '() ;;Punt - anyways, only
		  ;;meaningful if it crapped out right
		  ;;here.
		  ;;Punt info for now.
		  :info '())
	       (reverse rv-list-to-run))))))
;;;_ , Getting test suites indirectly.

;;$$WRITE ME Also get tests indirectly.  Maybe be replaced by just
;;allowing symbols as clauses.  This is waiting for a means of keeping
;;tests from being run twice in one testrun.

'(or (get symbol 'emtt:test-thru) symbol)

;;;_ , Insinuate
;;Autoloads that put this on a list:

'(#'emthow:suite-p #'emtt:explore-suite "suite")


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/explorers/suite)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/explorers/suite.el ends here
