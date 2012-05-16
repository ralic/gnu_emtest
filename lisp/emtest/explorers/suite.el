;;;_ emtest/explorers/suite.el --- Suite explorer for Emtest

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

(require 'emtest/editing/lisp)
(require 'emtest/main/define)
(require 'emtest/main/find-tests)
(require 'emtest/support/keepup)
(require 'emtest/types/run-types)
(require 'emtest/types/testral-types)

;;;_. Body
;;;_ , Launchers
;;;_  . emt:xp:suite:run
(defun emt:xp:suite:run (suite-sym &optional arg)
   "Run the test suite associated with SUITE-SYM."
   (emt:lch:run 
      `(suite ,suite-sym)
      (emt:lch:get-prop-list arg)
      (list (format "Suite %s" suite-sym))))

;;;_  . emtest:defun-at-point
;;;###autoload
(defun emtest:defun-at-point (arg)
  "Run tests on the function or suite under point.

If prefix ARG is non-nil, eval it first.

Does nothing if the buffer is not in a known lisp mode."

   (interactive "P")
   ;;Only proceed if we know how to run tests
   (when (eq major-mode 'emacs-lisp-mode)

      ;;If `arg', eval that definition first.
      (when arg (eval-defun nil))
      (let
	 ((suite-sym
	     (emt:ed:lisp:suite-sym-at-point)))
	 (check-type suite-sym symbol)
	 (emt:xp:suite:run suite-sym nil))))


;;;_ , Explorer

;;;_ , emt:xp:suite
;;;###autoload
(defun emt:xp:suite (test-id props-unused path report-f)
   ""
   (if (cdr test-id)
      (let* 
	 (
	    (suite-sym
	       (second test-id)))
      
	 (emtd:update-for-sym suite-sym)
	 (emtd:destructure-suite-3 suite-sym
	    (let
	       (  
		  (rv-list-to-run '()))
	       (dotimes (n (length clause-list))
		  (push  
		     (emtt:make-explorable
			:how-to-run
			(emt:t:->how `(indexed-clause ,suite-sym ,n))
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
		     :grade '())
		  (reverse rv-list-to-run)))))

      ;; $$IMPROVE ME:  List suites that we know about.
      (funcall report-f
	 (emt:testral:make-suite
	    :contents 
	    (emt:testral:make-note-list
	       :notes 
	       (list
		  (emt:testral:make-note
		     :id 	"0"
		     :parent-id nil
		     :grade     nil
		     :relation 'trace
		     :governor 'doc
		     :value    '("Listing all suites is not implemented"))))
	    :grade nil)
	 '())))

;;;_ , Getting test suites indirectly.

;;$$WRITE ME Also get tests indirectly.  Maybe be replaced by just
;;allowing symbols as clauses.  This is waiting for a means of keeping
;;tests from being run twice in one testrun.

'(or (get symbol 'emtt:test-thru) symbol)

;;;_ , Insinuate
;;;###autoload (eval-after-load 'emtest/main/all-explorers
;;;###autoload  '(emt:exps:add 'suite #'emt:xp:suite
;;;###autoload  "Suite" nil))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/explorers/suite)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/suite.el ends here
