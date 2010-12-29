;;;_ tester/launch/testhelp.el --- Testhelp for tester/launch.el

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

(require 'emtest/runner/launch)  ;;launch is not actually used in here.
(require 'emtest/runner/tester)
(require 'emtest/testhelp/misc)
(require 'emtest/testhelp/tagnames)
(require 'emtest/common/run-types)

;;;_. Body
;;;_  . Functions

;;;_  . emtt:th:run-suite
(defun emtt:th:run-suite (suite-sym callback)
   "Run the test suite associated with SUITE-SYM.
Results are passed to function CALLBACK."
   
   (emtt:test-finder:top
      ;;$$UPDATE ME - will need to change what it makes.  

      ;;$$REFACTOR ME Would like to use `emtl:run-suite'.  Perhaps
      ;;these can process arglist and then apply, and `emtl:run-suite'
      ;;etc can take all the args.
      (emthow:make-suite
	 :suite-ID suite-sym) 
      '()
      "0" 
      callback))

;;;_ , emtt:ts:run-test
(defun emtt:ts:run-test (test-form callback &optional prefix testrun-id)
   "Run TEST-FORM as a test.
Results are passed to function CALLBACK.
NB, TEST-FORM is a *test-form*, which is a list, usually nil
followed by a form."

   ;;Validate that form is right.  

   ;;$$IMPROVE ME There should be an explicit type for this, in
   ;;test-support for emt-define.
   (check-type test-form (list t t))

   (emtt:test-finder:top 
      (emthow:make-form
	 :test-form test-form)
      (or prefix (list "test-form"))
      (or testrun-id "0")
      callback))

;;;_ , emtl:th:hello
(defun emtl:th:hello (callback &optional prefix testrun-id)
   ""
   
   (emtt:test-finder:top 
      (emthow:make-hello)
      (or prefix (list ""))
      (or testrun-id "0")
      callback))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/launch/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/launch/testhelp.el ends here
