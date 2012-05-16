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

(require 'emtest/main/find-tests)
(require 'emtest/testhelp/misc)
(require 'emtest/testhelp/tagnames)
(require 'emtest/types/run-types)

;;;_. Body
;;;_  . Functions

;;;_  . emtt:th:run-suite
(defun emtt:th:run-suite (suite-sym callback)
   "Run the test suite associated with SUITE-SYM.
Results are passed to function CALLBACK."
   
   (emt:lch:run
      `(suite ,suite-sym)
      emt:lch:proplist:vanilla
      '()
      callback
      "0"))


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

   (emt:lch:run
      `(form ,test-form)
      emt:lch:proplist:vanilla
      (or prefix (list "test-form"))
      callback
      (or testrun-id "0")))


;;;_ , emtl:th:hello
'  ;; $$OBSOLETE
(defun emtl:th:hello (callback &optional prefix testrun-id)
   ""
   
   (emt:lch:run
      '(hello)
      emt:lch:proplist:vanilla
      (or prefix (list ""))
      callback
      (or testrun-id "0")))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/launch/all/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/launch/testhelp.el ends here
