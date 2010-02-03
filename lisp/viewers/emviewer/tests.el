;;;_ emviewer/tests.el --- Emviewer tests

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: 

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

;; This file may be slightly misnamed.  It is largely combined tests
;; of emviewer and tester.  This proved the most sensible way to test
;; tester.


;;;_ , Requires

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

(require 'emviewer/testhelp) ;;For `emtest:ts:run-test'

;;;_. Body

;;Testing will be mostly separate for the formatters - maybe chewie
;;(and wookie) should support this sort of part comparison directly.
;;A test helper could take (object data formatter
;;persist-id-of-expectation)

;;For overall, we may not need much testing since it's mostly
;;liveness.  But we can use a temp buffer, just be sure the
;;ewoc/wookie/chewie does not outlive it.

;;let `emtest:viewer:emviewer:result-root' and
;;`emtest:viewer:emviewer:chewie' as nil, make a temp buffer for
;;`emtest:viewer:emviewer:report-buffer', then call setup.
;;Then run `emtest:viewer:receive'.
;;This is getting more complex than simply using an ADT instead of globals.

;;$$TESTME.

;;Hard to do regression testing, so manual testing for the moment.
;;TO control the buffer, let `emtest:viewer:emviewer:report-buffer'
;;But that requires altering `emtest:viewer:setup-if-needed', which
;;may not be what is wanted.  But controlling the chewie may make more
;;sense.  A chewie should not be in a temp buffer unless it's temp
;;too.  See above for thoughts.


'
(emtest:viewer:receive
   (emt:eg (type report)(name just-test-runner)))

;;Says OK because we don't summarize badnesses yet.
'  
(emtest:viewer:receive
   (emt:eg (type report)(role original-add)(what-test test-1)))

'
(emtest:viewer:receive
   (emt:eg (type report)(what-test test-2)))

;;End-to-end viewing.  
'
(emt:test:ts:run-test 
   '("Situation: testing an example" 
       (error "An example error")) 
   #'emtest:viewer:receive)
'  ;;To show the result
(pp
   (let 
      ((l))
      (emt:test:ts:run-test 
	 '("Situation: testing an example" 
	     (error "An example error")) 
	 #'(lambda (r)
	      (push r l)))
      l))


'
(emtest:ts:run-test
   '("Situation: testing an example" 
       (error "An example error")))



;;;_. Footers
;;;_ , Provides

(provide 'emviewer/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emviewer/tests.el ends here
