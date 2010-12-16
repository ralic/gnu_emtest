;;;_ emtest/runner/runners/external/tests.el --- Tests for emtest/runner/runners/external

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

(require 'emtest/runner/runners/external)

;;;_. Body
;;;_ , Insulation
(defconst emtest/runner/runners/external:th:surrounders 
   '()
   "The normal surrounders for emtest/runner/runners/external tests" )
;;;_ , emtr:external
(emt:deftest-3
   ((of 'emtr:external))
   ;;A simple test, which should just pass.

   (external ;;The governor.
      ;;The parameters
      ((exec+args ("/bin/sh" "-i"))
	 (shell nil)
	 (prompt "% ")
	 (timeout 10))

      ;;The interactions.  In each, `t' is the governor.
      ;;Nil cdr = no test
      (t "PS1='% '\n")
      ;;If there's a cdr, it's the form.
      (t "echo hello\n"
	 (emt:doc "A simple test")
	 (emt:assert 
	    (equal answer "hello\r\n% ")))))

;;;_ , To meta-test
;;Receive and test that fail/no fail was found, and no other badnesses.
;;Check the notes in the order they come in.

;;Use emtt:th:explore-one with the bare form and the callback of
;;interest.  Top testhelp doesn't provide such a one, but summarize
;;may.  In fact, 'emtest/viewer/emviewer/testhelp' provides something,
;;but just `emtve:ts:run-test:callback' to check that results are the
;;right type.  Similarly `emtest/viewer/emviewer2/testhelp'.  We'd
;;want to summarize them and check the summary.

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/runners/external/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/runners/external/tests.el ends here
