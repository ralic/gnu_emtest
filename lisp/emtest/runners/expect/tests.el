;;;_ emtest/runners/expect/tests.el --- Tests for emtest/runners/expect

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

(require 'emtest/runners/expect)

;;;_. Body
;;;_ , Insulation
(defconst emtest/runners/expect:th:surrounders 
   '()
   "The normal surrounders for emtest/runners/expect tests" )
;;;_ , emt:xp:expect:expect
(emt:deftest-3
   ((of 'emt:xp:expect:expect))
   ;;A simple test, which should just pass.

   (expect ;;The governor.
      ;;The parameters
      ((exec+args '("/bin/sh" "-i"))
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
	    (equal raw-answer "hello\n% "))
	 (emt:assert 
	    (equal answer "hello\n")))))

;;;_ , To meta-test
;;Receive and test that fail/no fail was found, and no other grade.
;;Check the notes in the order they come in.

;;Use emtt:th:explore-one with the bare form and the callback of
;;interest.  Top testhelp doesn't provide such a one, but summarize
;;may.  In fact, 'emtest/viewer/emviewer/testhelp' provides something,
;;but just `emt:vw:top:ts:run-test:callback' to check that results are the
;;right type.  Similarly `emtest/viewer/emviewer2/testhelp'.  We'd
;;want to summarize them and check the summary.

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runners/expect/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runners/expect/tests.el ends here
