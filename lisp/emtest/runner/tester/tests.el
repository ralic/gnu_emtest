;;;_ emtest/runner/tester/tests.el --- Tests for tester itself

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

;;;_ , Requires
(require 'emtest/runner/tester)
(require 'emtest/runner/tester/testhelp)
(require 'emtest/runner/launch/testhelp)
(require 'emtest/runner/surrounders)

;;;_. Body


;;;_ , emth:trap-errors

(emt:deftest-3 emth:trap-errors
   (nil
      (progn
	 (emt:doc "Shows: Works with `emtts:surround'.")
	 (equal
	    (eval
	       (emts:add-surrounders 12
		  '((emth:trap-errors))
		  nil))
	    12)))

   (nil
      (progn
	 (emt:doc "Situation: Body throws an emt:already-handled error.")
	 (emt:doc "Response: (Punt) Marks the event-group aborted.
Adds no error report.
Does not signal error.")
	 (progn
	    (emt:assert
	       (not
		  (emth:gives-error
		     (emth:trap-errors
			(signal 'emt:already-handled nil)))))
	    t)))
   ;;More to add.  See [[id:ca903ca0-bd5d-4985-8cd3-a5a4dd998b5c][]]
   )

;;;_ , utim:get-properties
(emt:deftest-3 utim:get-properties
   (nil
      (progn
	 (emt:doc "Situation: A test is defined with properties.
That test is now being run.
Behavior: `utim:get-properties' returns the relevant property.")
	 (emth:let-noprops
	    '(dummy-sym)
	    (emt:deftest-3
	       ((of 'dummy-sym)
		  (db-id "my-db")
		  (example-dir "examples/"))
	       (nil
		  (progn
		     (emt:doc "Docstring")
		     (emtp 531a913c-aa10-4730-9be5-30c1cb02b7c4 nil t))))
	    (emtp:eval
	       (emtt:th:run-suite 'dummy-sym #'ignore)
	       (tp 531a913c-aa10-4730-9be5-30c1cb02b7c4 nil
		  (progn
		     (emt:assert
			(equal
			   (utim:get-properties 'db-id)
			   "my-db")
			t)
		     (emt:assert
			(equal
			   (utim:get-properties 'example-dir)
			   "examples/")
			t))))
	    t))))

;;;_ , emtt:explore-one

(put 'emtt:explore-one 'emt:test-thru 'emtt:test-finder:top)

;;;_ , emtt:test-finder:top

(emt:deftest-3 emtt:test-finder:top
   (nil
      (progn
	 (emt:doc "Shows: It passes callback an `emt:testral:report'.")
	 (progn
	    (emtt:th:explore-one
	       '(error "An example error")
	       #'(lambda
		    (report)
		    (emty:check report emt:testral:report)))
	    t))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/tester/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/tester/tests.el ends here
