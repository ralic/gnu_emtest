;;;_ emtest/testhelp/tables/tests.el --- Tests for emtest/testhelp/tables

;;;_. Headers
;;;_ , License
;; Copyright (C) 2012  Tom Breton (Tehom)

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

(require 'emtest/testhelp/tables)

;;;_. Body

(emt:deftest-3
   ((of 'emt:tab:make))
   (nil
      (let
	 ((table
	     (emt:tab:make
		"My table of data for my test"
		(input result)
		("Trivial example" :input 0 :result 0)
		("More complex example" :input 10 :result 20))))
	 (emt:doc "Situation: We have a table.  We look at it in
   various ways.")

	 (emt:doc "Operation: Loop thru it.")
	 (emt:tab:for-each-row table i
	    (emt:doc "Check: Value is one of ours.")
	    (emt:assert
	       (member
		  (emt:tab i 'input)
		  '(0 10)))
	    (emt:assert
	       (member
		  (emt:tab i 'result)
		  '(0 20)))
	    (emt:doc "Operation: Use the values in a little test.")
	    (emt:doc "Result: the values correspond within rows")
	    (emt:assert
	       (equal
		  (* 2 (emt:tab i 'input))
		  (emt:tab i 'result))))
	 
	 )))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tables/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tables/tests.el ends here
