;;;_ viewers/hiformat/tests.el --- Tests for hiformat

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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

(require 'viewers/hiformat)

;;;_. Body

;;;_   , Test data
;;Define examples, some share the same functors.
;;;_   , Tests
(emt:deftest-3 hiformat:map
   (nil
      (progn
	 (emt:doc "Situation: Func and separator are trivial, just write 1 or 0.
PARAM: Separator is a formatted list.")
	 (emt:doc "Response: As expected.")
	 (progn
	    (emt:assert
	       (equal
		  (hiformat:map
		     #'(lambda
			  (&rest dummy)
			  '("1"))
		     '(a b c)
		     :separator
		     '("
"))
		  '("1" "
" "1" "
" "1")))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Func and separator are trivial, just write 1 or 0.
PARAM: Separator is a function.")
	 (emt:doc "Response: As expected.")
	 (progn
	    (emt:assert
	       (equal
		  (hiformat:map
		     #'(lambda
			  (&rest dummy)
			  '("1"))
		     '(a b c)
		     :separator
		     #'(lambda
			  (&rest dummy)
			  '("
")))
		  '("1" "
" "1" "
" "1")))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Func and separator-f see data.")
	 (emt:doc "Response: As expected.")
	 (progn
	    (emt:assert
	       (equal
		  (hiformat:map
		     #'(lambda
			  (dummy-1 dummy-2 immediate-data-x)
			  (cond
			     ((assq 'first immediate-data-x)
				'("FIRST"))
			     ((assq 'last immediate-data-x)
				'("LAST"))
			     (t
				'("OTHER"))))
		     '(a b c)
		     :separator
		     #'(lambda
			  (&rest dummy)
			  '("
")))
		  '("FIRST" "
" "OTHER" "
" "LAST")))
	    t)))
   (nil
      (progn
	 (emt:doc "PARAM: ELS=0 is given
PARAM: LIST is empty")
	 (emt:doc "Response: The ELS=0 form is printed.")
	 (progn
	    (emt:assert
	       (equal
		  (hiformat:map
		     #'(lambda
			  (&rest dummy)
			  '("1"))
		     'nil :separator
		     #'(lambda
			  (&rest dummy)
			  '("
"))
		     :els=0
		     '("12" "144"))
		  '("12" "144")))
	    t)))
   (nil
      (progn
	 (emt:doc "PARAM: ELS=1 is given
PARAM: LIST has 1 element")
	 (emt:doc "Response: The ELS=1 function is called instead of the usual function.")
	 (progn
	    (emt:assert
	       (equal
		  (hiformat:map
		     #'(lambda
			  (&rest dummy)
			  '("1"))
		     '(0)
		     :separator
		     #'(lambda
			  (&rest dummy)
			  '("
"))
		     :els=1
		     #'(lambda
			  (dummy dummy-2)
			  '("12" "144")))
		  '("12" "144")))
	    t))))


;;;_. Footers
;;;_ , Provides

(provide 'viewers/hiformat/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/hiformat/tests.el ends here
