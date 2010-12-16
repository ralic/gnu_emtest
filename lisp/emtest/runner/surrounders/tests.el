;;;_ tests.el --- Tests for surrounders

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

(require 'emtest/runner/surrounders)
(require 'emtest/testhelp/standard)
(require 'emtest/runner/define)

;;;_. Body

;;;_ , emts:add-surrounders

;;Do not use surrounders in this test - bootstrapping issue.
(emt:deftest-3 emts:add-surrounders
   (nil
      (progn
	 (emt:doc
	    "Args: Empty list of protectors.")
	 (emt:doc
	    "Result: The original form.")
	 (emt:assert
	    (equal
	       (emts:add-surrounders 'foo '() '())
	       'foo))))
   
   (nil
      (progn
	 (emt:doc
	    "Args: List of one protector.")
	 (emt:doc
	    "Result: Form is surrounded by that protector.")
	 (emt:assert
	    (equal
	       (emts:add-surrounders 'foo '((progn)) '())
	       '(progn foo)))))

   (nil
      (progn
	 (emt:doc
	    "Args: List of one protector, more complicated form.")
	 (emt:doc
	    "Result: More complicated form is correctly surrounded.")
	 (emt:assert
	    (equal
	       (emts:add-surrounders 
		  '(let (a b) foo) 
		  '((progn))
		   '())
	       '(progn (let (a b) foo))))))
   (nil
      (progn
	 (emt:doc
	    "Args: List of one more complicated surrounder.")
	 (emt:doc
	    "Result: Form is correctly surrounded.")
	 (emt:assert
	    (equal
	       (emts:add-surrounders 
		  'foo
		  '((let (a b)))
		   '())
	       '(let (a b) foo)))))

   (nil
      (progn
	 (emt:doc
	    "Args: List of three protectors.")
	 (emt:doc
	    "Result: Form is surrounded by all three in order, first outermost.")
	 (emt:assert
	    (equal
	       (emts:add-surrounders 
		  'foo 
		  '((progn) (save-excursion) (with-temp-buffer))
		   '())
	       '(progn (save-excursion (with-temp-buffer foo)))))))
   )

;;;_. Footers
;;;_ , Provides

(provide 'tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tests.el ends here
