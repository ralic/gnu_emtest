;;;_ emtest/support/keepup/tests.el --- Tests for define

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

(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/misc)
(require 'emtest/main/define)
(require 'emtest/support/keepup)


;;;_. Body
;;;_ , emt:keep-up-to-date
(emt:deftest-3 emt:keep-up-to-date
   (nil
      ;;Insulate
      (let (foo bar)
	 (emth:let-noprops '(foo bar)
	    (emt:doc "Situation: Function foo is defined, giving `old-value'.")
	    (defun foo () 'old-value)
	    (emt:doc "Operation: define `bar', with keep-up-to-date on foo.")
	    (emt:keep-up-to-date (foo)
	       (defconst bar (foo)))
	    (emt:doc "Validate: bar is bound and has the expected value.")
	    (emt:assert (boundp 'bar))
	    (emt:assert
	       (eq bar 'old-value))
	    (emt:doc "Operation: Redefine function `foo'.")
	    (defun foo () 'new-value)
	    (emt:doc "Operation: Call emtd:update-for-sym manually.")
	    (emtd:update-for-sym 'foo)
	    (emt:doc "Result: bar now has the new value.")
	    (emt:assert
	       (eq bar 'new-value))))))

;;;_ , emt:keep-up-to-date-x
(put 'emt:keep-up-to-date-x 'emt:test-thru
   'emt:keep-up-to-date)
;;;_ , emtd:update-for-sym
(put 'emtd:update-for-sym 'emt:test-thru
   'emt:keep-up-to-date)


;;;_. Footers
;;;_ , Provides

(provide 'emtest/support/keepup/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/support/keepup/tests.el ends here
