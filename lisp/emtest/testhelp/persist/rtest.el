;;;_ emtest/testhelp/persist/rtest.el --- Tests for emtest/testhelp/persist

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(require 'emtest/testhelp/misc)
;;;_. Body

;;;_ , emt:persist
(rtest:deftest emt:persist

   (  "Situation: `emt:trace:properties' has the db-id property bound.
Param: The backend param is not given.
Response: The value of the `db-id' property is used as backend."
      ;;$$CHANGEME
      ;;This is old-style emt deftest, change to version 3
      (let-noprops '(dummy-sym)
	 (emt:deftest-2 dummy-sym
	    (props (db-id "my-db"))
	    ("Clause is not used" ()))
	 (emtt:destructure-suite 'dummy-sym
	    (let* ((emt:trace:properties props)
		     (placeholder (emt:persist 'id-0)))
	       (assert
		  (equal 
		     (emt:db:id-index.-backend placeholder) 
		     "my-db")
		  t))
	    t))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/persist/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/persist/rtest.el ends here
