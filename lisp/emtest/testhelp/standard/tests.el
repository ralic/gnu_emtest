;;;_ emtest/testhelp/standard/tests.el --- Tests for standard testhelp

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

(require 'emtest/testhelp/tagnames)

;;Just because emth:wrap-form was tested with the persist examples.
(require 'emtest/testhelp/persist)  

;;;_. Body
;;;_  . emth:map&trap

(emt:deftest-3 emth:map&trap
   (nil
      (progn
	 (emt:doc "Proves: Maps over values.")
	 (emt:assert
	    (equal
	       (emth:map&trap
		  #'eval
		  '(12 (+ 1 2)))
	       '(12 3)))))
   
   (nil
      (let ((a 0))
	 (emt:doc "Situation: First form errors. Second one increments
      a variable")
	 (ignore-errors
	    (emth:map&trap
	       #'eval 
	       '((error "Example error")
		   (incf a))))
	 
	 (emt:doc "Response: The error is raised, which shows that the
	 first clause ran.") 
	 (emt:doc "Response: The variable has been incremented, which
	 shows that the second clause ran.")
	 (emt:assert
	    (equal a 1)))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/standard/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/standard/tests.el ends here
