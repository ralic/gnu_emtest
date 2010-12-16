;;;_ emtest/editing/versioning/tests.el --- Tests of versioning

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

(require 'emtest/testhelp/standard)
(require 'emtest/runner/define)
(require 'emtest/editing/versioning)

;;;_. Body
(emt:deftest-3 emtvers:substring->docform
   (nil
      (progn
	 (emt:doc "Situation: Start and end are the same.")
	 (emt:doc "Response: Gives nil.")
	 (emt:assert
	    (equal
	       (emtvers:substring->docform "abc" 0 0)
	       nil))))
   
   (nil
      (progn
	 (emt:doc "Situation: Substring is all whitespace.")
	 (emt:doc "Response: Gives nil.")
	 (emt:assert
	    (equal
	       (emtvers:substring->docform " abc" 0 1)
	       nil))))
   (nil
      (progn
	 (emt:doc "Situation: Normal.")
	 (emt:doc "Response: Gives an emt docstring.")
	 (emt:assert
	    (equal
	       (emtvers:substring->docform " abc" 0 4)
	       '(emt:doc "abc")))))

   (nil
      (progn
	 (emt:doc "Situation: String contains a left parenthesis at
      beginning of a line.")
	 (emt:doc "Response: That parenthesis is now escaped.")
	 (emt:assert
	    (equal
	       (emtvers:substring->docform "a\n(bc)" 0)
	       '(emt:doc "a\n\\(bc)")))))
   
   )



;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/versioning/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/versioning/tests.el ends here
