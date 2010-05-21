;;;_ emtest/testhelp/types/dll/tests.el --- Rtest tests for dll typedef

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

(require 'emtest/testhelp/types/dll)

;;;_. Body
;;;_  . dll-repeat-f
(put 'dll-repeat-f 'emt:test-thru 'dll-repeat)

;;;_  . dll-repeat

(emt:deftest-3 dll-repeat
   (nil
      (progn
	 (emt:doc "Situation: DLL is empty.")
	 (emt:doc "Response: Returns t.")
	 (typep
	    (dll-create)
	    '(dll-repeat *))))
   (nil
      (progn
	 (emt:doc "Situation: Not a DLL")
	 (emt:doc "Response: Fail (return nil).")
	 (not
	    (typep 13
	       '(dll-repeat *)))))
   (nil
      (progn
	 (emt:doc "Situation: DLL has one item, integer.
Test for integer contents.")
	 (emt:doc "Response: Passes.")
	 (let*
	    ((dll
		(dll-create))
	       (obj
		  (dll-enter-first dll 12)))
	    (typep dll
	       '(dll-repeat integer)))))
   (nil
      (progn
	 (emt:doc "Situation: DLL has one item, integer.
Test for string contents.")
	 (emt:doc "Response: Fails.")
	 (let*
	    ((dll
		(dll-create))
	       (obj
		  (dll-enter-first dll 12)))
	    (not
	       (typep dll
		  '(dll-repeat string)))))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/types/dll/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/types/dll/tests.el ends here
