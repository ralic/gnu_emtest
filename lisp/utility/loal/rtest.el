;;;_ utility/loal/rtest.el --- Tests for LOAL (Lists of Alists)

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal, maint

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

;;;_. Body
;;;_ , loal:assq
(rtest:deftest loal:assq

   (  "Param: Key is not in list.
Response: Nothing found."
      (progn
	 (assert
	    (eq
	       (loal:assq 'key1 '())
	       nil))
	 t))
   
   (  "Situation: Key in first list
Response: Value is found."
      (progn
	 (assert
	    (equal
	       (loal:assq 'key1 '(((key1 val1))))
	       '(key1 val1))
	    t)
	 t))
   
   (  "Situation: Key in another list
Response: Value is found."
      (progn
	 (assert
	    (equal
	       (loal:assq 'key1 '(((key2 val2))((key1 val1))))
	       '(key1 val1))
	    t)
	 t)))

;;;_ , loal:val
;;No tests provided.

;;;_ , loal:acons
(put 'loal:acons 'rtest:test-thru
   'loal:assq)



;;;_. Footers
;;;_ , Provides

(provide 'utility/loal/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/loal/rtest.el ends here
