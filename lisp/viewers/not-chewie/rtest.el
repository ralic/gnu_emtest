;;;_ tests.el --- The only part that exists of a hypothetical chewie alternative

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

;;;_. Body

(rtest:deftest not-chewie
   ;;Test was not consistent with the new chewie design.  It could be
   ;;made consistent with an alternative design of chewie where the
   ;;dynamic objects do not hold their own extra info, but that info
   ;;is held in association with the objects.
   (  "Demonstrates: updating by altering dynamic objects held by others."
      (with-temp-buffer
	 (let*
	    (  (endor:th:do-checks t)
	       (root
		  (wookie:make-tht:1s+1rec
		     :str "abc"
		     :recurse
		     (wookie:make-tht:1s+1rec
			:str "def"
			:recurse nil)))
	       (wookie
		  (wookie:th:make-usual-wookie
		     ;;$$YET TO BE WRITTEN - formatter, should
		     ;;parallel
		     ;;`wookie:th:format-dynamic-1s+1rec'.  Maybe
		     ;;can be shared.
		     #'wookie:th:format-1s+1rec
		     root
		     ;;$$YET TO BE WRITTEN - the lookup function
		     ;;from object to dlist
		     #'wookie:tht:1s->list)))
	    
	    ;;Validate original contents.
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc-def")
	       t)

	    ;;Even though one of the delegated objects was entirely
	    ;;removed, this must still come out OK.  It's possible,
	    ;;however, that some of it may be invisible, so turn
	    ;;the invisibility trick off if it's used.

	    (setf
	       (car root)
	       (endor:make-tht:1s :str "ghi"))
	       
	    (wookie:redisplay wookie root)
	       
	    ;;Check buffer. 
	    (assert
	       (emtb:buf-contents-matches
		  :string "ghi-def")
	       t)

	    t)))
   
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
