;;;_ viewers/chewie/rtest.el --- Tests for Chewie

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
(require 'viewers/chewie)
(require 'viewers/wookie/testhelp)
;;;_. Structures

;;;_ , chewie:th:format-dynamic-1s+1rec
(defun chewie:th:format-dynamic-1s+1rec (obj data)
   "Format prints a string field, then delegates to a child if any.
Distinct from `wookie:th:format-dynamic-1s+1rec', this expects to work
inside a chewie."
   `( 
      ,(wookie:tht:dynamic-1s+1rec->str obj)
      "("
       ,(chewie:dynamic-notnull
	   (wookie:tht:dynamic-1s+1rec->recurse obj)
	   data
	   #'chewie:th:format-dynamic-1s+1rec)
      ")"))


;;;_. Body
(rtest:deftest chewie

   (  "Proves: LOAL's values are available."
      (with-temp-buffer
	 (let
	    ((dlist (wookie:make-dlist)))
	    (chewie:make-chewie
	       ;;Root
	       0
	       ;;top-data 
	       (loal:acons 'my-key "abc" '())
	       ;;Expander function ignores obj and returns a list of
	       ;;one string obtained from data.
	       #'(lambda (obj data)
		    (list 
		       (loal:val 'my-key data 
			  "WRONG This default should not be seen")))
	       ;;ewoc-printer
	       #'wookie:th:usual-printer
	       ;;get-dlist
	       #'(lambda (&rest r) dlist))
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc"))
	    t)))

   (  "Situation: Typical chewie.
Response: Behaves as expected."
      (with-temp-buffer
	 (let
	    ((endor:th:do-checks t))
	    (chewie:make-chewie
	       ;;root
	       (wookie:make-tht:dynamic-1s+1rec
		  :list (wookie:make-dlist)
		  :str "abc"
		  :recurse
		  (wookie:make-tht:dynamic-1s+1rec
		     :list (wookie:make-dlist)
		     :str "def"
		     :recurse nil))
	       ;;top-data 
	       '()
	       ;;expander 
	       #'chewie:th:format-dynamic-1s+1rec
	       ;;ewoc-printer
	       #'wookie:th:usual-printer
	       ;;get-dlist
	       #'wookie:tht:dynamic-1s+1rec->list)
	    
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc(def())"))
	    t)))
   
   
   )
;;;_. Footers
;;;_ , Provides

(provide 'viewers/chewie/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/chewie/rtest.el ends here
