;;;_ wookie/tests.el --- Tests for wookie

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
(require 'wookie/testhelp)
(require 'mockbuf)

;;;_. Body
;;;_ , wookie:create
(put 'wookie:create 'rtest:test-thru
   'wookie)
;;;_ , wookie
(rtest:deftest wookie
   (  "Proves: Basic expansion works."
      (with-temp-buffer
	 ;;Create a wookie.  Root is given.
	 
	 (wookie:create
	    ;;Object expander for wookie.  Maybe make this a test helper.
	    #'(lambda (&rest r)
		 (list (wookie:th:->displayable "abc")))
	    
	    ;;Printer for ewoc.
	    #'loformat:print
	    :object 0
	    :buf (current-buffer)
	    :showing-cb #'ignore
	    :unshowing-cb #'ignore)
	 
	 (mockbuf:buf-contents-matches
	    :string "abc")))

   (  "Situation: A wookie has been made but no root given.
Operation: Set the root.
Result: Object is displayed in buffer.
Proves: Basic expansion works."
      (with-temp-buffer
	 (let
	    ((wookie
		(wookie:create
		   ;;Object expander for wookie.  Maybe make this a test helper.
		   #'(lambda (&rest r)
			(list (wookie:th:->displayable "abc")))
	    
		   ;;Printer for ewoc.
		   #'loformat:print
		   :buf (current-buffer)
		   :showing-cb #'ignore
		   :unshowing-cb #'ignore)))
	    (assert
	       (mockbuf:buf-contents-matches
		  :string "")
	       t)
	    (wookie:set-root wookie 0)

	    (assert
	       (mockbuf:buf-contents-matches
		  :string "abc")
	       t))
	 t))

   (  "Proves: Can recurse hierarchically."
      (with-temp-buffer
	 ;;Root contains an object of its own type.
	 (let*
	    (  (root
		  (wookie:make-tht:1s+1rec
		     :str "abc"
		     :recurse
		     (wookie:make-tht:1s+1rec
			:str "def"
			:recurse nil))))

	    (wookie:create
	       #'wookie:th:format-1s+1rec-static
	       ;;Printer for ewoc.
	       #'loformat:print
	       :object root
	       :buf (current-buffer)
	       :showing-cb #'ignore
	       :unshowing-cb #'ignore)
	    (mockbuf:buf-contents-matches
	       :string "abc(def())")))))

;;;_. Footers
;;;_ , Provides

(provide 'wookie/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; wookie/tests.el ends here
