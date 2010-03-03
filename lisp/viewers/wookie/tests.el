;;;_ viewers/wookie/tests.el --- Tests for wookie

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
(require 'viewers/wookie/testhelp)
(require 'tester/testhelp/mocks/filebuf)
(require 'viewers/hiformat) ;;For loformat

;;;_. Body
;;;_ , wookie:create
(put 'viewers/wookie:create 'rtest:test-thru
   'endor)
;;;_ , wookie:set-root
(put 'wookie:set-root 'rtest:test-thru
   'endor)
;;;_ , wookie:ewoc-handler
(put 'wookie:ewoc-handler 'rtest:test-thru
   'endor)
;;;_ , wookie:dispatch
(put 'wookie:dispatch 'rtest:test-thru
   'endor)
;;;_ , wookie:dispatch-x 
(put 'wookie:dispatch-x 'rtest:test-thru
   'endor)
;;;_ , wookie
   ;;$$CHANGEME viewers/wookie
'
(rtest:deftest wookie
   (  "Proves: Basic expansion works."
      (with-temp-buffer
	 ;;Create a wookie.  Root is given.
	 (let
	    ((wookie
		(wookie:th:make-usual-wookie
		   ;;Expander.  How does this still fit into the design?
		   #'(lambda (&rest r)
			(list (wookie:th:->displayable "abc")))
		   0)))
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc")))
	 t))

   (  "Situation: A wookie has been made but no root was given.
Operation: Set the root.
Result: Object is displayed in buffer.
Proves: Basic expansion works."
      (with-temp-buffer
	 (let
	    ((wookie
		(wookie:th:make-usual-wookie 
		   #'(lambda (&rest r)
			(list (wookie:th:->displayable "abc")))
		   nil)))
	    (assert
	       (emtb:buf-contents-matches
		  :string "")
	       t)
	    (wookie:set-root wookie 0)

	    (assert
	       (emtb:buf-contents-matches
		  :string "abc")
	       t))
	 t))

   (  "Situation: A wookie has been made and root was given.
Operation: Set the root (again).
Result: Error.
Proves: Can't set the root twice."
      (with-temp-buffer
	 (let
	    ((wookie
		(wookie:th:make-usual-wookie 
		   #'(lambda (&rest r)
			(list (wookie:th:->displayable "abc")))
		   nil)))
	    (emt:gives-error
	       (wookie:set-root wookie 0)))
	 t))

   (  "Proves: Can recurse statically hierarchically."
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
	    (wookie:th:make-usual-wookie
	       #'wookie:th:format-1s+1rec-static
	       root)
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc(def())")))
	 t))

   (  "Proves: Root object's value is available."
      (with-temp-buffer
	 (wookie:th:make-usual-wookie
	    ;;Format function returns a list of one constant string.
	    #'(lambda (obj)
		 (list obj))
	    ;;Root is a string
	    "abc")
	 
	 (emtb:buf-contents-matches
	    :string "abc")))

   ;;This test only relates wookie and the low printer that
   ;;`wookie:th:make-usual-wookie' uses.  But it validates an
   ;;assumption other tests rely on.
   (  "Proves: Will skip `nil' as a component."
      (with-temp-buffer
	 ;;Set up a root.
	 (wookie:th:make-usual-wookie
	    ;;Format function returns a list of one nil
	    #'(lambda (&rest r)
		 (list nil))
	    ;;Dummy object and alist
	    0)
	 	 
	 (emtb:buf-contents-matches
	    :string "")))

   )

;;$$CHANGEME viewers/endor
(rtest:deftest endor

   (  "Proves: Root object's value can be used by the ewoc mechanism."
      (with-temp-buffer
	 (wookie:th:make-usual-wookie-2
	    ;;Format function returns a list of one constant string.
	    #'(lambda (obj)
		 (list obj))
	    ;;Root is a string
	    "abc")
	 
	 (emtb:buf-contents-matches
	    :string "abc")))
   
   (  "Proves: Basic expansion works."
      ;;Here, the root is just an ewoc.  For others, the root will be
      ;;a list and all will be list or ewoc.
      (with-temp-buffer
	 ;;Create a wookie.  Root is given.
	 (let
	    ((wookie
		(wookie:th:make-usual-wookie-2
		   ;;Expander.  How does this still fit into the
		   ;;design?  But it makes a list, which isn't right.
		   ;;This is about a non-dynamic wookie, not about
		   ;;a bare expander.
		   ;;Expander is now irrelevant.
		   ;;$$REMOVE ME but co-ordinate with definition.
		   #'(lambda (&rest r)
			(wookie:th:->displayable "abc"))
		   '("abc"))))
	    (assert
	       (emtb:buf-contents-matches
		  :string "(abc)")))
	 t))

   (  "Situation: A wookie has been made but no root was given.
Operation: Set the root.
Result: Object is displayed in buffer.
Proves: Basic expansion works."
      (with-temp-buffer
	 (let
	    ((wookie
		(wookie:th:make-usual-wookie-2 
		   #'(lambda (&rest r)
			(list (wookie:th:->displayable "abc")))
		   nil)))
	    (assert
	       (emtb:buf-contents-matches
		  :string "")
	       t)
	    (wookie:set-root wookie '("abc"))

	    (assert
	       (emtb:buf-contents-matches
		  :string "(abc)")
	       t))
	 t))

   (  "Situation: A wookie has been made and root was given.
Operation: Set the root (again).
Result: Error.
Proves: Can't set the root twice."
      (with-temp-buffer
	 (let
	    ((wookie
		(wookie:th:make-usual-wookie-2 
		   #'(lambda (&rest r)
			(list (wookie:th:->displayable "abc")))
		   0)))
	    (assert
	       (emt:gives-error
		  (wookie:set-root wookie 0))))
	 t))

   ;;This test only relates wookie and the low printer that
   ;;`wookie:th:make-usual-wookie' uses.  But it validates an
   ;;assumption other tests rely on.
   (  "Proves: Will skip `nil' as a component."
      (with-temp-buffer
	 ;;Set up a root.
	 (wookie:th:make-usual-wookie-2
	    ;;Format function returns a list of one nil
	    #'(lambda (&rest r)
		 (list nil))
	    ;;Dummy object
	    '(nil nil))
	 	 
	 (emtb:buf-contents-matches
	    :string "()")))

   )
;;;_. Footers
;;;_ , Provides

(provide 'viewers/wookie/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/wookie/tests.el ends here
