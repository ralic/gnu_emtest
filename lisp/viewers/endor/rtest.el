;;;_ viewers/endor/rtest.el --- Tests for Endor

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
(require 'viewers/endor/testhelp)
(require 'emtest/testhelp/mocks/filebuf)
(require 'viewers/hiformat) ;;For loformat

;;;_. Body
;;;_ , endor:create
(put 'endor:make-endor 'rtest:test-thru
   'viewers/endor)
;;;_ , endor:--set-root
(put 'endor:--set-root 'rtest:test-thru
   'viewers/endor)
;;;_ , endor:ewoc-handler
(put 'endor:ewoc-handler 'rtest:test-thru
   'viewers/endor)
;;;_ , endor:dispatch
(put 'endor:dispatch 'rtest:test-thru
   'viewers/endor)
;;;_ , endor:dispatch-x 
(put 'endor:dispatch-x 'rtest:test-thru
   'viewers/endor)
;;;_ , viewers/endor

(rtest:deftest viewers/endor
   ;;This test only relates endor and the low printer that
   ;;`endor:th:make-usual-endor' uses.  But it validates an
   ;;assumption other tests rely on.
   (  "Proves: Will skip `nil' as a component."
      (with-temp-buffer
	 ;;Set up a vapid root
	 (endor:th:make-usual-endor '(nil nil))
	 (assert
	    (emtb:buf-contents-matches
	       :string "()"))
	 t))

   (  "Proves: Root object's value can be used by the ewoc mechanism."
      (with-temp-buffer
	 ;;Root is a string
	 (endor:th:make-usual-endor "abc")
	 
	 (assert
	    (emtb:buf-contents-matches
	       :string "abc"))
	 t))
   
   (  "Proves: Basic expansion works."
      ;;Here, the root is just an ewoc.  For others, the root will be
      ;;a list and all will be list or ewoc.
      (with-temp-buffer
	 ;;Create an endor.  Root is given.
	 (let
	    ((endor:th:do-checks t)
	       (endor
		  (endor:th:make-usual-endor
		     '("abc"))))
	    (assert
	       (emtb:buf-contents-matches
		  :string "(abc)")))
	 t))

   (  "Situation: An endor has been made but no root was given.
Operation: Set the root.
Result: Object is displayed in buffer.
Proves: Basic expansion works."
      (with-temp-buffer
	 (let
	    ((endor:th:do-checks t)
	       (endor
		  (endor:th:make-usual-endor nil)))
	    (assert
	       (emtb:buf-contents-matches
		  :string "")
	       t)
	    (endor:--set-root endor '("abc"))
	    (assert
	       (emtb:buf-contents-matches
		  :string "(abc)")
	       t))
	 t))

   (  "Situation: An endor has been made and root was given.
Operation: Set the root (again).
Result: Error.
Proves: Can't set the root twice."
      (with-temp-buffer
	 (let
	    ((endor:th:do-checks t)
	       (endor
		  (endor:th:make-usual-endor 0)))
	    (assert
	       (emt:gives-error
		  (endor:--set-root endor 0))))
	 t))

   (  "Operation: A node is queued for display multiple times
Response: It is still displayed only once."
      (with-temp-buffer
	 (let*
	    (
	       (endor
		  (endor:th:make-usual-endor "abc"))
	       (node
		  (endor:endor->root endor)))
	    (emtb:buf-contents-matches
	       :string "abc")
	    (endor:will-display-node endor node)
	    (endor:will-display-node endor node)
	    (endor:will-display-node endor node)
	    
	    (emtp:eval 
	       (endor:display-pending endor)
	       (tp*
		  (:id tp:j4rfxx-display :fallthru t :count 1)
		  (node)))
	    
	    t)))
   
   )
;;;_. Footers
;;;_ , Provides

(provide 'viewers/endor/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/endor/rtest.el ends here
