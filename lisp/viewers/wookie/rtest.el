;;;_ viewers/wookie/rtest.el --- Tests for Wookie

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
;;;_. Body
;;;_ , wookie:handler-alist

(put 'wookie:handler-alist 'rtest:test-thru
   'wookie)
;;;_ , wookie:compress-list
(rtest:deftest wookie:compress-list

   (  "Situation: With a (dummy) wookie that recognizes nothing.
Param: A list of strings.
Response: The strings have been collected into a list."
      (with-temp-buffer
	 (let*
	    ;;Dummy dlist, since our objects here are just empty
	    ;;dummies.
	    ((endor:th:do-checks t)
	       (dlist (wookie:make-dlist))
	       (wookie
		  (wookie:th:make-usual-wookie
		     ;;Dummy format function 
		     #'(lambda (obj) '())
		     ;;Dummy object
		     0
		     ;;Dummy Dlist getter
		     #'(lambda (&rest r) dlist))))
	    
	    (assert
	       (equal
		  (wookie:compress-list wookie '("a" "b" "c"))
		   '(("a" "b" "c")))
	       t)
	    t)))
   
   )
;;;_ , wookie:th:w/o-compression
;;Maybe could just put this around entire 
(defmacro wookie:th:w/o-compression (&rest forms)
   "
Intended to be placed around tests that don't want compression.
"
   
   `(emtp:eval
      (progn ,@forms)
      ;;Don't compress before edits, so all items stay separate.  We
      ;;want a known sequence of items to be stored.
      (tp*
	 (:id tp:tdo969n0qxe0 :fallthru t :count nil)
	 (data-list)
	 (throw 'emtp:tag-return data-list))))


;;;_ , Tests of wookie as a whole

(rtest:deftest wookie

   (  "Proves: Nested dynamic expansion works.
Dynamic objects are displayed immediately."
      (with-temp-buffer
	 ;;Set up
	 (let
	    ;;Dummy dlist, since our objects here are just empty
	    ;;dummies.
	    ((endor:th:do-checks t)
	       (dlist (wookie:make-dlist)))

	    (wookie:th:make-usual-wookie
	       ;;Format function returns a list of constant strings
	       ;;and one dynamic object.
	       #'(lambda (obj)
		    (if
		       (zerop obj)
		       (list 
			  "abc"
			  `(th:dynamic 1)
			  "ghi")
		       (list "def")))
	       
	       ;;Dummy object
	       0
	       ;;Dummy Dlist getter
	       #'(lambda (&rest r) dlist))
	    
	    (assert
	       (emtb:buf-contents-matches
		  :string "abcdefghi"))
	    ;;There should be exactly 2 displayers registered now:
	    ;;Root and dynamic def.
	    (assert
	       (=
		  (length
		     (wookie:dlist->displayers dlist))
		  2)
	       t)
	    t)))
   
   

   (  "Proves: Can recurse hierarchically."
      (with-temp-buffer
	 ;;Root contains an object of its own type.
	 (let
	    (
	       (endor:th:do-checks t)
	       (wookie
		  (wookie:th:make-usual-wookie
		     ;;Format function interprets this type of object.  It
		     ;;is passed data too.
		     #'wookie:th:format-dynamic-1s+1rec
		     ;;Root contains an object of its own type.
		     (wookie:make-tht:dynamic-1s+1rec
			:list (wookie:make-dlist)
			:str "abc"
			:recurse
			(wookie:make-tht:dynamic-1s+1rec
			   :list (wookie:make-dlist)
			   :str "def"
			   :recurse nil))
		     ;;Wookie list getter
		     #'wookie:tht:dynamic-1s+1rec->list)))
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc(def())"))
	    t)))

   (  "Demonstrates: updating by altering objects' simple data."
      (with-temp-buffer
	 (labels
	    (
	       (reset (wookie obj str)
		  (setf
		     (endor:tht:1s+1rec->str obj) str)
		  (wookie:redisplay 
		     wookie 
		     (wookie:tht:dynamic-1s+1rec->list obj))))
	    
	    (let*
	       ;;Root contains two objects.  
	       ((endor:th:do-checks t)
		  (root
		     (wookie:make-tht:dynamic-1s+1rec
			:list (wookie:make-dlist)
			:str "abc"
			:recurse
			(wookie:make-tht:dynamic-1s+1rec
			   :list (wookie:make-dlist)
			   :str "def"
			   :recurse nil)))
		  (wookie
		     (wookie:th:make-usual-wookie
			#'wookie:th:format-dynamic-1s+1rec
			root
			#'wookie:tht:dynamic-1s+1rec->list)))
	       
	       ;;Validate original contents.
	       (assert
		  (emtb:buf-contents-matches
		     :string "abc(def())")
		  t)

	       ;;$$MAKE ME A TEST We are getting extra nils here.

	       '((nil "") (nil nil) (nil nil) (1 "abc") (4 "-") 
		   (nil nil) (5 "def") (nil "")) 
	       '
	       (endor:th:get-position-skeleton 
		  wookie)
	       
	       (reset wookie root "ghi")

	       ;;Check buffer. 
	       (assert
		  (emtb:buf-contents-matches
		     :string "ghi(def())")
		  t)

	       ;;Update the other.
	       (reset wookie (endor:tht:1s+1rec->recurse root) "jkl")

	       ;;Check buffer. 
	       (assert
		  (emtb:buf-contents-matches
		     :string "ghi(jkl())")
		  t)
	       t))))



   (  "Demonstrates: Removing objects can work."
      (with-temp-buffer
	 (let*
	    (  (endor:th:do-checks t)
	       (root
		  ;;Root contains an object of its own type.
		  (wookie:make-tht:dynamic-1s+1rec
		     :list (wookie:make-dlist)
		     :str "abc"
		     :recurse
		     (wookie:make-tht:dynamic-1s+1rec
			:list (wookie:make-dlist)
			:str "def"
			:recurse nil)))
	       (wookie
		  (wookie:th:make-usual-wookie
		     ;;Format function interprets this type of object.  It
		     ;;is passed data too.
		     #'wookie:th:format-dynamic-1s+1rec
		     root
		     ;;Wookie list getter
		     #'wookie:tht:dynamic-1s+1rec->list)))
	    
	    (emtb:buf-contents-matches
	       :string "abc(def())")

	    ;;Remove the child item and undisplay it
	    (let
	       ((child
		   (wookie:tht:dynamic-1s+1rec->recurse root)))
	       (setf (wookie:tht:dynamic-1s+1rec->recurse root) nil)
	       (wookie:display-gone 
		  wookie 
		  (wookie:tht:dynamic-1s+1rec->list child)))
	    
	    ;;Check contents
	    (emtb:buf-contents-matches
	       :string "abc()")

	    ;;Replace the child item with "ghi" instead.  Redisplay
	    ;;its parent (because only caller knows what holds that
	    ;;node)
	    (let
	       ((new-child
		   (wookie:make-tht:dynamic-1s+1rec
		      :list (wookie:make-dlist)
		      :str "ghi"
		      :recurse nil)))
	       (setf (wookie:tht:dynamic-1s+1rec->recurse root) new-child)
	       (wookie:redisplay 
		  wookie 
		  (wookie:tht:dynamic-1s+1rec->list root)))

	    ;;New node should be visible in the right place.
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc(ghi())"))
	    t)))
   
   



   ;;Prove: Updating uses the "local" alist

   ;;Prove: The local alist overrides the inherited one.

   ;;Folding:
   ;;(Here, understand `folded' in the alist)
   ;;Root contains two foldable objects. They either say "[folded]" or
   ;;their text string.
   ;;Write everything, folded.
   ;;Update root, unfolded.
   ;;Should show everything.
   ;;Update one object, folded.
   ;;Should show the one folded, the other unfolded.

   ;;Same dynamic object is rendered twice - still works.

   ;;Have to somehow set the object contents.


   ;;Note how we have to alter the objects' contents without their
   ;;identity if we want to simply redisplay them, as we must for
   ;;root.

   (  "Situation: Root has been set and displayed.
We are using string and displaying them directly as their names.
Operation: Redisplay root, via dlist.
Response: Buffer display is as expected.
Dlist still holds only one display."
      (with-temp-buffer
	 (let*
	    (  (endor:th:do-checks t)
	       (dlist (wookie:make-dlist))
	       (root (list '("abcd")))
	       (wookie
		  (wookie:th:make-usual-wookie
		     ;;Formatter just returns list
		     #'(lambda (obj &rest r)
			  (check-type obj (list (repeat string)) 
			     "List of strings")
			  (car obj))
		     root
		     #'(lambda (&rest r) dlist))))
	    (setf (car root) '("abc"))
	    (wookie:redisplay wookie dlist)
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc"))
	    (assert
	       (=
		  (length
		     (wookie:dlist->displayers dlist))
		  1)
	       t)
	    t)))

   (  "Situation: Root has been set and displayed.
We are using symbols and displaying symbols directly as their names.
Operation: Redisplay root, via dlist.
Response: Buffer display is as expected.
Dlist still holds only one display."
      (wookie:th:w/o-compression
	 (with-temp-buffer
	    (let*
	       (  (endor:th:do-checks t)
		  (dlist (wookie:make-dlist))
		  (root (list '(a b c d)))
		  (wookie
		     (wookie:th:make-usual-wookie
			;;Formatter just returns list
			#'(lambda (obj &rest r)
			     (check-type obj (list (repeat symbol)) 
				"List of symbols")
			     (car obj))
			root
			#'(lambda (&rest r) dlist)
			;;Use an ewoc displayer that expects a symbol.
			#'(lambda (x)
			     (etypecase x
				(symbol
				   (insert (symbol-name x))))))))
	       (assert
		  (emtb:buf-contents-matches
		     :string "abcd"))

	       (setf (car root) '(a b c))
	       (wookie:redisplay wookie dlist)
	       (assert
		  (emtb:buf-contents-matches
		     :string "abc"))
	       (assert
		  (=
		     (length
			(wookie:dlist->displayers dlist))
		     1)
		  t)
	       t))))
   


   ;;Test use of align-lists
   (  "Operation: Remove one node at the end.
Result: Just the expected sub-operations are used.
Shows: It minimizes sub-operations."
      (wookie:th:w/o-compression
	 (with-temp-buffer
	    (let*
	       (  (endor:th:do-checks t)
		  (dlist (wookie:make-dlist))
		  ;;Must be dynamic because we mutate its car later
		  (root (list '(a b c d)))
		  (wookie
		     (wookie:th:make-usual-wookie
			;;Formatter just returns list
			#'(lambda (obj &rest r)
			     (check-type obj (list (repeat symbol)) 
				"List of symbols")
			     (car obj))
			root
			#'(lambda (&rest r) dlist)
			;;Use an ewoc displayer that expects symbols.
			#'(lambda (x)
			     (typecase x
				(symbol
				   (insert (symbol-name x))))))))

	       (assert
		  (emtb:buf-contents-matches
		     :string "abcd"))
	       (let
		  (
		     (deleted '())
		     (displayed '())
		     (make-noded '()))
		  (emtp:eval 
		     ;;As implied above, this call finds dlist because
		     ;;that's in a special variable.  So it does find the
		     ;;display.
		     (progn
			(setf (car root) '(a b c))
			(wookie:redisplay wookie dlist))
		     ;;Don't compress before edits, so all items stay
		     ;;separate.  This clause is needed despite
		     ;;`wookie:th:w/o-compression' because testpoint
		     ;;doesn't nest.
		     (tp*
			(:id tp:tdo969n0qxe0 :fallthru t :count nil)
			(data-list)
			(throw 'emtp:tag-return data-list))
		     (tp*
			(:id tp:mus34bd1mxe0 :count nil)
			(edits)
			(assert
			   (equal 
			      edits
			      (list
				 (list 'both (sxhash 'a)(sxhash 'a))
				 (list 'both (sxhash 'b)(sxhash 'b))
				 (list 'both (sxhash 'c)(sxhash 'c))
				 (list 'a    (sxhash 'd))))
			   t))
		     (tp*
			(:id tp:n3k5ro-make-node :fallthru t :count nil)
			(o)
			(push o make-noded))
		     (tp*
			(:id tp:9wr6as-delete :fallthru t :count nil)
			(node)
			(push (ewoc--node-data node) deleted))
		     (tp*
			(:id tp:j4rfxx-display :fallthru t :count nil)
			(node)
			(push (ewoc--node-data node) displayed)))
	       
		  (assert
		     (emtb:buf-contents-matches
			:string "abc"))
		  (assert (equal deleted    '(d)) t)
		  (assert (equal make-noded '()) t)
		  (assert (equal displayed  '()) t))
      
	       t))))
   
   (  "Operation: Replace the one node.
Result: Just the expected sub-operations are used.
Shows: It minimizes sub-operations."
      (wookie:th:w/o-compression
	 (with-temp-buffer
	    (let*
	       (  (endor:th:do-checks t)
		  (dlist (wookie:make-dlist))
		  ;;Must be dynamic because we mutate its car later
		  (root (list '(a)))
		  (wookie
		     (wookie:th:make-usual-wookie
			;;Formatter just returns list
			#'(lambda (obj &rest r)
			     (check-type obj (list (repeat symbol)) 
				"List of symbols")
			     (car obj))
			root
			#'(lambda (&rest r) dlist)
			;;Use an ewoc displayer that expects symbols.
			#'(lambda (x)
			     (typecase x
				(symbol
				   (insert (symbol-name x))))))))
	       (assert
		  (emtb:buf-contents-matches
		     :string "a"))
	       (let
		  (
		     (deleted '())
		     (displayed '())
		     (make-noded '()))
		  (emtp:eval 
		     ;;As implied above, this call finds dlist because
		     ;;that's in a special variable.  So it does find the
		     ;;display.
		     (progn
			(setf (car root) '(e))
			(wookie:redisplay wookie dlist))
		     ;;Don't compress before edits, so all items stay
		     ;;separate.  This clause is needed despite
		     ;;`wookie:th:w/o-compression' because testpoint
		     ;;doesn't nest.
		     (tp*
			(:id tp:tdo969n0qxe0 :fallthru t :count nil)
			(data-list)
			(throw 'emtp:tag-return data-list))
		     (tp*
			(:id tp:mus34bd1mxe0 :count nil)
			(edits)
			(assert
			   (equal 
			      edits
			      (list
				 (list 'a    (sxhash 'a))
				 (list 'b    (sxhash 'e))))
			   t))
		     (tp*
			(:id tp:n3k5ro-make-node :fallthru t :count nil)
			(o)
			(push o make-noded))
		     (tp*
			(:id tp:9wr6as-delete :fallthru t :count nil)
			(node)
			(push (ewoc--node-data node) deleted))
		     (tp*
			(:id tp:j4rfxx-display :fallthru t :count nil)
			(node)
			(push (ewoc--node-data node) displayed)))
	       
		  (assert
		     (emtb:buf-contents-matches
			:string "e"))
		  (assert (equal deleted    '(a)) t)
		  (assert (equal make-noded '(e)) t)
		  (assert (equal displayed  '(e)) t))
      
	       t))))

   (  "Operation: Replace a node in the middle
Result: Just the expected sub-operations are used.
Shows: It minimizes sub-operations."
      (wookie:th:w/o-compression
	 (with-temp-buffer
	    (let*
	       (  (endor:th:do-checks t)
		  (dlist (wookie:make-dlist))
		  ;;Must be dynamic because we mutate its car later
		  (root (list '(a b c)))
		  (wookie
		     (wookie:th:make-usual-wookie
			;;Formatter just returns list
			#'(lambda (obj &rest r)
			     (check-type obj (list (repeat symbol)) 
				"List of symbols")
			     (car obj))
			root
			#'(lambda (&rest r) dlist)
			;;Use an ewoc displayer that expects symbols.
			#'(lambda (x)
			     (typecase x
				(symbol
				   (insert (symbol-name x))))))))
	       (assert
		  (emtb:buf-contents-matches
		     :string "abc"))
	       (let
		  (
		     (deleted '())
		     (displayed '())
		     (make-noded '()))
		  (emtp:eval 
		     ;;As implied above, this call finds dlist because
		     ;;that's in a special variable.  So it does find the
		     ;;display.
		     (progn
			(setf (car root) '(a e c))
			(wookie:redisplay wookie dlist))
		     ;;Don't compress before edits, so all items stay
		     ;;separate.  This clause is needed despite
		     ;;`wookie:th:w/o-compression' because testpoint
		     ;;doesn't nest.
		     (tp*
			(:id tp:tdo969n0qxe0 :fallthru t :count nil)
			(data-list)
			(throw 'emtp:tag-return data-list))
		     (tp*
			(:id tp:mus34bd1mxe0 :count nil)
			(edits)
			(assert
			   (equal 
			      edits
			      (list
				 (list 'both (sxhash 'a)(sxhash 'a))
				 (list 'a    (sxhash 'b))
				 (list 'b    (sxhash 'e))
				 (list 'both (sxhash 'c)(sxhash 'c))))
			   t))
		     (tp*
			(:id tp:n3k5ro-make-node :fallthru t :count nil)
			(o)
			(push o make-noded))
		     (tp*
			(:id tp:9wr6as-delete :fallthru t :count nil)
			(node)
			(push (ewoc--node-data node) deleted))
		     (tp*
			(:id tp:j4rfxx-display :fallthru t :count nil)
			(node)
			(push (ewoc--node-data node) displayed)))
	       
		  (assert
		     (emtb:buf-contents-matches
			:string "aec"))
		  (assert (equal deleted    '(b)) t)
		  (assert (equal make-noded '(e)) t)
		  (assert (equal displayed  '(e)) t))
      
	       t))))
   

   )

;;;_. Footers
;;;_ , Provides

(provide 'viewers/wookie/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/wookie/rtest.el ends here
