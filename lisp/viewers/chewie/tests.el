;;;_ viewers/chewie/tests.el --- Tests for Chewie

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
(require 'viewers/chewie/testhelp)
;;;_. Body
;;;_ , chewie:handler-alist

(put 'chewie:handler-alist 'rtest:test-thru
   'chewie)

;;;_ , Tests of chewie as a whole

(rtest:deftest chewie

   ;;Contrast with static expansion for simple endor.
   (  "Proves: Nested dynamic expansion works.
Dynamic objects are displayed immediately."
      (with-temp-buffer
	 ;;Set up
	 (let
	    ;;Dummy chewie list, since our objects here are just
	    ;;empty dummies.
	    ((endor:th:do-checks t)
	       (chewlist (chewie:2:make-list)))

	    (chewie:th:make-usual-chewie
	       ;;Format function returns a list of constant strings
	       ;;and one dynamic object.
	       #'(lambda (&rest r)
		    (list 
		       "abc"
		       `(dynamic 1 () 
			   ,#'(lambda (&rest r)
				 (list "def")))
		       "ghi"))
	       ;;Dummy object and alist.  Set this up differently now.
	       0
	       ;;Dummy chewie list getter
	       #'(lambda (&rest r) chewlist))
	    
	    (assert
	       (emtb:buf-contents-matches
		  :string "abcdefghi"))
	    ;;There should be exactly 2 displayers registered now:
	    ;;Root and dynamic def.
	    (assert
	       (=
		  (length
		     (chewie:2:list->displayers chewlist))
		  2)
	       t)
	    t)))
   
   

   (  "Proves: Can recurse hierarchically."
      (with-temp-buffer
	 ;;Root contains an object of its own type.
	 (let
	    (
	       (endor:th:do-checks t)
	       (chewie
		  (chewie:th:make-usual-chewie
		     ;;Format function interprets this type of object.  It
		     ;;is passed data too.
		     #'chewie:th:format-dynamic-1s+1rec
		     ;;Root contains an object of its own type.
		     (chewie:make-tht:dynamic-1s+1rec
			:list (chewie:2:make-list)
			:str "abc"
			:recurse
			(chewie:make-tht:dynamic-1s+1rec
			   :list (chewie:2:make-list)
			   :str "def"
			   :recurse nil))
		     ;;Chewie list getter
		     #'chewie:tht:dynamic-1s+1rec->list)))
	    
	    (emtb:buf-contents-matches
	       :string "abc(def())"))))

   ;;Version of the other test, changed to have hierarchical root.
   (  "Demonstrates: updating by altering objects' simple data."
      (with-temp-buffer
	 (labels
	    ;;Formatter function for list of `wookie:th:1s',
	    ;;corresponding to the structure of `root'.
	    ;;$$CHANGE ME.  This must have a wookie-like object as
	    ;;root.  So how does it recurse to doubles, then?
	    ;;Make a different set of handlers just for this test?
	    ;;Change to use the recursive form, as the other one did?
	    ;;This is unused now
	    ((format-f
		;;Formatting function will change.  Not clear any more
		;;what its proper role is.
		(obj data)
		(list
		   `(dynamic ,(car obj) ()
		       endor:th:format-1s)
		   "-"
		   `(dynamic ,(second obj) ()
		       endor:th:format-1s)))
	       (reset (wookie obj str)
		  (setf
		     (endor:tht:1s+1rec->str obj) str)
		  (chewie:redisplay wookie obj)))
	    
	    (let*
	       ;;Root contains two objects.  
	       ((endor:th:do-checks t)
		  (root
		   (chewie:make-tht:dynamic-1s+1rec
		      :list (chewie:2:make-list)
		      :str "abc"
		      :recurse
		      (chewie:make-tht:dynamic-1s+1rec
			 :list (chewie:2:make-list)
			 :str "def"
			 :recurse nil)))
		  (wookie
		     (chewie:th:make-usual-chewie
			#'chewie:th:format-dynamic-1s+1rec
			root
			#'chewie:tht:dynamic-1s+1rec->list)))
	       
	       ;;Validate original contents.
	       (assert
		  (emtb:buf-contents-matches
		     :string "abc(def())")
		  t)

	       ;;$$FIXME We are getting extra nils here.

	       '((nil "") (nil nil) (nil nil) (1 "abc") (4 "-") 
		   (nil nil) (5 "def") (nil "")) 

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

   ;;This test had to be changed because it "simplified" by making
   ;;root a list, which we have no handler for.
   ;;Updating
   '
   (  "Demonstrates: updating by altering objects' simple data."
      (with-temp-buffer
	 (labels
	    ;;Formatter function for list of `wookie:th:1s',
	    ;;corresponding to the structure of `root'.
	    ;;$$CHANGE ME.  This must have a wookie-like object as
	    ;;root.  So how does it recurse to doubles, then?
	    ;;Make a different set of handlers just for this test?
	    ;;Change to use the recursive form, as the other one did?
	    ((format-f
		(obj data)
		(list
		   `(dynamic ,(car obj) ()
		       endor:th:format-1s)
		   "-"
		   `(dynamic ,(second obj) ()
		       endor:th:format-1s))))
	    (let*
	       ;;Root contains two objects.  
	       (  (endor:th:do-checks t)
		  (root
		   (list
		      (chewie:make-tht:dynamic-1s
			 :list (chewie:2:make-list)
			 :str "abc")
		      (chewie:make-tht:dynamic-1s
			 :list (chewie:2:make-list)
			 :str "def")))
		  (wookie
		     (chewie:th:make-usual-chewie
			#'format-f
			root
			#'chewie:tht:dynamic-1s->list)))
	       
	       ;;Validate original contents.
	       (assert
		  (emtb:buf-contents-matches
		     :string "abc-def")
		  t)

	       ;;$$FIXME The nils may be placeholders that didn't go
	       ;;away.

	       '((nil "") (nil nil) (nil nil) (1 "abc") (4 "-") 
		   (nil nil) (5 "def") (nil "")) 

	       (endor:th:get-position-skeleton 
		  wookie)
	       
	       (setf
		  (endor:tht:1s->str (car root))
		  "ghi")
	       (chewie:redisplay wookie (car root))

	       ;;Check buffer. 
	       (assert
		  (emtb:buf-contents-matches
		     :string "ghi-def")
		  t)
	       ;;Update the other.
	       (setf
		  (endor:tht:1s->str (cadr root))
		  "jkl")
	       (chewie:redisplay wookie (cadr root))

	       ;;Check buffer. 
	       (assert
		  (emtb:buf-contents-matches
		     :string "ghi-jkl")
		  t)
	       t))))


   ;;DORMANT Test is not consistent with the new design.  It could be
   ;;made consistent with another design of chewie where the dynamic
   ;;objects do not hold their own extra info, but that info is held
   ;;in association with the objects.
   '  
   (  "Demonstrates: updating by altering dynamic objects held by others."
      (with-temp-buffer
	 (labels
	    ;;Formatter function for list of `wookie:th:1s',
	    ;;corresponding to the structure of `root'.  But root
	    ;;needs to be dynamic too.
	    ((format-f
		(obj data)
		(list
		   `(dynamic ,(car obj) ()
		       endor:th:format-1s)
		   "-"
		   `(dynamic ,(second obj) ()
		       endor:th:format-1s))))
	    (let*
	       ;;Root contains two objects.  
	       (  (endor:th:do-checks t)
		  (root
		   (list
		      (chewie:make-tht:dynamic-1s
			 :list (chewie:2:make-list)
			 :str "abc")
		      (chewie:make-tht:dynamic-1s
			 :list (chewie:2:make-list)
			 :str "def")))
		  (wookie
		     (chewie:th:make-usual-chewie
			#'format-f
			root
			#'chewie:tht:dynamic-1s->list)))
	       
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
	       
	       (chewie:redisplay wookie root)
	       
	       ;;Check buffer. 
	       (assert
		  (emtb:buf-contents-matches
		     :string "ghi-def")
		  t)

	       t))))

   (  "Demonstrates: Removing objects can work."
      (with-temp-buffer
	 (let*
	    (  (endor:th:do-checks t)
	       (root
		  ;;Root contains an object of its own type.
		  (chewie:make-tht:dynamic-1s+1rec
		     :list (chewie:2:make-list)
		     :str "abc"
		     :recurse
		     (chewie:make-tht:dynamic-1s+1rec
			:list (chewie:2:make-list)
			:str "def"
			:recurse nil)))
	       (wookie
		  (chewie:th:make-usual-chewie
		     ;;Format function interprets this type of object.  It
		     ;;is passed data too.
		     #'chewie:th:format-dynamic-1s+1rec
		     root
		     ;;Chewie list getter
		     #'chewie:tht:dynamic-1s+1rec->list)))
	    
	    (emtb:buf-contents-matches
	       :string "abc(def())")

	    ;;Remove the child item and undisplay it
	    (let
	       ((child
		   (chewie:tht:dynamic-1s+1rec->recurse root)))
	       (setf (chewie:tht:dynamic-1s+1rec->recurse root) nil)
	       (chewie:display-gone wookie child))
	    
	    ;;Check contents
	    (emtb:buf-contents-matches
	       :string "abc()")

	    ;;Replace the child item with "ghi" instead.  Redisplay
	    ;;its parent (because only caller knows what holds that
	    ;;node)
	    (let
	       ((new-child
		   (chewie:make-tht:dynamic-1s+1rec
		      :list (chewie:2:make-list)
		      :str "ghi"
		      :recurse nil)))
	       (setf (chewie:tht:dynamic-1s+1rec->recurse root) new-child)
	       (chewie:redisplay wookie root))

	    ;;New node should be visible in the right place.
	    (emtb:buf-contents-matches
	       :string "abc(ghi())")	    
	    )))
   



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
Operation: Redisplay root, via chewlist.
Response: Buffer display is as expected.
Chewlist still holds only one display."
      (with-temp-buffer
	 (let*
	    (  (endor:th:do-checks t)
	       (chewlist (chewie:2:make-list))
	       (root '(("abcd")))
	       (chewie
		  (chewie:th:make-usual-chewie
		     ;;Formatter just returns list
		     #'(lambda (obj &rest r)
			  (check-type obj (list (repeat string)) 
			     "List of strings")
			  (car obj))
		     root
		     #'(lambda (&rest r) chewlist)
		     ;;Use an ewoc displayer that expects strings
		     #'(lambda (x)
			  (typecase x
			     (string
				(insert x)))))))
	    (setf (car root) '("abc"))
	    (chewie:redisplay chewie root)
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc"))
	    (assert
	       (=
		  (length
		     (chewie:2:list->displayers chewlist))
		  1)
	       t)
	    t)))

   (  "Situation: Root has been set and displayed.
We are using symbols and displaying symbols directly as their names.
Operation: Redisplay root, via chewlist.
Response: Buffer display is as expected.
Chewlist still holds only one display."
      (with-temp-buffer
	 (let*
	    (  (endor:th:do-checks t)
	       (chewlist (chewie:2:make-list))
	       (root '((a b c d)))
	       (chewie
		  (chewie:th:make-usual-chewie
		     ;;Formatter just returns list
		     #'(lambda (obj &rest r)
			  (check-type obj (list (repeat symbol)) 
			     "List of symbols")
			  (car obj))
		     root
		     #'(lambda (&rest r) chewlist)
		     ;;Use an ewoc displayer that expects symbols.
		     #'(lambda (x)
			  (typecase x
			     (symbol
				(insert (symbol-name x))))))))
	    (setf (car root) '(a b c))
	    (chewie:redisplay chewie root)
	    (assert
	       (emtb:buf-contents-matches
		  :string "abc"))
	    (assert
	       (=
		  (length
		     (chewie:2:list->displayers chewlist))
		  1)
	       t)
	    t)))
   


   ;;Test use of align-lists
   ;;Dormant for the moment.
   '
   (  "Operation: Replace stuff
Result: Just the expected sub-operations are used.
Shows: It minimizes sub-operations."
      (with-temp-buffer
	 (let*
	    (  (endor:th:do-checks t)
	       (chewlist (chewie:2:make-list))
	       (root '((a b c d)))
	       (chewie
		  (chewie:th:make-usual-chewie
		     ;;Formatter just returns list
		     #'(lambda (obj &rest r)
			  (check-type obj (list (repeat symbol)) 
			     "List of symbols")
			  (car obj))
		     root
		     #'(lambda (&rest r) chewlist)
		     ;;Use an ewoc displayer that expects symbols.
		     #'(lambda (x)
			  (typecase x
			     (symbol
				(insert (symbol-name x))))))))
	    
	    (let
	       (
		  (deleted '())
		  (displayed '())
		  (make-noded '()))
	       (emtp:eval 
		  ;;As implied above, this call finds chewlist because
		  ;;that's in a special variable.  So it does find the
		  ;;display. 
		  (chewie:redisplay chewie '(a b c))
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
	       
	       (assert (equal deleted    '(d)) t)
	       (assert (equal make-noded '()) t)
	       (assert (equal displayed  '()) t))
      
	    t)))
   


   
   

   )

;;;_. Footers
;;;_ , Provides

(provide 'viewers/chewie/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/chewie/tests.el ends here
