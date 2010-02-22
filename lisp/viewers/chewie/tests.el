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
;;;_ , Help to set the usuals
;;May belong in testhelp.

;;;_ , Tests of chewie as a whole

(rtest:deftest chewie

   ;;Contract with static expansion for plain "wookie".
   (  "Proves: Nested dynamic expansion works.
Dynamic objects are displayed immediately."
      (with-temp-buffer
	 ;;Set up
	 (let
	    ;;Dummy chewie list, since our objects here are just
	    ;;empty dummies.
	    ((chewlist (chewie:2:make-list)))

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
	    
	    (emtb:buf-contents-matches
	       :string "abcdefghi"))))
   

   (  "Proves: Can recurse hierarchically."
      (with-temp-buffer
	 ;;Root contains an object of its own type.
	 (let
	    (
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


   ;;Updating
   (  "Demonstrates: updating by altering objects' simple data."
      (with-temp-buffer
	 (labels
	    ;;Formatter function for list of `wookie:th:1s',
	    ;;corresponding to the structure of `root'.
	    ((format-f
		(obj data)
		(list
		   `(dynamic ,(car obj) ()
		       wookie:th:format-1s)
		   "-"
		   `(dynamic ,(second obj) ()
		       wookie:th:format-1s))))
	    (let*
	       ;;Root contains two objects.  
	       ((root
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

	       (wookie-debug-get-position-skeleton 
		  wookie)
	       
	       (setf
		  (wookie:tht:1s->str (car root))
		  "ghi")
	       (chewie:redisplay wookie (car root))

	       ;;Check buffer. 
	       (assert
		  (emtb:buf-contents-matches
		     :string "ghi-def")
		  t)
	       ;;Update the other.
	       (setf
		  (wookie:tht:1s->str (cadr root))
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
		       wookie:th:format-1s)
		   "-"
		   `(dynamic ,(second obj) ()
		       wookie:th:format-1s))))
	    (let*
	       ;;Root contains two objects.  
	       ((root
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
 		  (wookie:make-tht:1s :str "ghi"))
	       
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
	    (
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
