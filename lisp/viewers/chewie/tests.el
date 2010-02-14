;;;_ chewie/tests.el --- Tests for Chewie

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
(require 'chewie)
;;;_. Body
;;;_ , Help to set the usuals
;;May belong in testhelp.

;;;_ , Tests of chewie as a whole

(rtest:deftest chewie

   ;;Some of these tests just mirror "wookie" tests.

   (  "Proves: Basic expansion works."
      (with-temp-buffer
	 ;;Set up a root.
	 (chewie:setup-root 
	    ;;Dummy object and alist
	    0
	    ()
	    ;;Format function returns a list of one constant string.
	    #'(lambda (&rest r)
		 (list "abc")))
	 
	 (mockbuf:buf-contents-matches
	    :string "abc")))
   ;;
   (  "Proves: Will skip `nil' as a component."
      (with-temp-buffer
	 ;;Set up a root.
	 (chewie:setup-root 
	    ;;Dummy object and alist
	    0
	    ()
	    ;;Format function returns a list of one nil
	    #'(lambda (&rest r)
		 (list nil)))
	 
	 (mockbuf:buf-contents-matches
	    :string "")))

   
   (  "Proves: Nested expansion works."
      (with-temp-buffer
	 ;;Set up
	 (chewie:setup-root 
	    ;;Dummy object and alist
	    0
	    () 
	    ;;Format function returns a list of one constant string.
	    #'(lambda (&rest r)
		 (list 
		    "abc"
		    `(dynamic 0 () 
			,#'(lambda (&rest r)
			     (list "def")))
		    "ghi")))
	 
	 (mockbuf:buf-contents-matches
	    :string "abcdefghi")))
   


   (  "Proves: Root object's value is available."
      (with-temp-buffer
	 ;;Set up a root.
	 (chewie:setup-root 
	    ;;Root is a string
	    "abc"
	    ;;Empty alist
	    ()
	    ;;Format function returns a list of one constant string.
	    #'(lambda (obj data)
		 (list obj)))
	 
	 (mockbuf:buf-contents-matches
	    :string "abc")))

   ;;$$MOVE ME maybe. It's really a test of interaction with LOAL, not
   ;;of chewie.  Maybe into "emformat"
   (  "Proves: Alist's value is available."
      (with-temp-buffer
	 ;;Set up a root.
	 (chewie:setup-root
	    ;;Dummy object
	    0
	    (loal:acons 'my-key "abc" '())
	    ;;Format function returns a list of one string obtained
	    ;;from data.
	    #'(lambda (obj data)
		 (list 
		    (loal:val 'my-key data "Wrongwrong"))))
	 
	 (mockbuf:buf-contents-matches
	    :string "abc")))

   (  "Proves: Can recurse hierarchically."
      (with-temp-buffer
	 ;;Root contains an object of its own type.
	 (let
	    ((chewie
		(chewie:setup-root 
		   (wookie:make-tht:1s+1rec
		      :str "abc"
		      :recurse
		      (wookie:make-tht:1s+1rec
			 :str "def"
			 :recurse nil))
		   ;;Empty alist
		   ()
		   #'chewie:th:format-1s+1rec-dynamic)))
	    	 
	 (mockbuf:buf-contents-matches
	    :string "abc(def())"))))


   ;;Updating
   (  "Demonstrates: updating by altering objects' simple data."
      (with-temp-buffer
	 (labels
	    ;;Formatter function for list of `wookie:th:1s',
	    ;;corresponding to the structure of `root'
	    ((format-f
		(obj data)
		(list
		   `(dynamic ,(car obj) ()
		       chewie:th:format-1s)
		   "-"
		   `(dynamic ,(second obj) ()
		       chewie:th:format-1s))))
	    (let*
	       ;;Root contains two objects.  
	       ((root
		   (list
		      (wookie:make-tht:1s
			 :str "abc")
		      (wookie:make-tht:1s
			 :str "def")))
		  (chewie
		     (chewie:setup-root 
			root
			()
			#'format-f)))
	       
	       ;;Validate original contents.
	       (assert
		  (mockbuf:buf-contents-matches
		     :string "abc-def")
		  t)

	       ;;$$FIXME The nils may be placeholders that didn't go
	       ;;away.

	       '((nil "") (nil nil) (nil nil) (1 "abc") (4 "-") 
		   (nil nil) (5 "def") (nil "")) 

	       (wookie-debug-get-position-skeleton 
		  (chewie:chewie->wookie chewie))
	       
	       (setf
		  (wookie:tht:1s->str (car root))
		  "ghi")
	       (chewie:freshen-obj chewie (car root))

	       ;;Check buffer. 
	       (assert
		  (mockbuf:buf-contents-matches
		     :string "ghi-def")
		  t)
	       ;;Update the other.
	       (setf
		  (wookie:tht:1s->str (cadr root))
		  "jkl")
	       (chewie:freshen-obj chewie (cadr root))

	       ;;Check buffer. 
	       (assert
		  (mockbuf:buf-contents-matches
		     :string "ghi-jkl")
		  t)
	       t))))
   

   (  "Demonstrates: updating by altering dynamic objects held by others."
      (with-temp-buffer
	 (labels
	    ;;Formatter function for list of `wookie:th:1s',
	    ;;corresponding to the structure of `root'
	    ((format-f
		(obj data)
		(list
		   `(dynamic ,(car obj) ()
		       chewie:th:format-1s)
		   "-"
		   `(dynamic ,(second obj) ()
		       chewie:th:format-1s))))
	    (let*
	       ;;Root contains two objects.  
	       ((root
		   (list
		      (wookie:make-tht:1s
			 :str "abc")
		      (wookie:make-tht:1s
			 :str "def")))
		  (chewie
		     (chewie:setup-root 
			root
			()
			#'format-f)))
	       
	       ;;Validate original contents.
	       (assert
		  (mockbuf:buf-contents-matches
		     :string "abc-def")
		  t)

	       ;;Even though one of the delegated objects was entirely
	       ;;removed, this must still come out OK.  It's possible,
	       ;;however, that some of it may be invisible, so turn
	       ;;the invisibility trick off if it's used.

	       (setf
		  (car root)
		  (wookie:make-tht:1s
		     :str "ghi"))
	       (chewie:freshen-obj chewie root)
	       
	       ;;Check buffer. 
	       (assert
		  (mockbuf:buf-contents-matches
		     :string "ghi-def")
		  t)

	       t))))


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

(provide 'chewie/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; chewie/tests.el ends here
