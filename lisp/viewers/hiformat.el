;;;_ hiformat.el --- Hi-level formatting functionality for Emtest

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
;;;_ , Types
;;;_  . hiformat:xformer
(deftype hiformat:xformer ()
   "A formatting function.  
 * Takes an input object.
 * Takes a loal object.
 * Return value is a hiformat:format.

The type only demands a function, so it's mostly for
   documentation."
   '(satisfies functionp))

;;;_  . hiformat:format Format list type
;;A separate thing
(deftype hiformat:format ()
   "A format list"
   (repeat
      (or
	 string ;;$$EXPANDME  The input type to loformat
	 (list 
	    (member dynamic) ;;Ie, what chewie provides.
	    t ;;Covariant with formatter input.
	    ;;$$FIXED This is an entire loal object not a
	    ;;`loal:single-alist' 
	    loal
	    hiformat:xformer))))

;;;_ , Functions
;;;_  . chewie:formatlist Map over a list
;;Perhaps more generally with a function to get next element.

(defun* hiformat:map (func list &key data-loal separator els=0 els=1)
   "Format LIST, using FUNC to transform each element.
FUNC must be a function taking 3 args:
 * Object
 * A loal which is all the passed-down data
 * Immediate data.  This is a list.  It will contain `first', `last'
   and `index' as appropriate.

DATA-LOAL must be a loal.

ELS=0 is used if the list is empty.  ELS=0 must be a formattable.

ELS=1, if given, is used if the list has just 1 element (a singleton).
ELS=1 must be a function taking 3 args (same signature as FUNC).

"

   (let*
      ((len (length list)))
      (cond
	 ((= len 0) els=0)  ;;Correct even if els=0 is nil
	 ((and (= len 1) els=1)
	    (funcall els=1 (car list) data-loal))
	 (t
	    (loop
	       for el in list
	       for index from 0
	       append
	       (let* 
		  ;;Create appropriate loop variables
		  (
		     (immediate-data
			(append
			   (list (list 'index index))
			   (if (= index 0) '((first t)) '())
			   (if (= index (1- len)) '((last t)) '())))
		     (sub-list
			(funcall func el data-loal immediate-data)))
	    
		  (if (= index 0) 
		     sub-list
		     (let
			((sep-form
			    (if (functionp separator)
			       (funcall separator data-loal immediate-data)
			       separator)))
			(append sep-form sub-list))))
	 
	       ;;Do we need a separate statement to get the results?
	       )))))

;;;_   , Test data
;;Define examples, some share the same functors.
;;;_   , Tests
(rtest:deftest hiformat:map

   ;;Data includes "index", "first", "last" (but may include other
   ;;elements) Can't neatly test that without creating "subset" for
   ;;emt-match.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn)
      )
   (  "Situation: Func and separator are trivial, just write 1 or 0.
PARAM: Separator is a formatted list.
Response: As expected."
      (progn
	 (assert
	    (equal
	       (hiformat:map
		  #'(lambda (&rest dummy)
		       '("1"))
		  '(a b c)
		  :separator
		  '("\n"))
	       '("1""\n""1""\n""1"))
	    t)
	 t))
   
   (  "Situation: Func and separator are trivial, just write 1 or 0.
PARAM: Separator is a function.
Response: As expected."
      (progn
	 (assert
	    (equal
	       (hiformat:map
		  #'(lambda (&rest dummy)
		       '("1"))
		  '(a b c)
		  :separator
		  #'(lambda (&rest dummy)
		       '("\n")))
	       '("1""\n""1""\n""1"))
	    t)
	 t))

   (  "Situation: Func and separator-f see data.
Response: As expected."
      (progn
	 (assert
	    (equal
	       (hiformat:map
		  #'(lambda (dummy-1 dummy-2 immediate-data-x)
		       (cond
			  ((assq 'first immediate-data-x)
			     '("FIRST"))
			  ((assq 'last immediate-data-x)
			     '("LAST"))
			  (t '("OTHER"))))
		   '(a b c)
		  :separator
		  #'(lambda (&rest dummy)
		       '("\n")))
	       '("FIRST""\n""OTHER""\n""LAST"))
	    t)
	 t))

   ;;Test ()
   ;;ELS=1 is given and length = 1

   (  "PARAM: ELS=0 is given
PARAM: LIST is empty
Response: The ELS=0 form is printed."
      (progn
	 (assert
	    (equal
	       (hiformat:map
		  #'(lambda (&rest dummy)
		       '("1"))
		  '()
		  :separator
		  #'(lambda (&rest dummy)
		       '("\n"))
		  :els=0 '("12" "144")
		  )
	       '("12" "144"))
	    t)
	 t))
   
   (  "PARAM: ELS=1 is given
PARAM: LIST has 1 element
Response: The ELS=1 function is called instead of the usual function."
      (progn
	 (assert
	    (equal
	       (hiformat:map
		  #'(lambda (&rest dummy)
		       '("1"))
		  '(0)
		  :separator
		  #'(lambda (&rest dummy)
		       '("\n"))
		  :els=1 
		  #'(lambda (dummy dummy-2)
		       '("12" "144"))
		  )
	       '("12" "144"))
	    t)
	 t))
   

   ;;Sees data-loal as param
   )

;;;_  . loformat functions (Living in hiformat for the moment)
;;;_   , loformat:print
;;You can tell it's loformat because it inserts, rather than make a
;;format string.
(defun loformat:print (x)
   ""
   (typecase x
      (string
	 (insert x))))

;;;_. Footers
;;;_ , Provides

(provide 'hiformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; hiformat.el ends here
