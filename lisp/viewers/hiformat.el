;;;_ viewers/hiformat.el --- Hi-level formatting functionality for Emtest

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
(eval-when-compile
   (require 'cl))

;;;_. Body
;;;_ , Types
;;;_  . hiformat:xformer
;;$$MOVE ME
(deftype hiformat:xformer ()
   "A formatting function.  
 * Takes an input object.
 * Takes a loal object.
 * Return value is a hiformat:format.

The type only demands a function, so it's mostly for
   documentation."
   '(satisfies functionp))

;;;_  . hiformat:format Format list type
;;$$OBSOLETE
(deftype hiformat:format ()
   "A format list"
   '(repeat
      (or
	 ;;This may change from `string', because it should be
	 ;;covariant with input type to loformat.
	 string 
	 (list 
	    (member dynamic)
	    t ;;Should be covariant with emformatter input type
	    loal
	    hiformat:xformer))))

;;;_ , Functions
;;;_  . hiformat:map Map over a list
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
			   (if (= index 0)        '((first t)) '())
			   (if (= index (1- len)) '((last t))  '())))
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


;;;_. Footers
;;;_ , Provides

(provide 'viewers/hiformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/hiformat.el ends here
