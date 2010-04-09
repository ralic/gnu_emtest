;;;_ viewers/hiformat/rtest.el --- Tests for hiformat

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

(require 'viewers/hiformat)

;;;_. Body

;;;_   , Test data
;;Define examples, some share the same functors.
;;;_   , Tests
(rtest:deftest hiformat:map

   ;;Data includes "index", "first", "last" (but may include other
   ;;elements) Can't neatly test that without creating "subset" for
   ;;emtm.
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

;;;_. Footers
;;;_ , Provides

(provide 'viewers/hiformat/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/hiformat/rtest.el ends here
