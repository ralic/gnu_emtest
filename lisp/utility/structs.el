;;;_ utility/structs.el --- Utilities bearing on structures.  Never finished.

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

;;;_ , Utilities
;;;_  . defstruct**
;;Use `emtm:define-struct-governor' as the skeleton of a general
;;struct parser?  It would take a callback.

;;$$RETHINK ME - My older struct package already pushed all the
;;defined info.  More easily, I could recover it from there.
(defmacro defstruct** (name fields)
   ""
   ;;Parse name for :conc-name and :predicate
   ;;Store that info too.
   ;;Particulars?  Name?
   `(defstruct ,name ,@fields
       
       ))
;;;_  . A specialized definer for TESTRAL note types (Not yet)

;;;_  . etypecase-w/accessor (Impossible right now)
;;Impossible because conc-name is not available.  Could define a
;;defstruct variant to set it in the information (And predicate name)
;;And maybe formatter for here.
(defmacro etypecase-w/accessor (obj accessor-sym &rest cases)
   "
ACCESSOR-SYM will be available as a macro in each case, bound to the
respective object."
   
   `(etypecase ,obj
       ,@(mapcar
	    #'(lambda (case)
		 `(macrolet 
		     ;;$$FIXME Don't let field-sym be captured
		     ((,accessor-sym (field-sym)
			 ,(etypecase-w/accessor-x
			     ;;$$FIXME Don't let obj be evaluated twice.
			     case 'field-sym ,obj)))
		     ,case))
	    cases)))
;;;_   , etypecase-w/accessor-x Helper (Impossible right now)
(defun etypecase-w/accessor-x (case field-sym obj)
   ""
   ;;$$FINISHME
   (let*
      ((type-sym (car case)))
      ;;If type-sym is a symbol and not null or t, proceed
      ;;CT a function call name.
      ;;$$FIXME:  Would use conc-name but it's not available.  So this
      ;;can't work.  Maybe another defstruct* macro to store that
      ;;information?  Check whether that exists anywhere.
      `(,(intern (concat (symbol-name type-sym) "-" (symbol-name field-sym)))
	  ,obj)))
;;;_   , Tests
;;$$WRITEME


;;;_. Footers
;;;_ , Provides

(provide 'utility/structs)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/structs.el ends here
