;;;_ emtest/editing/trail/testhelp.el --- Testhelp for trail

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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

(require 'emtest/editing/trail)

;;;_. Body

;;;_   , Test helpers 
;;;_    . emter:build-code:th
(defun emter:build-code:th (input-list)
   ""
   
   (emtp:eval
      (dolist (input input-list)
	 ;;Build item.
	 (destructuring-bind (should &rest expression) input
	    (emter:record-item expression (eval expression))
	    ;;SHOULD tells us to set up a should.
	    (when should
	       (setf
		  (emter:item->should
		     (car (last emter:value-list)))
		  t))))
      ;;Testpoint to skip displaying the item.
      (tp* (:id tp:8efac1de-09d4-4865-bea2-d3df2b211ad3 :count nil)())))

;;;_    . emter:build-code:thm
(defmacro emter:build-code:thm (input-list bindings &rest body)
   "Run BODY with `emter:value-list' in a particular state.
INPUT-LIST elements are in the form (SHOULD . EXPRESSION).
BINDINGS are as for a `let'"
   
   `(let ((emter:value-list ())
	    ,@bindings)
       (emter:build-code:th ,input-list)
       ,@body))
;;;_    . emter:build-code:th2
(defun emter:build-code:th2 (precedence-pairs)
   ""
   (emtp:eval
      (emter:build-code)
      ;;Testpoint per dependency
      (tp*
	 (:id tp:ea524bc6-1e65-4c90-8772-ab94c916a375
	    :count 
	    (length precedence-pairs))
	 (got-index want-index type)
	 (emt:tp:collect (list got-index want-index type)))
      (finally (:bindings ((precedence-pairs precedence-pairs))) (depend-set)
	 (assert
	    (emth:sets= depend-set 
	       precedence-pairs)
	    t))))
;;;_    . emter:build-code:thm2
(defmacro* emter:build-code:thm2 
   ((&key inputs bindings precedence-pairs sym) &rest body)
   ""
   
   `(emter:build-code:thm
       ,inputs
       ,bindings
       (let
	  ((,sym
	      (emter:build-code:th2 ,precedence-pairs)))
	  ,@body)))


;;;_    . emter:build-code:th:w-bindings-passes-p
(defun emter:build-code:th:w-bindings-passes-p 
   (bindings should-pass-p code)
   ""

   (let*
      ((form
	  ;;Bind the values CODE sees.
	  `(let 
	      (,@bindings)
	      ;;Code is a single form, not a list.
	      ,code))
	 ;;Eval it.  Does it err?
	 (errs-p
	    (emth:gives-error
	       (eval form))))
      
      ;;Test that it errored just if it should.
      (if should-pass-p
	 (assert (not errs-p)
	    nil "Form %s failed" form)
	 (assert errs-p
	    nil "Form %s wrongly succeeded" form))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/trail/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/trail/testhelp.el ends here
