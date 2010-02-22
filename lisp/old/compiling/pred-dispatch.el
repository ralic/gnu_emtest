;;;_ pred-dispatch.el --- Dispatch by a type predicate

;;;_. Headers
;;;_ , License
;; Copyright (C) 2008  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp

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
(require 'define-interface "compiling/define-interface.el")

;;;_. Body

(defalias 'emt:pred-dispatch-pred 'car)
(defalias 'emt:pred-dispatch-data 'cdr)
(defalias 'make-emt:pred-dispatch 'cons)

(defun emt:pred-dispatch:get-match (obj list)
   ""
   (let
      ((  match
	  (assoc* obj list
	     :test 
	     #'(lambda (obj el)
		  (funcall el obj)))))
      (if match
	 (emt:pred-dispatch-data match)
	 nil)))

;;;_ , Test-help data

(defconst emt:pred-dispatch:thd:list
   (list
      (make-emt:pred-dispatch
	 #'integerp
	 'integer)
      (make-emt:pred-dispatch
	 #'symbolp
	 'symbol))
   

   "List associating type predicates to symbols reflecting the type.
Each element is a pred-dispatch element" )


;;;_ , Tests

(rtest:deftest emt:pred-dispatch:get-match

   (  "Situation: There are no matching entries.
Response: Return nil."
      (equal
	 (emt:pred-dispatch:get-match 
	    "No strings types are available"
	    emt:pred-dispatch:thd:list)
	 nil))
   
   (  "Situation: There is a matching entry
Response: Return it."
      (equal
	 (emt:pred-dispatch:get-match 
	    12
	    emt:pred-dispatch:thd:list)
	 'integer))
      
   (  "Situation: There is a matching entry
Response: Return it."
      (equal
	 (emt:pred-dispatch:get-match 
	    'x
	    emt:pred-dispatch:thd:list)
	 'symbol))
   )



;;;_. Footers
;;;_ , Provides

(provide 'pred-dispatch)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; pred-dispatch.el ends here
