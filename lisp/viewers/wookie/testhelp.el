;;;_ wookie/testhelp.el --- Test support for wookie

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

;;;_. Body

;;;_ , Test structures
;;$$RETHINK ME Do these apply the chewie, and wookie doesn't use them?
;;Or apply to wookie and are currently misnamed?

;;We make our own test structures because these want field types
;;covariant with formatter.  Using a vanilla test structure would
;;run the risk of mixing the different meanings.
(defstruct (wookie:tht:1s
	      (:constructor wookie:make-tht:1s)
	      (:conc-name wookie:tht:1s->))
   
   "Test-help structure with 1 string"
   str)

(defstruct (wookie:tht:1s+1rec
	      (:constructor wookie:make-tht:1s+1rec)
	      (:conc-name wookie:tht:1s+1rec->)	      
	      )
   "Test-help structure with 1 string, 1 recursion"
   str
   recurse)


;;;_ , Test helpers
;;;_  . wookie:th:->displayable
(defun wookie:th:->displayable (obj)
   ""
   (wookie:make-displayable 
      :data obj
      :held-outside-p nil))

;;;_  . wookie:th:format-1s+1rec-static
(defun wookie:th:format-1s+1rec-static (obj)
   ""
   (mapcar
      #'wookie:th:->displayable
      (wookie:th:format-1s+1rec-static-x obj)))

;;;_  . wookie:th:format-1s+1rec-static
;;Contrast with `chewie:th:format-1s+1rec-dynamic' acting dynamically
;;on the same type.
(defun wookie:th:format-1s+1rec-static-x (obj)
   "Format prints a string field, then statically recurses to a child if any."
   `(
       ,(wookie:tht:1s+1rec->str obj)
       "("
       ,@(let 
	    ((obj2 (wookie:tht:1s+1rec->recurse obj)))
	    (when obj2 
	       (wookie:th:format-1s+1rec-static-x obj2)))
       ")"))


;;;_  . wookie-debug-get-position-skeleton
(defun wookie-debug-get-position-skeleton (tree)
   ""
   ;;Requires the ewoc test helper.
   (require 'ewoc/testhelp)
   (labels
      (
	 (show-sub (x)
	    (typecase x
	       (string x)
	       (chewie:dynamic-obj
		  ;;No longer expected to recurse.
		  "*Unexpanded*")
	       (t nil))))
      (ewoc-debug-get-position-skeleton 
	 (wookie:wookie->ewoc tree)
	 #'show-sub)))



;;;_. Footers
;;;_ , Provides

(provide 'wookie/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; wookie/testhelp.el ends here
