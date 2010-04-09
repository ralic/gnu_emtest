;;;_ viewers/endor/testhelp.el --- Test support for wookie

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
(require 'viewers/endor)
(require 'viewers/ewoc/testhelp)

;;;_. Body
;;;_ , Control variable
(defvar endor:th:do-checks nil 
   "Control variable for endor checks.
If non-nil, do checks." )

;;;_ , Test structures
;;We make our own test structures because these want field types
;;covariant with formatter.  Using a vanilla test structure would
;;run the risk of mixing the different meanings.
(defstruct (endor:tht:1s
	      (:constructor endor:make-tht:1s)
	      (:conc-name endor:tht:1s->))
   
   "Test-help structure with 1 string"
   str)

(defstruct (endor:tht:1s+1rec
	      (:constructor endor:make-tht:1s+1rec)
	      (:conc-name endor:tht:1s+1rec->)	      
	      )
   "Test-help structure with 1 string, 1 recursion"
   str
   recurse)


;;;_ , Test helpers
;;;_  . endor:th:handler-alist

(defconst endor:th:handler-alist
   (endor:make-callback-table
      :node-tag nil ;;cl-struct-NAME-tags
      :node-type 'list
      :match-type-p
      #'(lambda (wookie node)
	 (listp node))
      :display
      #'(lambda (wookie node)
	 (dolist (child node)
	    (endor:display-one wookie child)))
      :delete
      #'(lambda (wookie node)
	 (dolist (child node)
	    (wookie:delete-either wookie child)))
      :get-left-ewoc
      #'(lambda (wookie node)
	 (endor:dispatch 'get-left-ewoc wookie (car node)))
      :make-node
      #'(lambda (wookie o placeholder parent)
	 ;;Only make these nodes for lists of data
	 (when
	    (consp o)
	    (let
	       ((ewoc (endor:endor->ewoc wookie))
		  (node
		     (mapcar
			#'(lambda (obj)
			     (ewoc-enter-before 
				ewoc 
				placeholder
				obj))
			`("(" ,@o ")"))))
	       ;;Remove the placeholder
	       (ewoc-delete ewoc placeholder)
	       node)))
      ;;Punt: always t.  Because this hasn't been useful yet.
      :linked-p
      #'(lambda (endor node) t)))



;;;_  . endor:th:make-usual-endor
(defun endor:th:make-usual-endor (root)
   ""
   (let
      ((endor
	  (endor:make-endor
	     :ewoc-print-func 
	     #'loformat:print
	     :other-handlers 
	     (list endor:th:handler-alist))))
      (when root
	 (endor:--set-root endor root))
      endor))

;;;_ , Formatter functions
;;;_  . endor:th:format-1s
(defun endor:th:format-1s (obj data)
   "Format just prints the string field."
   (list 
      (endor:tht:1s->str obj)))


;;;_  . endor:th:format-1s+1rec-static
;;Contrast with `chewie:th:format-1s+1rec-dynamic' acting dynamically
;;on the same type.
(defun endor:th:format-1s+1rec-static (obj)
   ""
   (endor:th:format-1s+1rec-static-x obj))

;;;_  . endor:th:format-1s+1rec-static
(defun endor:th:format-1s+1rec-static-x (obj)
   "Format prints a string field, then statically recurses to a child if any."
   `(
       ,(endor:tht:1s+1rec->str obj)
       "("
       ,@(let 
	    ((obj2 (endor:tht:1s+1rec->recurse obj)))
	    (when obj2 
	       (endor:th:format-1s+1rec-static-x obj2)))
       ")"))

;;;_ , Inspection testhelp 
;;;_  . endor:th:get-position-skeleton
(defun endor:th:get-position-skeleton (tree)
   ""
   (labels
      (
	 (show-sub (x)
	    (typecase x
	       (string x)
	       (symbol `(symbol ,x))
	       (t "*Unexpanded*"))))
      (ewoc-debug-get-position-skeleton 
	 (endor:endor->ewoc tree)
	 #'show-sub)))

;;;_  . endor:th:all-linked-p
(defun endor:th:all-linked-p (endor node)
   ""
   (endor:dispatch 'linked-p endor node))



;;;_. Footers
;;;_ , Provides

(provide 'viewers/endor/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/endor/testhelp.el ends here
