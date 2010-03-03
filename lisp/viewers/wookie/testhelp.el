;;;_ viewers/wookie/testhelp.el --- Test support for wookie

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
(require 'viewers/wookie)
(require 'viewers/ewoc/testhelp)

;;;_. Body

;;;_ , Test structures
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
   '
   (wookie:make-displayable 
      :data obj
      :held-outside-p nil)
   ;;After the change
   obj)
;;;_  . endor:th:handler-alist
;;Trivial handlers for testing Endor.  Object type is any list, and
;;will be printed with parentheses around it.
'
(defconst endor:th:handler-alist
   `(
       (display
	  ,#'(lambda (wookie node)
		(dolist (child node)
		   (wookie:display-one child wookie))))
       
       (delete
	  ,#'(lambda (wookie node)
		(dolist (child node)
		   (wookie:delete-either wookie child))))
       
       (get-left-ewoc
	  ,#'(lambda (wookie node)
		(wookie:dispatch 'get-left-ewoc wookie (car node))))
       
       (make-node
	  ,#'(lambda (wookie o placeholder parent)
		;;Only make these nodes for lists of data
		(when
		   (consp o)
		   (let
		      ((ewoc (wookie:wookie->ewoc wookie))
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
		      node))))
       
       (match-type-p
	  ,#'(lambda (wookie node)
		(listp node))))
   
   "" )
;;;_  . endor:th:handler-alist Vtable version

(defconst endor:th:handler-alist
   (make-wookie:callback-table
      :match-type-p
      #'(lambda (wookie node)
	 (listp node))
      :display
      #'(lambda (wookie node)
	 (dolist (child node)
	    (wookie:display-one child wookie)))
      :delete
      #'(lambda (wookie node)
	 (dolist (child node)
	    (wookie:delete-either wookie child)))
      :get-left-ewoc
      #'(lambda (wookie node)
	 (wookie:dispatch 'get-left-ewoc wookie (car node)))
      :make-node
      #'(lambda (wookie o placeholder parent)
	 ;;Only make these nodes for lists of data
	 (when
	    (consp o)
	    (let
	       ((ewoc (wookie:wookie->ewoc wookie))
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
	       node)))))


;;;_  . wookie:th:make-usual-wookie
' ;;Obsolete
(defun wookie:th:make-usual-wookie (expander root &optional get-chewie-list)
   ""
   (wookie:create
      expander
      ;;Printer for ewoc.
      #'loformat:print
      :object 
      root
      ;;Make the root object be dynamic?
      ;;`(dynamic ,root nil ,expander)
      :get-chewie-list get-chewie-list
      :buf (current-buffer)
      :func-list (list #'chewie:handler)
      ;;Not for this
      ;;:handlers (list chewie:handler-alist)
;;       :showing-cb #'ignore
;;       :unshowing-cb #'ignore
      ))

;;But this is about Endor.
;;Expander doesn't do anything?  And root should entirely be the
;;data.  And we don't try to treat structures.
(defun wookie:th:make-usual-wookie-2 (expander root &optional get-chewie-list)
   ""
   (wookie:create
      ;;This will go away
      expander
      ;;Printer for ewoc.
      #'loformat:print
      :object root
      ;;This will go away
      :get-chewie-list get-chewie-list
      :buf (current-buffer)
      ;;:func-list (list #'chewie:handler)
      ;;Not for this
      :handlers 
      (list endor:th:handler-alist)
;;       :showing-cb #'ignore
;;       :unshowing-cb #'ignore
      ))

;;Is this too much work just to test wookie? (Or endor)?
;;For lists.
;;And the test is whether node is a list.
'`(make-node 
     ;;Enter everything on the list.
     ,#'(lambda (wookie o-list placeholder parent)
	   (let
	      ((ewoc (wookie:wookie->ewoc wookie)))
	      (dolist (obj o-list)
		 (ewoc-enter-before 
		    ewoc 
		    placeholder
		    obj)))))


;;;_ , Formatter functions
;;;_  . wookie:th:format-1s
(defun wookie:th:format-1s (obj data)
   "Format just prints the string field."
   (list 
      (wookie:tht:1s->str obj)))


;;;_  . wookie:th:format-1s+1rec-static
;;Contrast with `chewie:th:format-1s+1rec-dynamic' acting dynamically
;;on the same type.
(defun wookie:th:format-1s+1rec-static (obj)
   ""
   (mapcar
      #'wookie:th:->displayable
      (wookie:th:format-1s+1rec-static-x obj)))

;;;_  . wookie:th:format-1s+1rec-static
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

;;;_ , Inspection testhelp 
;;;_  . wookie-debug-get-position-skeleton
(defun wookie-debug-get-position-skeleton (tree)
   ""
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

;;;_  . wookie:th:children-linked-p
(defun wookie:th:children-linked-p (node)
   ""
   (let
      ((children (wookie:node->children node)))
   (if
      (listp children)
      (every
	 #'(lambda (x)
	      (etypecase x
		 (wookie:node
		    ;;We don't try to recurse.
		    t)
		 (vector
		    (ewoc:th:linked-p x))))
	 children)
      ;;Singleton
      (ewoc:th:linked-p children))))
;;;_  . wookie:either:th:all-linked-p
(defun wookie:either:th:all-linked-p (node)
   ""
   
   (etypecase node
      (wookie:node
	 (wookie:th:children-linked-p node))
      (vector 
	 (ewoc:th:linked-p node))))

;;;_  . wookie:th:show-parts
(defun wookie:th:show-parts (node &optional key)
   ""
   
   (etypecase node
      (wookie:node
	 (mapcar 
	    #'wookie:th:show-parts 
	    (wookie:node->children node)))
      (vector 
	 (ewoc--node-data node))))


;;;_. Footers
;;;_ , Provides

(provide 'viewers/wookie/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/wookie/testhelp.el ends here
