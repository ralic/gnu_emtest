;;;_ utility/pathtree/testhelp.el --- Testhelp for pathtree

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
(require 'emtest/testhelp/match)
(require 'emtest/testhelp/deep-type-checker)
(require 'utility/pathtree)
;;;_. Body
;;;_ , Derived structure
(defstruct (pathtree:th:derived-node
	      (:include pathtree:node)
	      (:constructor pathtree:th:make-derived-node)
	      (:conc-name pathtree:th:derived-node->)
	      (:copier nil))
   "A node derived from pathtree:node"
   data)


;;;_ , Pattern governors
(emtm:define-struct-governor 
   (pathtree:node
      (:constructor pathtree:make-node)
      (:conc-name pathtree:node->)
      (:copier nil))
   name path parent children data)
(emtm:define-struct-governor 
   (pathtree:th:derived-node
      (:include pathtree:node)
      (:constructor pathtree:th:make-derived-node)
      (:conc-name pathtree:th:derived-node->)
      (:copier nil))
   name path parent children data)
(emtm:define-struct-governor 
   (pathtree
      (:constructor pathtree:make)
      (:conc-name pathtree->)
      (:copier nil))
   root)
;;;_ , pathtree:th:type-correct-p Type checker
(defun pathtree:th:type-correct-p (tree)
   "Return non-nil just if the tree is a type-correct pathtree"
   (and
      (typep tree 'pathtree)
      (pathtree:th:subtree-typecorrect-p
	 (pathtree->root tree)
	 (pathtree->type tree))))

;;;_ , pathtree:th:subtree-typecorrect-p
(defun pathtree:th:subtree-typecorrect-p (node type)
   "Return non-nil just if the subtree is type-correct"
   (and
      (typep node type)
      (every
	 #'(lambda (x)
	      (pathtree:th:subtree-typecorrect-p x type))
	 (pathtree:node->children node))))


;;;_  . pathtree:th:callback:push
(defun pathtree:th:callback:push (x)
   "Callback for testing pathtree.
Store data about X on the list `*nodes-freshened*'.  Then clean up
   like a normal callback would must."
   (check-type x pathtree:node)
   (push  
      (list
	 (pathtree:th:derived-node->name x)
	 (pathtree:th:derived-node->data x)
	 (pathtree:th:derived-node->dirty-flags x))
      *nodes-freshened*)
   ;;Wipe out previous dirty-flags in case we are interested in later
   ;;operations. 
   (setf (pathtree:node->dirty-flags x) '())

   ;;Return the empty list, indicating that there are no forther
   ;;operations to perform.
   ())
;;;_  . pathtree:th:how-dirty
;;Usage: (assert (equal (pathtree:th:how-dirty Name) Expected) t)
;;Usage: (assert (emtm (pathtree:th:how-dirty Name) Expected) t)

;;But this can't be easily used when matching patterns.  Maybe if we
;;sort flags. 
(defun pathtree:th:how-dirty (name)
   ""
   
   ;;Mapcar, giving applicable dirty-flags.  Empty list for other names.
   ;;No need to delete the empty lists.
   ;;Union the dirty-flags of `name'.  Ie, reduce them by union.
   (let*
      ((total-flags
	  (mapcar
	     #'(lambda (node)
		  (if
		     (string= name (car node))
		     (third node)
		     '()))

	     *nodes-freshened*)))
      (if total-flags
	 (reduce #'union total-flags)
	 '())))

;;;_   , Tests (of this helper)
;;See below
;;;_  . pathtree:th:assert-name-dirtiness
(defmacro pathtree:th:assert-name-dirtiness (name pattern)
   ""
   
   `(let
       ((how-dirty
	   (pathtree:th:how-dirty ,name)))
       (assert 
	  (emtm how-dirty ,pattern)
	  nil "Mismatch %S has %s" ,name how-dirty)))


;;;_  . pathtree:th:let-usuals
(defmacro pathtree:th:let-usuals (other-lets &rest body)
   ""
   
   `(let*
	 (  (*nodes-freshened* '())
	    (tree 
	       (pathtree:make-pathtree
		  #'pathtree:th:callback:push
		  #'(lambda(node data) 
		       (pathtree:th:make-derived-node
			  :data (or data "default-data")))
		  'pathtree:th:derived-node))
	    ,@other-lets)
       ,@body))

;;;_  . pathtree:th:skeleton
(defun pathtree:th:skeleton (tree)
   "Return a skeleton of TREE"
   (check-type tree pathtree)
   (pathtree:th:skeleton-recurse (pathtree->root tree)))

;;;_  . pathtree:th:skeleton-recurse
(defun pathtree:th:skeleton-recurse (node)
   "Return a skeleton of NODE"
   (cons
      (pathtree:node->name node)
      (mapcar
	 #'pathtree:th:skeleton-recurse
      (pathtree:node->children node))))
;;;_  . pathtree:th:add/replace-node
;;$$NEW, for testing
(defun pathtree:th:add/replace-node (tree path arg)
   "Add a node"

   (let
      ((node 
	  (pathtree:find-node tree path 
	     #'(lambda() 
		  (pathtree:th:make-derived-node :data
		     "default-data")))))
      
      (pathtree:replace-node tree node
	 (pathtree:th:make-derived-node
	    :data arg))))

;;;_  . pathtree:th:add/replace-node-recurse
(defun pathtree:th:add/replace-node-recurse (tree node path arg)
   ""

   (let
      ((node 
	  (pathtree:find-node-under-node tree path node
	     #'(lambda() 
		  (pathtree:th:make-derived-node :data
		     "default-data")))))
      
      (pathtree:replace-node tree node
	 (pathtree:th:make-derived-node
	    :data arg))))
;;;_. Footers
;;;_ , Provides

(provide 'utility/pathtree/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/pathtree/testhelp.el ends here
