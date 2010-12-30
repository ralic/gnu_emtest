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
(defstruct (emtvp:th:derived-node
	      (:include emtvp:node)
	      (:constructor emtvp:th:make-derived-node)
	      (:conc-name emtvp:th:derived-node->)
	      (:copier nil))
   "A node derived from emtvp:node"
   data)


;;;_ , Pattern governors
(emtm:define-struct-governor 
   (emtvp:node
      (:constructor emtvp:make-node)
      (:conc-name emtvp:node->)
      (:copier nil))
   name path parent children data)
(emtm:define-struct-governor 
   (emtvp:th:derived-node
      (:include emtvp:node)
      (:constructor emtvp:th:make-derived-node)
      (:conc-name emtvp:th:derived-node->)
      (:copier nil))
   name path parent children data)
(emtm:define-struct-governor 
   (emtvp
      (:constructor emtvp:make)
      (:conc-name emtvp->)
      (:copier nil))
   root)
;;;_ , emtvp:th:type-correct-p Type checker
(defun emtvp:th:type-correct-p (tree)
   "Return non-nil just if the tree is a type-correct emtvp"
   (and
      (typep tree 'emtvp)
      (emtvp:th:subtree-typecorrect-p
	 (emtvp->root tree)
	 (emtvp->type tree))))

;;;_ , emtvp:th:subtree-typecorrect-p
(defun emtvp:th:subtree-typecorrect-p (node type)
   "Return non-nil just if the subtree is type-correct"
   (and
      (typep node type)
      (every
	 #'(lambda (x)
	      (emtvp:th:subtree-typecorrect-p x type))
	 (emtvp:node->children node))))


;;;_  . emtvp:th:callback:push
(defun emtvp:th:callback:push (x)
   "Callback for testing pathtree.
Store data about X on the list `*nodes-freshened*'.  Then clean up
   like a normal callback would must."
   (check-type x emtvp:node)
   (push  
      (list
	 (emtvp:th:derived-node->name x)
	 (emtvp:th:derived-node->data x)
	 (emtvp:th:derived-node->dirty-flags x))
      *nodes-freshened*)
   ;;Wipe out previous dirty-flags in case we are interested in later
   ;;operations. 
   (setf (emtvp:node->dirty-flags x) '())

   ;;Return the empty list, indicating that there are no forther
   ;;operations to perform.
   ())
;;;_  . emtvp:th:how-dirty
;;Usage: (assert (equal (emtvp:th:how-dirty Name) Expected) t)
;;Usage: (assert (emtm (emtvp:th:how-dirty Name) Expected) t)

;;But this can't be easily used when matching patterns.  Maybe if we
;;sort flags. 
(defun emtvp:th:how-dirty (name)
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
;;;_  . emtvp:th:assert-name-dirtiness
(defmacro emtvp:th:assert-name-dirtiness (name pattern)
   ""
   
   `(let
       ((how-dirty
	   (emtvp:th:how-dirty ,name)))
       (assert 
	  (emtm how-dirty ,pattern)
	  nil "Mismatch %S has %s" ,name how-dirty)))


;;;_  . emtvp:th:let-usuals
(defmacro emtvp:th:let-usuals (other-lets &rest body)
   ""
   
   `(let*
	 (  (*nodes-freshened* '())
	    (tree 
	       (emtvp:make-pathtree
		  #'emtvp:th:callback:push
		  'emtvp:th:derived-node))
	    ,@other-lets)
       ,@body))

;;;_  . emtvr:th:skeleton
(defun emtvr:th:skeleton (tree)
   "Return a skeleton of TREE"
   (check-type tree emtvp)
   (emtvr:th:skeleton-recurse (emtvp->root tree)))

;;;_  . emtvr:th:skeleton-recurse
(defun emtvr:th:skeleton-recurse (node)
   "Return a skeleton of NODE"
   (cons
      (emtvp:node->name node)
      (mapcar
	 #'emtvr:th:skeleton-recurse
      (emtvp:node->children node))))
;;;_  . emtvp:th:add/replace-node
;;$$NEW, for testing
(defun emtvp:th:add/replace-node (tree path arg)
   "Add a node"

   (let
      ((node 
	  (emtvp:find-node tree path 
	     #'(lambda() 
		  (emtvp:th:make-derived-node :data
		     "default-data")))))
      
      (emtvp:replace-node tree node
	 (emtvp:th:make-derived-node
	    :data arg))))

;;;_  . emtvp:th:add/replace-node-recurse
(defun emtvp:th:add/replace-node-recurse (tree node path arg)
   ""

   (let
      ((node 
	  (emtvp:find-node-under-node tree path node
	     #'(lambda() 
		  (emtvp:th:make-derived-node :data
		     "default-data")))))
      
      (emtvp:replace-node tree node
	 (emtvp:th:make-derived-node
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
