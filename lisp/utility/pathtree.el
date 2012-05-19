;;;_ utility/pathtree.el --- Pathtree library

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
(require 'utility/pending)

;;;_. Body
;;;_ , Pathtree
;;;_  . Types
;;;_   , pathtree:name-type
(deftype pathtree:name-type ()
   '(or null string symbol integer))
;;;_   , pathtree:node
(defstruct (pathtree:node
	      (:constructor pathtree:make-node)
	      (:conc-name pathtree:node->)
	      (:copier nil))
   "A node in a pathtree"
   (name ()      :type pathtree:name-type)
   (parent () :type (or null pathtree:node))
   (children () 
      :type (repeat pathtree:node))
   (dirty-flags ()
      :type (repeat symbol)))

;;;_   , pathtree
(defstruct (pathtree
	      (:constructor pathtree:make)
	      (:conc-name pathtree->)
	      (:copier nil))
   "A pathtree object"
   (root ()         :type pathtree:node)

   ;;*Callbacks*
   (node-dirtied   () 
      :type function
      :doc "Function to act on a node according to its dirty-flags
   field.

This function should not directly modify parent or children nodes,
other than by setting dirty flags in them and pushing them onto the
dirty list.")

   (foreign-data ())

   ;;*Misc*
   (type 'pathtree:node
      :doc "The type of nodes.   Used in testing and testhelping.
Must be derived from `pathtree:node'.")
   (dirty () 
      :type (repeat pathtree:node)
      :doc "Dirty-list of nodes that want updating via NODE-DIRTIED"))

;;;_  . Functions
;;;_   , pathtree:name=
(defalias 'pathtree:name= 'equal)
;;;_   , pathtree:make-pathtree
;;$$CHANGE MY ARGLIST Replace make-node and root-name with ROOT.
(defun pathtree:make-pathtree (node-dirtied make-node type &optional root-name)
   "Make an empty tree"
   (let
      ((root (funcall make-node nil nil)))
      (setf
	 (pathtree:node->name        root) (or root-name "")
	 (pathtree:node->parent      root) nil
	 (pathtree:node->dirty-flags root) '(new))
      (pathtree:make
	 :root         root
	 :node-dirtied node-dirtied
	 :type         type)))
;;;_   , Find nodes
;;;_    . pathtree:find-node
(defun pathtree:find-node (tree path make-empty-node)
   "Find a node at path PATH in TREE.
Make intervening nodes if they don't exist.
TREE must be a `pathtree'.
PATH must be a list of `pathtree:name-type'."
   
   (check-type tree pathtree)
   (check-type path (repeat pathtree:name-type))
   (pathtree:find-node-under-node
      tree path (pathtree->root tree) make-empty-node))

;;;_    . pathtree:find-node-under-node
(defun pathtree:find-node-under-node (tree path node make-empty-node)
   "Return a node at path PATH under node NODE.
The return value is suitable as a parent
Make intervening nodes if they don't exist.  

TREE must be a `pathtree'.
NODE must be a `pathtree:node' or descendant.
PATH must be a list of `pathtree:name-type'."

   (check-type tree   pathtree)
   (check-type path   (repeat pathtree:name-type))
   (check-type node   pathtree:node)
   
   (let*
      (
	 (name (car path))
	 (tail (cdr path))
	 (child
	    (or 
	       (find name
		  (pathtree:node->children node)
		  :key #'pathtree:node->name
		  :test #'pathtree:name=)
	       (pathtree:add-child 
		  tree 
		  node 
		  name 
		  (funcall make-empty-node)))))
      (if
	 tail
	 (pathtree:find-node-under-node tree tail child make-empty-node)
	 child)))
;;;_   , Add/remove nodes
;;;_    . pathtree:replace-node
(defun pathtree:replace-node (tree old-node new-node)
   "Replace OLD-NODE with NEW-NODE in TREE.
Error if OLD-NODE is the root or otherwise unparented."
   
   (let
      ((parent (pathtree:node->parent old-node)))
      (unless parent 
	 (error "Node %s must not be the root" old-node))
      (pathtree:remove-child tree parent old-node)
      (pathtree:add-child tree parent (pathtree:node->name new-node) new-node)))

;;;_    . pathtree:add-child
;;$$RETHINK MY ARGLIST don't take `name', caller should set it.
(defun pathtree:add-child (tree parent name new-child &optional prepend)
   "Add node NEW-CHILD at the end of PARENT's children.

To stitch NEW-CHILD in we set name, parent, and dirty-flags, but
don't otherwise alter it."
   (check-type tree   pathtree)
   (check-type parent pathtree:node)
   (check-type name   pathtree:name-type)
   
   (setf
      (pathtree:node->name        new-child) name
      (pathtree:node->parent      new-child) parent)

   ;;Don't set NEW-CHILD's children - the callback is allowed to set
   ;;them and expect them to be used.
   (if prepend
      (callf2 cons
	 new-child
	 (pathtree:node->children parent))
      (callf append
	 (pathtree:node->children parent)
	 (list new-child)))
   (pathtree:set-dirty tree new-child 'new)
   new-child)
;;;_    . pathtree:remove-child
(defun pathtree:remove-child (tree parent child)
   "Remove child CHILD of PARENT and return it."
   (check-type tree   pathtree)
   (check-type parent pathtree:node)
   (check-type child  pathtree:node)
      
   ;;Do the actual removal
   (callf2 delq child (pathtree:node->children parent))
   ;;Indicate what has been done to each node.
   (pathtree:set-dirty tree child 'lost-children)
   (pathtree:set-dirty tree child 'deleted))


;;;_ , Dirty flags
;;;_  . pathtree:set-dirty
(defun pathtree:set-dirty (tree node flag)
   "Mark NODE as dirty.  
FLAG says what type of dirtiness is marked"
   (progn
       (pushnew flag
	  (pathtree:node->dirty-flags node))
       (push node
	  (pathtree->dirty tree))))

;;;_  . pathtree:freshen
(defun pathtree:freshen (tree)
   ""
   ;;This call empties the dirty list too.
   (pending:do-all
      (pathtree->dirty tree)
      #'(lambda (el tree)
	   "Call the cleaner callback.  No-op if there are no dirty-flags."
	   (if
	      (pathtree:node->dirty-flags el)
	      (funcall (pathtree->node-dirtied tree) el tree)
	      '()))
      (list tree)
      #'(lambda (unprocessed &rest args)
	   (format
	      "Couldn't process nodes %S"
	      (mapconcat
		 #'(lambda (x)
		      (pathtree:node->name x))
		 unprocessed
		 "\n")))
      t))


;;;_ , Utilities to help define "cleaning" callbacks
;;;_  . pathtree:util:match-as-car
(defun pathtree:util:match-as-car (x el)
   ""
   (and 
      (listp el)
      (eq (car el) x)))

;;;_  . pathtree:util:member-as-car
(defun pathtree:util:member-as-car (elt list)
   ""
   (member* elt list
      :test #'pathtree:util:match-as-car))
;;;_  . pathtree:util:handle-dirty
(defmacro pathtree:util:handle-dirty (obj form)
   "Evaluate form with:
 * DIRTY-FLAGS bound to OBJ's dirty flags

And with the following functions defined:

 * UNDIRTY - remove flag to this node's dirty flags
 * UNDIRTY-CAR - remove governor from this node's dirty
   flags, where the flag is of the form (GOVERNOR args...)
 * NEW-DIRTY - add flag to OBJ's dirty flags
 * NEW-DIRTY-NODE - add flag to another object's dirty flags."
   (let
      ((objsym (make-symbol "objsym"))
	 (new-dirty-nodes (make-symbol "new-dirty-nodes")))
      
      `(let* 
	  (  (,objsym ,obj)
	     (dirty-flags (pathtree:node->dirty-flags ,objsym))
	     (,new-dirty-nodes '()))
	  (flet
	     (  (undirty (flag)
		   (setq dirty-flags
		      (delete* flag dirty-flags)))
		(undirty-car (flag)
		   (setq dirty-flags
		      (delete* 
			 flag
			 dirty-flags 
			 :test #'pathtree:util:match-as-car)))
		(new-dirty (flag)
		   (pushnew flag dirty-flags))
		(new-dirty-node (flag node)
		   (pushnew flag (pathtree:node->dirty-flags node))
		   (push node ,new-dirty-nodes)))
	 
	     ,form)
      
	  (setf 
	     (pathtree:node->dirty-flags ,objsym) dirty-flags)
      

	  ;;Return the nodes we newly know are dirty.  If dirty-flags is
	  ;;non-nil, that includes this node.
	  (if dirty-flags
	     (cons ,objsym ,new-dirty-nodes)
	     ,new-dirty-nodes))))



;;;_. Footers
;;;_ , Provides

(provide 'utility/pathtree)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/pathtree.el ends here
