;;;_ wookie.el --- Emtest viewer that uses ewocs

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp, maint

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

(require 'ewoc)
(when (not (boundp 'ewoc-provides-variable-separator))
   (unless
      (require 'ewoc "231ewoc" t)
   (error "Needs an ewoc that provides variable separator")))

;;;_. Body
;;;_ , Data types

;;;_ , Wookie
;;;_  . wookie:wookie
(defstruct (wookie:wookie
	      (:constructor wookie:make-tree)
	      (:conc-name wookie:wookie->))
   "A wookie object"
   (root () :type wookie:node)
   (ewoc () :type ewoc)
   (expand-f () :type (satisfies functionp)
      :doc
      "EXPAND-F takes 2 args and returns a list of `wookie:displayable'
which will be the cores of ewocs or wookies.  Args are:

 * covariant node data
 * covariant overall data")
   (showing-cb   () :type (satisfies functionp)
      :doc "SHOWING-CB is called when a known-outside object is
displayed.  It takes 3 args: 
 * The node (a `wookie:either' which caller should treat as opaque)
 * The node's data (Type is covariant with the caller)
 * The DATA field here.
Its return value is ignored." )
   (unshowing-cb () :type (satisfies functionp)
      :doc
      "UNSHOWING-CB is called when a known-outside object is no longer
displayed.  Its signature is the same as SHOWING-CB.")
   
   (data ()
      :doc "Data passed to all the callbacks.  
\(Type is covariant with the caller")
   (pending () :type (repeat wookie:either)
      :doc "List of nodes that are waiting to be displayed."
      ))

;;;_  . wookie:node
(defstruct (wookie:node
	      (:constructor wookie:make-node)
	      (:conc-name wookie:node->))
   "A hierarchical ewoc-like node"
   (parent () :type wookie:node)
   (children () :type (or ewoc--node (repeat wookie:either)))
   (data ()
      :doc "The node data (Type is covariant with the caller)"))

;;;_  . wookie:either
(deftype wookie:either ()
   "Either type that can dynamically print etc"
   ;;We'd like to write `ewoc--node' instead of `vector' but
   ;;`ewoc--node' has no tag.
   '(or wookie:node vector))

;;;_  . wookie:displayable
(defstruct (wookie:displayable
	      (:constructor wookie:make-displayable)
	      (:conc-name wookie:displayable->)
	      (:type list))
   "The object that EXPAND-F returns a list of."
   data
   held-outside-p
   ;;$$CHANGING These two are now unused
   ;;callback
   ;;cb-data
   )

;;;_ , Entry points

;;;_  . wookie:get-placeholder-contents
(defun wookie:get-placeholder-contents ()
   "Get contents appropriate for a placeholder node"
   'placeholder)

;;;_  . wookie:create
(defun* wookie:create (expand-func ewoc-print-func 
			 &key buf object showing-cb unshowing-cb)
   "Create a wookie.
EXPAND-FUNC is a function of one argument.  It expands an object, returning a
list of `wookie:displayable'.

EWOC-PRINT-FUNC is a function taking 1 argument and inserting
some text at point.

OBJECT, if non-nil, is the root of the tree.  It must be a type that
EXPAND-FUNC accepts as an argument.

BUF is not handled yet."
   
   (let*
      (
	 (ewoc 
	    (ewoc-create ewoc-print-func nil nil ""))

	 (wookie
	    (wookie:make-tree
	       :root nil
	       :ewoc ewoc
	       :expand-f expand-func
	       :showing-cb showing-cb
	       :unshowing-cb unshowing-cb)))

      ;;Set the root just if it was given (non-nil)
      (when object (wookie:set-root wookie object))
      wookie))

;;;_  . wookie:set-root
(defun wookie:set-root (wookie object)
   "Set the root of a wookie.
Error if it has been set before."
   (when (wookie:wookie->root wookie)
      (error "Wookie root has already been set"))

   (let*
      (
	 (ewoc (wookie:wookie->ewoc wookie))
	 (placeholder
	    (ewoc-enter-last ewoc (wookie:get-placeholder-contents)))
	 (node
	    (wookie:make-node
	       :parent nil
	       :children placeholder
	       :data object)))
      
      (setf (wookie:wookie->root wookie) node)

      ;;Expand the root node immediately.
      (wookie:expand-one wookie node)
      ;;Expand any pending parts
      (wookie:display-pending wookie)
      wookie))

(put 'wookie:set-root 'rtest:test-thru
   'wookie)
;;;_  . Wookie pending-list functions

;;;_   , wookie:will-display-node
(defun wookie:will-display-node (wookie node)
   ""
   
   (push node (wookie:wookie->pending wookie)))


;;;_   , wookie:display-pending
(defun wookie:display-pending (wookie)
   ""
   (while (wookie:wookie->pending wookie)
      (wookie:display-one 
	 (pop (wookie:wookie->pending wookie)) 
	 wookie)))

;;;_   , wookie:display-one
(defun wookie:display-one (obj wookie)
   "Display or redisplay an ewoc node or a wookie node."
   (etypecase obj
      (wookie:node
	 (wookie:expand-one wookie obj))
      ;;Can't typecase ewoc--node, so assume any vector is one.
      (vector
	 (ewoc-invalidate (wookie:wookie->ewoc wookie) obj))))
;;;_  . Wookie callback management
;;;_   , wookie:held-outside-p
(defun wookie:held-outside-p (obj)
   "Non-nil just if the callbacks should be called on the node"
   (etypecase obj
      (wookie:node t)
      ;;Can't typecase ewoc--node, so assume any vector is one.
      (vector nil)))


;;;_  . Wookie children management
;;;_   , wookie:enter-new-children
(defun wookie:enter-new-children (wookie data-list following-ewoc-node parent)
   ""

   ;;`mapcar' traverses elements in order, so it's OK to use.
   (mapcar
      #'(lambda (d)
	   (let*
	      (  (data
		    (wookie:displayable->data d))
		 
		 (node
		    (if
		       (wookie:displayable->held-outside-p d)
		       ;;If held-outside-p, this node may become an
		       ;;inner node (it's not yet), so make a wookie.
		       (let*
			  (
			     (placeholder
				(ewoc-enter-before 
				   ewoc 
				   following-ewoc-node 
				   (wookie:get-placeholder-contents)))
			     (wookie-node
				(wookie:make-node
				   :parent parent
				   :children placeholder
				   :data data)))

			  ;;Queue the new node to be expanded later.
			  (wookie:will-display-node wookie wookie-node)
			  wookie-node)

		       ;;Otherwise make just an ewoc, which won't ever be
		       ;;expanded.  Ewoc displays it immediately so we
		       ;;needn't queue it.
		       (ewoc-enter-before 
			  ewoc 
			  following-ewoc-node 
			  data))))

	      (when
		 (and
		    (wookie:wookie->showing-cb wookie)
		    (wookie:held-outside-p node))
		 (funcall 
		    (wookie:wookie->showing-cb wookie)
		    node
		    data
		    (wookie:wookie->data wookie)))
	      

	      ;;Call back.  Client may want to know the nodes we make.
	      ;;Obsolete
	      ;; 	      '
	      ;; 	      (when (wookie:displayable->callback d)
	      ;; 		 (funcall 
	      ;; 		    (wookie:displayable->callback d)
	      ;; 		    node
	      ;; 		    (wookie:displayable->cb-data d)
	      ;; 		    (wookie:wookie->data wookie)))
	      node))
      data-list))
;;;_   , wookie:delete-either
(defun wookie:delete-either (wookie node)
   ""
   (check-type node wookie:either)
   (etypecase node
      (wookie:node
	 (wookie:delete-node wookie node))
      (vector
	 (ewoc-delete
	    (wookie:wookie->ewoc wookie)
	    node))))

;;;_   , wookie:delete-node
(defun wookie:delete-node (wookie node)
   ""
   (check-type node wookie:node)
   (dolist (child (wookie:node->children node))
      (wookie:delete-either wookie child)))

;;;_   , wookie:get-leftmost-sub-ewoc
(defun wookie:get-leftmost-sub-ewoc (wookie node)
   ""

   (let
      ((left-child (car (wookie:node->children node))))
      (etypecase left-child
	 (wookie:node
	    (wookie:get-leftmost-sub-ewoc wookie left-child))
	 (vector
	    left-child))))

;;;_   , wookie:expand-empty
(defun wookie:expand-empty (tree node)
   ""

   (let*
      (
	 (ewoc (wookie:wookie->ewoc tree))
	 (placeholder
	    (wookie:node->children node))
	 ;;Cannot use `ewoc-next' because we may really want to get
	 ;;the footer node.
	 (following-ewoc-node
	    (ewoc--node-right placeholder))
	 (data-list 
	    (funcall 
	       (wookie:wookie->expand-f tree)
	       (wookie:node->data node)))
	 (new-children
	    (wookie:enter-new-children
	       tree
	       data-list 
	       following-ewoc-node
	       node)))
      
      ;;Remove placeholder from dll
      (ewoc-delete ewoc placeholder)
      (setf (wookie:node->children node) new-children)

      ;;Return the wookie node.
      node)   
   )
;;;_   , wookie:expand-one

(defun wookie:expand-one (wookie node)
   "(Re)expand the node's children.

For now, we assume it always wants expansion.  Later we'll support
reprinting too, when it's not a placeholder."
   ;;We're really testing for it being a placeholder, indicated by a
   ;;bare ewoc--node, but ewoc--node is an untagged type so instead we
   ;;use `listp' to test for children
   (if
      (listp (wookie:node->children node))

      ;;Re-expand.  Later this may smartly re-use parts of the list.
      (let*
	 (  (ewoc (wookie:wookie->ewoc wookie))
	    (left-ewoc-node
	       (wookie:get-leftmost-sub-ewoc wookie node))
	    ;;Make a placeholder.
	    (placeholder
	       (ewoc-enter-before 
		  ewoc 
		  left-ewoc-node
		  (wookie:get-placeholder-contents))))
	 
	 ;;Remove the original children recursively.
	 (dolist (child (wookie:node->children node))
	    (wookie:delete-either wookie child))
	 ;;Now the wookie is empty again.
	 (setf (wookie:node->children node) placeholder)
	 (wookie:expand-empty wookie node))
      (wookie:expand-empty wookie node)))



;;;_. Footers
;;;_ , Provides

(provide 'wookie)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; wookie.el ends here
