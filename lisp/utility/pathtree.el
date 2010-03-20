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
;;;_   , emtvp-id-element
;;$$FIXME These types should be gotten by TESTRAL from here, not vv.
;;Or both from a common source.  It is a presentation element anyways.
(deftype emtvp-id-element ()
   'emt:testral:id-element)
;;;_   , emtvp-node
(defstruct emtvp-node
   "A node in a pathtree"
   (name ()      :type emtvp-id-element)
   ;;$$RETHINK ME
   ;;Is this full path ever actually useful?
   (path ()      :type (repeat emtvp-id-element)) 

   (parent () :type (or null emtvp-node))
   ;;Presented children
   (children () 
      :type (repeat emtvp-node))
   (dirty-flags ()
      :type (repeat symbol))
   (data () :type t))
;;;_   , emtvp
(defstruct emtvp
   "A pathtree object"
   (root ()         :type emtvp-node)
   (redisplay-cb () :type function
      :doc 
      "REDISPLAY-CB is a function to cause redisplay of a node.  
It takes one argument.")  ;;To be replaced.
   (cleaner-cb   () :type function)  ;;New, unused
   ;;Make an empty data element
   (make-data () :type function)
   ;;The type of `data' elements.   Check this at appropriate places.
   (type t)
   (dirty () :type (repeat emtvp-node)))



;;;_  . emtvp:add/replace-node
(defun  emtvp:add/replace-node (tree path cell)
   ""
   (check-type tree emtvp)

   (assert 
      (typep cell (emtvp-type tree))
      t)
   ;;To parallel check-type, should signal something like this:
   '(signal 'wrong-type-argument 
       (list (emtvp-type tree) cell 'cell))
   (emtvp:add/replace-node-recurse 
      tree (emtvp-root tree) path cell path))
;;;_  . emtvp:add/replace-node-recurse
(defun emtvp:add/replace-node-recurse 
   (tree node path data full-path)
   "Add a node corresponding to PATH and DATA to the tree.
Create any required nodes that don't exist.

PATH must be a list."

   (let*
      (
	 (name (car path))
	 (tail (cdr path))
	 (child
	    (find name
	       (emtvp-node-children node)
	       :key #'emtvp-node-name
	       :test #'equal)))
      (if
	 child
	 ;;There's a child named NAME.
	 (if
	    tail
	    ;;Recurse.  Keep looking.
	    (emtvp:add/replace-node-recurse 
	       tree child (cdr path) data full-path)

	    (let
	       ;;Punt for now.  This should retain the old data and
	       ;;old dirty-flags, but for now nothing is using those.
	       ((old-data)
		  (old-dirty-flags))
	       ;;Make it point to this data.
	       (setf (emtvp-node-data child) data)
	       (push `(replaced ,old-data ,old-dirty-flags)
		  (emtvp-node-dirty-flags child))
	       (push
		  child
		  (emtvp-dirty tree))))
	 
	 ;;There is no child named NAME.
	 (if
	    tail
	    ;;Add a node to transitively contain the node we want to
	    ;;add.
	    (let
	       ((new-child
		   (emtvp:add-child 
		      tree 
		      node 
		      name 
		      (funcall (emtvp-make-data tree))
		      (subseq full-path (- (length tail))))))

	       ;;Look in the new child we made.  We know it's
	       ;;childless but treating it like it might have children
	       ;;is harmless.
	       (emtvp:add/replace-node-recurse 
		  tree new-child (cdr path) data full-path))

	    ;;Add a new child corresponding to data
	    (let 
	       ((new-child
		   (emtvp:add-child 
		      tree node name data full-path)))

	       new-child)))))

;;;_   , Test helpers
;;Moved to testhelp
;;;_   , Tests
;;Moved to tests

;;;_  . emtvp:add-child
(defun emtvp:add-child (tree parent name data full-path)
   "Adds a child node at the end of the children list"
   (let
      ((new-child
	  (make-emtvp-node
	     :name name
	     :path full-path
	     :parent parent
	     :children '()
	     :dirty-flags '(new)
	     :data data)))
      (callf append
	 (emtvp-node-children parent)
	 (list new-child))
      (push
	 new-child
	 (emtvp-dirty tree))
      new-child))

;;;_  . emtvp:remove-node-recurse
(defun emtvp:remove-node-recurse (path)
   ""
   ;;Punt for now.  Not needed until we're much further along.
   (let*
      ()
      
      ))

;;;_  . emtvp:make-empty-tree-newstyle
;;This is basically a ctor for `emtvp' now
(defun emtvp:make-empty-tree-newstyle (cleaner-cb make-data type)
   "Make an empty tree"
   (make-emtvp
      :root
      (make-emtvp-node
	 :name ""
	 :path '()
	 :parent nil
	 :children '()
	 :data (funcall make-data))
      :cleaner-cb cleaner-cb
      :make-data    make-data
      :type         type))

;;;_  . emtvp:freshen
(defun emtvp:freshen (tree)
   ""
   ;;This call empties the dirty list too.
   (pending:do-all
      (emtvp-dirty tree)
      #'(lambda (el tree)
	   "Call the cleaner callback.  No-op if there are no dirty-flags."
	   (if
	      (emtvp-node-dirty-flags el)
	      (funcall (emtvp-cleaner-cb tree) el)
	      '()))
      (list tree)
      #'(lambda (unprocessed &rest args)
	   (format
	      "Couldn't process nodes %S"
	      (mapconcat
		 #'(lambda (x)
		      (emtvp-node-name x))
		 unprocessed
		 "\n")))
      t))


;;;_ , Utilities to help define "cleaning" callbacks
;;;_  . emtvp:util:match-as-car
(defun emtvp:util:match-as-car (x el)
   ""
   (and 
      (listp el)
      (eq (car el) x)))

;;;_  . emtvp:util:member-as-car
(defun emtvp:util:member-as-car (elt list)
   ""
   (member* elt list
      :test #'emtvp:util:match-as-car))
;;;_  . emtvp:util:handle-dirty
(defmacro emtvp:util:handle-dirty (obj form)
   "Evaluate form with:
 * DIRTY-FLAGS bound to OBJ's dirty flags
 * NEW-DIRTY-NODES bound, but don't play with it

And with the following functions defined:

 * UNDIRTY - remove flag to this node's dirty flags
 * UNDIRTY-CAR - remove governor from this node's dirty
   flags, where the flag is of the form (GOVERNOR args...)
 * NEW-DIRTY - add flag to OBJ's dirty flags
 * NEW-DIRTY-NODE - add flag to another object's dirty flags."
   (let
      ((objsym (make-symbol "objsym")))
      `(let* 
	  (  (,objsym ,obj)
	     (dirty-flags (emtvp-node-dirty-flags ,objsym))
	     (new-dirty-nodes '()))
	  (flet
	     (  (undirty (flag)
		   (setq dirty-flags
		      (delete* flag dirty-flags)))
		(undirty-car (flag)
		   (setq dirty-flags
		      (delete* 
			 flag
			 dirty-flags 
			 :test #'emtvp:util:match-as-car)))
		(new-dirty (flag)
		   (push flag dirty-flags))
		(new-dirty-node (flag node)
		   (push flag (emtvp-node-dirty-flags node))
		   (push node new-dirty-nodes)))
	 
	     ,form)
      
	  (setf 
	     (emtvp-node-dirty-flags ,objsym) dirty-flags)
      

	  ;;Return the nodes we newly know are dirty.  If dirty-flags is
	  ;;non-nil, that includes this node.
	  (if dirty-flags
	     (cons ,objsym new-dirty-nodes)
	     new-dirty-nodes))))



;;;_. Footers
;;;_ , Provides

(provide 'utility/pathtree)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/pathtree.el ends here
