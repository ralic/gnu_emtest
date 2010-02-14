;;;_ pathtree.el --- Pathtree library

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

(require 'pending)
(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

;;;_. Body
;;;_ , Pathtree
;;;_  . Types
;;;_   , emt:view:pathtree-id-element
;;$$FIXME These types should be gotten by TESTRAL from here, not vv.
;;Or both from a common source.  It is a presentation element anyways.
(deftype emt:view:pathtree-id-element ()
   'emt:testral:id-element)
;;;_   , emt:view:pathtree-node
(defstruct emt:view:pathtree-node
   "A node in a pathtree"
   (name ()      :type emt:view:pathtree-id-element)
   ;;$$RETHINK ME
   ;;Is this full path ever actually useful?
   (path ()      :type (repeat emt:view:pathtree-id-element)) 

   (parent () :type (or null emt:view:pathtree-node))
   ;;Presented children
   (children () 
      :type (repeat emt:view:pathtree-node))
   (dirty-flags ()
      :type (repeat symbol))
   (data () :type t))
;;;_   , emt:view:pathtree
(defstruct emt:view:pathtree
   "A pathtree object"
   (root ()         :type emt:view:pathtree-node)
   (redisplay-cb () :type function
      :doc 
      "REDISPLAY-CB is a function to cause redisplay of a node.  
It takes one argument.")  ;;To be replaced.
   (cleaner-cb   () :type function)  ;;New, unused
   ;;Make an empty data element
   (make-data () :type function)
   ;;The type of `data' elements.   Check this at appropriate places.
   (type t)
   (dirty () :type (repeat emt:view:pathtree-node)))



;;;_  . emt:pathtree:add/replace-node
(defun  emt:pathtree:add/replace-node (tree path cell)
   ""
   (check-type tree emt:view:pathtree)

   (assert 
      (typep cell (emt:view:pathtree-type tree))
      t)
   ;;To parallel check-type, should signal something like this:
   '(signal 'wrong-type-argument 
       (list (emt:view:pathtree-type tree) cell 'cell))
   (emt:pathtree:add/replace-node-recurse 
      tree (emt:view:pathtree-root tree) path cell path))
;;;_   , Tests
(put 'emt:pathtree:add/replace-node 'rtest:test-thru
   'emt:pathtree:add/replace-node-recurse)
;;;_  . emt:pathtree:add/replace-node-recurse
(defun emt:pathtree:add/replace-node-recurse 
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
	       (emt:view:pathtree-node-children node)
	       :key #'emt:view:pathtree-node-name
	       :test #'equal)))
      (if
	 child
	 ;;There's a child named NAME.
	 (if
	    tail
	    ;;Recurse.  Keep looking.
	    (emt:pathtree:add/replace-node-recurse 
	       tree child (cdr path) data full-path)

	    (let
	       ;;Punt for now.  This should retain the old data and
	       ;;old dirty-flags, but for now nothing is using those.
	       ((old-data)
		  (old-dirty-flags))
	       ;;Make it point to this data.
	       (setf (emt:view:pathtree-node-data child) data)
	       (push `(replaced ,old-data ,old-dirty-flags)
		  (emt:view:pathtree-node-dirty-flags child))
	       (push
		  child
		  (emt:view:pathtree-dirty tree))))
	 
	 ;;There is no child named NAME.
	 (if
	    tail
	    ;;Add a node to transitively contain the node we want to
	    ;;add.
	    (let
	       ((new-child
		   (emt:pathtree:add-child 
		      tree 
		      node 
		      name 
		      (funcall (emt:view:pathtree-make-data tree))
		      (subseq full-path (- (length tail))))))

	       ;;Look in the new child we made.  We know it's
	       ;;childless but treating it like it might have children
	       ;;is harmless.
	       (emt:pathtree:add/replace-node-recurse 
		  tree new-child (cdr path) data full-path))

	    ;;Add a new child corresponding to data
	    (let 
	       ((new-child
		   (emt:pathtree:add-child 
		      tree node name data full-path)))

	       new-child)))))

;;;_   , Test helpers
;;Moved to testhelp
;;;_   , Tests
;;Moved to tests

;;;_  . emt:pathtree:add-child
(defun emt:pathtree:add-child (tree parent name data full-path)
   "Adds a child node at the end of the children list"
   (let
      ((new-child
	  (make-emt:view:pathtree-node
	     :name name
	     :path full-path
	     :parent parent
	     :children '()
	     :dirty-flags '(new)
	     :data data)))
      (callf append
	 (emt:view:pathtree-node-children parent)
	 (list new-child))
      (push
	 new-child
	 (emt:view:pathtree-dirty tree))
      new-child))

;;;_  . emt:pathtree:remove-node-recurse
(defun emt:pathtree:remove-node-recurse (path)
   ""
   ;;Punt for now.  Not needed until we're much further along.
   (let*
      ()
      
      ))

;;;_  . emt:pathtree:make-empty-tree-newstyle
;;This is basically a ctor for `emt:view:pathtree' now
(defun emt:pathtree:make-empty-tree-newstyle (cleaner-cb make-data type)
   "Make an empty tree"
   (make-emt:view:pathtree
      :root
      (make-emt:view:pathtree-node
	 :name ""
	 :path '()
	 :parent nil
	 :children '()
	 :data (funcall make-data))
      :cleaner-cb cleaner-cb
      :make-data    make-data
      :type         type))

;;;_  . emt:pathtree:freshen
(defun emt:pathtree:freshen (tree)
   ""
   ;;This call empties the dirty list too.
   (pending:do-all
      (emt:view:pathtree-dirty tree)
      #'(lambda (el tree)
	   "Call the cleaner callback.  No-op if there are no dirty-flags."
	   (if
	      (emt:view:pathtree-node-dirty-flags el)
	      (funcall (emt:view:pathtree-cleaner-cb tree) el)
	      '()))
      (list tree)
      #'(lambda (unprocessed &rest args)
	   (format
	      "Couldn't process nodes %S"
	      (mapconcat
		 #'(lambda (x)
		      (emt:view:pathtree-node-name x))
		 unprocessed
		 "\n")))
      t))


;;;_ , Utilities to help define "cleaning" callbacks
;;;_  . emt:view:pathtree:util:match-as-car
(defun emt:view:pathtree:util:match-as-car (x el)
   ""
   (and 
      (listp el)
      (eq (car el) x)))

;;;_  . emt:view:pathtree:util:member-as-car
(defun emt:view:pathtree:util:member-as-car (elt list)
   ""
   (member* elt list
      :test #'emt:view:pathtree:util:match-as-car))



;;;_. Footers
;;;_ , Provides

(provide 'pathtree)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; pathtree.el ends here
