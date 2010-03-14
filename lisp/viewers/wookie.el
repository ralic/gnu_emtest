;;;_ viewers/wookie.el --- Emtest viewer that uses ewocs

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

(require 'viewers/endor)
(require 'viewers/align-lists) ;;For aligning the lists.
(eval-when-compile
   (require 'cl))


;;;_. Body
;;;_ , Data types

;;;_ , Wookie
;;;_  . wookie:wookie
(defstruct (wookie:wookie
	      (:constructor wookie:make-wookie
		 (&key
		    other-handlers
		    ewoc-print-func
		    data
		    expand-f
		    alist-mk-node
		    get-dlist
		    &aux
		    (ewoc
		       (ewoc-create ewoc-print-func nil nil ""))
		    (handlers
		       ;;$$RETHINK ME Maybe always include the wookie
		       ;;handlers.
		       (append
			  other-handlers
			  (list endor:ewoc-handler-alist)))))
	      (:constructor nil)
	      (:conc-name wookie:wookie->)
	      (:include endor:endor))
   
   "An Endor subtype that has dynamic redisplay"
   (expand-f      () :type function
      :doc
      "A covariant expander function")
   (alist-mk-node () :type (repeat (list symbol function))
      :doc
      "Alist from governor name to node-maker function")
   (get-dlist     () :type function
      :doc
      "Function, takes an object covariant with endor node data type
 and returns a wookie:dlist" ))

;;;_  . wookie:node
(defstruct (wookie:node
	      (:constructor wookie:make-node)
	      (:conc-name wookie:node->))
   "A hierarchical display node."
   (parent   () :type wookie:node)
   (children () :type (or ewoc--node (repeat t)))
   (hashes   () :type (repeat integer))
   (data ()
      :doc "The node data (Type is covariant with the caller)"))

;;;_  . wookie:dlist
(defstruct (wookie:dlist
	      (:constructor wookie:make-dlist)
	      (:conc-name wookie:dlist->)
	      (:copier nil))
   "List of current displayers.  Intended to live inside other
types of data nodes."
   (displayers () :type (repeat wookie:node)))

;;;_  . wookie:either
;;Type covaries with handlers
;;$$REMOVE ME later
;;Will go away, use the match-type-p command instead.
'
(deftype wookie:either ()
   "Either type that can dynamically print etc"
   ;;We'd like to write `ewoc--node' instead of `vector' but
   ;;`ewoc--node' has no tag.
   '(or wookie:node vector))

;;;_ , Entry points

;;;_  . Wookie children management
;;;_   , wookie:delete-either
(defsubst wookie:delete-either (wookie node)
   ""
   (endor:dispatch 'delete wookie node))

;;;_   , wookie:delete-node
(defun wookie:delete-node (wookie node)
   ""
   (endor:check (wookie:th:children-linked-p node))
   (check-type node wookie:node)
   (dolist (child (wookie:node->children node))
      (wookie:delete-either wookie child)))
;;;_  . Finding ewoc nodes

;;;_   , wookie:get-left-ewoc
(defsubst wookie:get-left-ewoc (wookie node)
   "Get leftmost ewoc of NODE, which can be any type of node.
Distinct from `wookie:get-leftmost-sub-ewoc' which assumes node is a
wookie node."
   (endor:dispatch 'get-left-ewoc wookie node))

;;;_   , wookie:get-leftmost-sub-ewoc
(defun wookie:get-leftmost-sub-ewoc (wookie node)
   "Get leftmost ewoc of NODE, which must be a wookie node.
Distinct from `wookie:get-left-ewoc' which does not assume node is a
wookie node."
   (endor:check (wookie:th:children-linked-p node))
   (assert (listp (wookie:node->children node)))
   (let
      ((left-child (car (wookie:node->children node))))
      (endor:dispatch 'get-left-ewoc wookie left-child)))


;;;_   , wookie:parent-get-next-ewoc
(defun wookie:parent-get-next-ewoc (wookie node)
   ""
   (let
      ((parent (wookie:node->parent node)))
      (if parent
	 (or 
	    (let 
	       ((found nil))
	       (catch 'wookie:next-ewoc
		  (progn
		     (dolist (ch (wookie:node->children parent))
			(if (not found)
			   ;;CH is before or at NODE, so skip it.
			   (when
			      (eq ch node)
			      (setq found t))
			   ;;CH is past NODE, so try to use it
			   (let 
			      ((node
				  (endor:dispatch 'get-left-ewoc wookie ch)))
			      (if node (throw 'wookie:next-ewoc node)))))
		     nil)))
	    ;;Otherwise try parent's parent.
	    (wookie:parent-get-next-ewoc wookie parent))
      
	 ;;No parent.  Get ewoc node from ewoc itself.
	 (ewoc--set-buffer-bind-dll ewoc  
	    (ewoc--node-nth dll -1)))))

;;;_  . Child management
;;;_   , wookie:enter-new-child
(defun wookie:enter-new-child (datum ewoc wookie following-ewoc-node parent)
   ""
   
   (let
      (
	 (node
	    (endor:dispatch 'make-node wookie 
	       datum 
	       following-ewoc-node
	       parent)))
	      
      ;;Queue the new node to be expanded later.
      (endor:will-display-node wookie node)
      node))


;;;_   , wookie:edit-children
(defun wookie:edit-children (wookie node edits data-list)
   ""

   (let
      (  (ewoc (endor:endor->ewoc wookie))
	 (rv-new-children '())
	 (old-children (wookie:node->children node)))
      ;;Possibly use flet to abbreviate the push/setq/etc
      
      (dolist (e edits)
	 (case (car e)
	    (a
	       (assert old-children)
	       ;;Skip an old (A) item.
	       (wookie:delete-either wookie (car old-children))
	       ;;Advance the current position.
	       (pop old-children))
	    
	    (b
	       ;;Enter a new (B) item at the current position
	       (let
		  ((new-child
		      (wookie:enter-new-child
			 (car data-list) 
			 ewoc
			 wookie 
			 ;;Get which ewoc-node to insert it before.
			 (if old-children
			    ;;Possibly first unprocessed old child.
			    (endor:dispatch
			       'get-left-ewoc wookie (car old-children))
			    ;;Otherwise, get it from an ancestor.
			    (wookie:parent-get-next-ewoc wookie node))
			 node)))
		  (pop data-list)
		  (push new-child rv-new-children)))
	    
	    (both
	       ;;Accept the old item as new. 
	       (push (car old-children) rv-new-children)
	       (pop data-list)
	       (pop old-children))))

      ;;Put the new children in place, replacing the old ones.
      (setf (wookie:node->children node) 
	 (nreverse rv-new-children))))



;;;_   , wookie:expand-one
(defun wookie:expand-one (wookie node)
   "(Re)expand the node's children.

For now, we assume it always wants expansion.  Later we'll support
reprinting too, when it's not a placeholder."
   (endor:check (wookie:th:children-linked-p node))
   (assert (listp (wookie:node->children node)))

   (let*
      (  (ewoc (endor:endor->ewoc wookie))
	 ;;Expand to data.
	 (data-list
	    (funcall (wookie:wookie->expand-f wookie)
	       (wookie:node->data node)))
	    
	 ;;Here, could compress pieces into lists.

	 ;;Compute the respective hashes
	 (hashes
	    (mapcar #'sxhash data-list))

	 ;;Align the lists (with align-lists).  This could be done
	 ;;quickly if we know old node was empty, but that's
	 ;;align-list's concern, not ours.
	 (edits
	    (align-lists
	       (wookie:node->hashes node)  
	       hashes
	       #'=)))

      ;;Tests are interested in which edits were prescribed.
      (emtp tp:mus34bd1mxe0 (edits))

      ;;This call puts new children in place
      (wookie:edit-children 
	 wookie 
	 node 
	 edits
	 data-list)
	 
      ;;We put new hashes in place.
      (setf (wookie:node->hashes node) hashes)
      node))

;;;_  . Display management functions
;;;_   , wookie:node->dlist
(defsubst wookie:node->dlist (wookie node)
   ""
   (wookie:obj->dlist wookie (wookie:node->data node)))

;;;_   , wookie:obj->dlist
(defun wookie:obj->dlist (wookie obj)
   ""
   
   (let*
      ((getter
	  (wookie:wookie->get-dlist
	     wookie))
	 (dlist
	    (if getter
	       (funcall getter obj)
	       (error 
		  "Null get-dlist function in endor"))))
				
      (check-type dlist wookie:dlist)
      dlist))

;;;_   , wookie:register-display
(defun wookie:register-display (wookie node)
   ""
   (check-type wookie wookie:wookie)
   (check-type node wookie:node)
   (let
      ((dlist
	  (wookie:node->dlist wookie node)))
      (check-type dlist wookie:dlist)
      ;;Don't re-add the same display.
      (unless (memq node (wookie:dlist->displayers dlist))
	 (push node
	    (wookie:dlist->displayers dlist)))))

;;;_   , wookie:unregister-display
;;(dlist display)
(defun wookie:unregister-display (wookie node)
   ""
   (check-type wookie wookie:wookie)
   (check-type node wookie:node)
   (let
      ((dlist
	  (wookie:node->dlist wookie node)))
      (check-type dlist wookie:dlist)
      (callf2 delq node
	 (wookie:dlist->displayers dlist))))

;;;_   , wookie:redisplay
(defun wookie:redisplay (wookie dlist)
   ""

   (dolist (d (wookie:dlist->displayers dlist))
      (endor:check (wookie:either:th:all-linked-p d))
      ;;This can causes redisplay.  We don't indicate whether we're
      ;;showing it again, but the node itself knows.
      (endor:will-display-node wookie d))
   
   ;;$$RETHINK ME Not sure this belongs here.  Sometimes we may want
   ;;to delay doing all this.
   (endor:display-pending wookie))

;;;_   , wookie:display-gone
(defun wookie:display-gone (wookie dlist)
   ""

   (dolist (d (wookie:dlist->displayers dlist))
      ;;Redisplay the node's parent
      (let
	 ((parent (wookie:node->parent d)))
	 ;;If it has none, it's the root and we must take stronger
	 ;;measures.  Punted for now.
	 (if parent
	    (endor:will-display-node wookie parent)))))




;;;_  . Handler functions
;;;_   , wookie:handler-alist

(defconst wookie:handler-alist 
   (make-endor:callback-table
      :match-type-p
      #'(lambda (wookie node)
	   (wookie:node-p node))
      :display
      #'(lambda (wookie node)
	   ;;Register displayer now.
	   (wookie:register-display wookie node)
	   (wookie:expand-one wookie node))
      :delete
      #'(lambda (wookie node)
	   (wookie:unregister-display wookie node)
	   (wookie:delete-node wookie node))
      :get-left-ewoc
      #'(lambda (wookie node)
	   (wookie:get-leftmost-sub-ewoc wookie node))
      :make-node
      #'(lambda (wookie o following-ewoc parent)
	   ;;If either `when' fails we return nil which signals
	   ;;"Didn't use it"
	   (when (consp o)
	      (let* 
		 ((alist (wookie:wookie->alist-mk-node wookie))
		    (cell (assq (car o) alist)))
		 (when cell
		    (wookie:make-node
		       :parent   parent
		       :children '()
		       :hashes   '()
		       :data     
		       (apply (second cell) wookie (cdr o))))))))
   
   "Wookie table of handler functions." )


;;;_. Footers
;;;_ , Provides

(provide 'viewers/wookie)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/wookie.el ends here
