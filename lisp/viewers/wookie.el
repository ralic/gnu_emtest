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
(provide 'viewers/align-lists) ;;For aligning the lists.
(eval-when-compile
   (require 'cl))


;;;_. Body
;;;_ , Data types

;;;_ , Wookie
;;;_  . wookie:wookie
;;$$USE ME - and then remove get-chewie-list from endor:endor.
(defstruct (wookie:wookie
	      (:constructor wookie:make-wookie)
	      (:conc-name wookie:wookie->)
	      (:include endor:endor)
	      )
   ""
   ;;(get-chewie-list () :type function)
   )


;;;_  . wookie:node
(defstruct (wookie:node
	      (:constructor wookie:make-node)
	      (:conc-name wookie:node->))
   "A hierarchical display node."
   (parent () :type wookie:node)
   (children () :type (or ewoc--node (repeat t)))
   (data ()
      :doc "The node data (Type is covariant with the caller)"))

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

;;;_  . wookie:data-t
(defstruct wookie:data-t
   "Object as Endor data"
   data)

;;;_ , Entry points



;;;_  . Wookie children management
;;;_   , wookie:enter-new-children
(defun wookie:enter-new-children (wookie data-list following-ewoc-node parent)
   ""
   (endor:check (ewoc:th:linked-p following-ewoc-node))

   ;;`mapcar' traverses elements in order, so it's OK to use.
   (let
      ((ewoc (endor:endor->ewoc wookie)))
      (mapcar
	 #'(lambda (o)
	      (let*
		 ;;Make a placeholder.
		 ((placeholder
		     (ewoc-enter-before 
			ewoc 
			following-ewoc-node 
			(endor:get-placeholder-contents)))
		    (node
		       (endor:dispatch 'make-node wookie 
			  o placeholder parent)))
	      
		 ;;Queue the new node to be expanded later.
		 (endor:will-display-node wookie node)
		 node))
      
	 data-list)))
;;;_   , wookie:delete-either
(defun wookie:delete-either (wookie node)
   ""
   (endor:dispatch 'delete wookie node))

;;;_   , chewie:node->chewlist
;;$$MOVE ME inside chewie
(defun chewie:node->chewlist (wookie node)
   ""
   ;;$$REDESIGN ME
   ;;Getting this is ODD, unneccessarily complex.  There are 2
   ;;routes.  Could have just extracted it from the chewie dynamic
   ;;object, which data directly is.  So we don't really need that
   ;;field.  It doesn't make anything easier.  Or it could just be
   ;;merged in directly.  And `chewie:get-chewlist' can essentially
   ;;disappear. 

   ;;$$FIX ME Doesn't work right.  Maybe has to do with root setup.
   (let* 
      (  (obj
	    (chewie:dynamic-obj->obj
	       (wookie:node->data node))))
      (chewie:get-chewlist wookie obj)))


;;;_   , wookie:delete-node
;;$$MOVE ME inside chewie
(defun wookie:delete-node (wookie node)
   ""
   (endor:check (wookie:th:children-linked-p node))
   (check-type node wookie:node)

   (let* 
      (  (obj
	    (chewie:dynamic-obj->obj
	       (wookie:node->data node)))
	 ;;$$REPLACE ME with `chewie:node->chewlist'
	 (chewlist (chewie:get-chewlist wookie obj)))
      (chewie:unregister-display chewlist node))
   (dolist (child (wookie:node->children node))
      (wookie:delete-either wookie child)))

;;;_   , wookie:get-leftmost-sub-ewoc
;;$$MOVE ME This belongs inside new wookie or chewie.  Wookie itself
;;never initiates get-leftmost-ewoc.
(defun wookie:get-leftmost-sub-ewoc (wookie node)
   ""
   (endor:check (wookie:th:children-linked-p node))
   ;;This assertion is more local and later may be changed
   (assert (wookie:node->children node))
   (assert (listp (wookie:node->children node)))
   (let
      ((left-child (car (wookie:node->children node))))
      '  ;;$$REMOVE ME OBSOLETE
      (etypecase left-child
	 (wookie:node
	    (wookie:get-leftmost-sub-ewoc wookie left-child))
	 (vector
	    (endor:check (ewoc:th:linked-p left-child))
	    left-child))
      ;;$$NEW
      (endor:dispatch 'get-left-ewoc wookie left-child)))


;;;_   , wookie:expand-empty
;;$$MOVE ME and others inside wookie
(defun wookie:expand-empty (tree node)
   ""
   ;;Of course it has a singleton placeholder.
   (endor:check (wookie:th:children-linked-p node))

   ;;The expand-f call was this plus wrapping as displayables:
   (let*
      (
	 (ewoc (endor:endor->ewoc tree))
	 (placeholder
	    (wookie:node->children node))
	 ;;Cannot use `ewoc-next' because we may really want to get
	 ;;the footer node.
	 (following-ewoc-node
	    (ewoc--node-right placeholder))
	 (data-list
	    (chewie:get-expansion (wookie:node->data node))
	    ;;$$REMOVE ME
;;  	       (funcall 
;; 		  (endor:endor->expand-f tree) 
;; 		  (wookie:node->data node))

	    )
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
      node))

;;;_   , wookie:expand-one
;;$$MOVE ME and others inside wookie
(defun wookie:expand-one (wookie node)
   "(Re)expand the node's children.

For now, we assume it always wants expansion.  Later we'll support
reprinting too, when it's not a placeholder."
   (endor:check (wookie:th:children-linked-p node))
   ;;We're really testing for it being a placeholder, indicated by a
   ;;bare ewoc--node, but ewoc--node is an untagged type so instead we
   ;;use `listp' to test for children
   (if
      (listp (wookie:node->children node))

      ;;Re-expand.  Later this may smartly re-use parts of the list.
      (let*
	 (  (ewoc (endor:endor->ewoc wookie))
	    (left-ewoc-node
	       (wookie:get-leftmost-sub-ewoc wookie node))
	    ;;Make a placeholder.
	    (placeholder
	       (ewoc-enter-before 
		  ewoc 
		  left-ewoc-node
		  (endor:get-placeholder-contents))))
	 
	 ;;Remove the original children recursively.
	 (dolist (child (wookie:node->children node))
	    (wookie:delete-either wookie child))
	 ;;Now the wookie is empty again.
	 (setf (wookie:node->children node) placeholder)
	 (wookie:expand-empty wookie node))
      (wookie:expand-empty wookie node)))
;;;_  . Handler functions

;;;_   , chewie:handler-alist (temporary, will move to chewie)
'
(defconst chewie:handler-alist 
   `(
       (display
	  ,#'(lambda (wookie node)
		;;Register displayer now.
		;;$$REDESIGN ME - move this into
		;;chewie:register-display and simplify.
		(chewie:register-display 
		   (chewie:node->chewlist wookie node) node)
		(wookie:expand-one wookie node)))
       
       (delete
	  ,#'(lambda (wookie node)
		(wookie:delete-node wookie node)))
       
       (get-left-ewoc
	  ,#'(lambda (wookie node)
		(wookie:get-leftmost-sub-ewoc wookie node)))
       (make-node
	  ,#'(lambda (wookie o placeholder parent)
		(when
		   (and (consp o) (eq (car o) 'dynamic))
		   (destructuring-bind (dummy obj data func)
		      o
		      (let*
			 (
			    (chewlist
			       (chewie:get-chewlist wookie obj))
			    (dyn-obj
			       (chewie:make-dynamic-obj
				  :list     chewlist
				  :obj      obj
				  :data     data
				  :format-f func))
			    (wookie-node
			       (wookie:make-node
				  :parent   parent
				  :children placeholder
				  :data     dyn-obj)))
			 (endor:check (ewoc:th:linked-p placeholder))


			 ;;Register it as a displayer.  But it hasn't
			 ;;displayed yet!  
			 ;;$$MOVED ME.  MAYBE BUG HERE.  So only register
			 ;;display when displaying.
			 '(chewie:register-display chewlist wookie-node)

			 ;;Return the displayer we made.  It's not
			 ;;active yet so don't register it.
			 wookie-node)))))
       (match-type-p
	  ,#'(lambda (wookie node)
		(wookie:node-p node))))
   
   "Chewie handlers." )

;;;_   , Vtable version chewie:handler-alist 
;;Can use:
' (endor:check (wookie:th:children-linked-p node))

(defconst chewie:handler-alist 
   (make-endor:callback-table
      :match-type-p
      #'(lambda (wookie node)
	   (wookie:node-p node))
      :display
      #'(lambda (wookie node)
	   ;;Register displayer now.
	   ;;$$REDESIGN ME - move this into
	   ;;chewie:register-display and simplify.
	   (chewie:register-display 
	      (chewie:node->chewlist wookie node) node)
	   (wookie:expand-one wookie node))
      :delete
      #'(lambda (wookie node)
	   (wookie:delete-node wookie node))
      :get-left-ewoc
      #'(lambda (wookie node)
	   (wookie:get-leftmost-sub-ewoc wookie node))
      :make-node
      #'(lambda (wookie o placeholder parent)
	   (when
	      (and (consp o) (eq (car o) 'dynamic))
	      (destructuring-bind (dummy obj data func)
		 o
		 (let*
		    (
		       (chewlist
			  (chewie:get-chewlist wookie obj))
		       (dyn-obj
			  (chewie:make-dynamic-obj
			     :list     chewlist
			     :obj      obj
			     :data     data
			     :format-f func))
		       (wookie-node
			  (wookie:make-node
			     :parent   parent
			     :children placeholder
			     :data     dyn-obj)))
		    (endor:check (ewoc:th:linked-p placeholder))

		    ;;Return the displayer we made.  It's not
		    ;;active yet so don't register it.
		    wookie-node)))))
   "Chewie handlers.
Vtable version." )

;;;_   , Chewie handler
'  ;;$$OBSOLETE
(defun chewie:handler (command wookie &rest args)
   ""
   (error "Don't call chewie:handler")
   ;;Most of these want to check NODE for type.  Perhaps the outside
   ;;caller should do that except for nodeless functions, ie
   ;;`make-node'.  That clarifies the value.
   '
   (apply
      (case command
	 (display
	    #'(lambda (wookie node)
		 (when
		    (wookie:node-p node)
		    (wookie:expand-one wookie node))))
	 (delete
	    #'(lambda (wookie node)
		 (when
		    (wookie:node-p node)
		    (wookie:delete-node wookie node)
		    ;;Signal that we acted.
		    t)))
	 (get-left-ewoc
	    #'(lambda (wookie node)
		 (when
		    (wookie:node-p node)
		    (wookie:get-leftmost-sub-ewoc wookie node))))
	 (make-node
	    #'(lambda (wookie o placeholder parent)
		 (when
		    (and (consp o) (eq (car o) 'dynamic))
		    (destructuring-bind (dummy obj data func)
		       o
		       (let*
			  (
			     (chewlist
				(chewie:get-chewlist wookie obj))
			     (dyn-obj
				(chewie:make-dynamic-obj
				   :list     chewlist
				   :obj      obj
				   :data     data
				   :format-f func))
			     (wookie-node
				(wookie:make-node
				   :parent   parent
				   :children placeholder
				   :data     dyn-obj)))
			  (endor:check (ewoc:th:linked-p placeholder))

			  (chewie:register-display chewlist wookie-node)

			  ;;Return the displayer we made.  It's not
			  ;;active yet so don't register it.
			  wookie-node)))))
	 (match-type-p
	    #'(lambda (wookie node)
		 (wookie:node-p node)))

	 )
      
      wookie
      args)

   (let* 
      ((cell
	  (assq command chewie:handler-alist))
	 (handler
	    (second cell)))
      (apply handler wookie args))
   
   )


;;;_. Footers
;;;_ , Provides

(provide 'viewers/wookie)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/wookie.el ends here
