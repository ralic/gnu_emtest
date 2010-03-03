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

(when (not (fboundp 'rtest:deftest))
   (defmacro rtest:deftest (&rest dummy))
   (defmacro rtest:if-avail (&rest dummy)))

(require 'ewoc)
(unless
   (featurep 'ewoc 'variable-separator)
   ;;Fallback.  This will error if it doesn't succeed
   (load "viewers/231ewoc" nil))

(defmacro wookie:check (form &rest args)
   "Assert FORM just if wookie testhelp is available."
   `(when 
       (and 
	  (featurep 'viewers/wookie/testhelp)
	  ;;The `t' may become a config check
	  t)
       (assert ,form ,@args)))

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
   ;;Obsolescent
   (func-list () :type (satisfies functionp)
      :doc
      "Handler function.  Takes:
 * command
 * wookie
 * zero or more other args.  If NODE is passed it will be passed as
   the first of these other args.
 * Returns non-nil if it handled the command, otherwise nil.

Command can be:
 * match-type-p: W C NODE Return non-nil just if NODE is the
   respective type. 
 * display: W C NODE Cause it to be displayed
 * delete: W C NODE Cause it to be undisplayed and deleted.
 * get-left-ewoc: W C NODE Get the leftmost ewoc owned by this node.
 * make-node W C DATA PLACEHOLDER.
   * Make a new node of the respective type.")

   (handlers () 
      :type (repeat
	       (list
		  symbol
		  function))
      :doc
      "Handler function.  Takes:
 * command
 * wookie
 * zero or more other args.  If NODE is passed it will be passed as
   the first of these other args.
 * Returns non-nil if it handled the command, otherwise nil.

Command can be:
 * match-type-p: W C NODE Return non-nil just if NODE is the
   respective type. 
 * display: W C NODE Cause it to be displayed
 * delete: W C NODE Cause it to be undisplayed and deleted.
 * get-left-ewoc: W C NODE Get the leftmost ewoc owned by this node.
 * make-node W C DATA PLACEHOLDER.
   * Make a new node of the respective type.")

   ;;$$REMOVE ME later.  Use display instead.
   ;;Only used in calls that would be subsumed out anyways.  This is
   ;;really internal to chewie.
   (expand-f () :type (satisfies functionp)
      :doc
      "EXPAND-F takes 2 args and returns a list of `wookie:displayable'
which will be the cores of ewocs or wookies.  Args are:

 * covariant node data
 * covariant overall data")

   (data ()
      :doc "Blind data for callbacks.  
\(Type is covariant with the caller")
   ;;$$REMOVE ME later
   ;;Set but unused
   (get-chewie-list () :type (satisfies functionp)
      :doc
      "Function, takes an object covariant with wookie node data type
and returns a chewie:2:list" )
   
   (pending () :type (repeat wookie:either)
      :doc "List of nodes that are waiting to be displayed."))


;;;_  . wookie:node
;;$$MOVE ME later to chewie
(defstruct (wookie:node
	      (:constructor wookie:make-node)
	      (:conc-name wookie:node->))
   "A hierarchical display node."
   (parent () :type wookie:node)
   (children () :type (or ewoc--node (repeat wookie:either)))
   (data ()
      :doc "The node data (Type is covariant with the caller)"))

;;;_  . wookie:either
;;REMOVE ME later
;;Will go away, use the match-type-p command instead.
(deftype wookie:either ()
   "Either type that can dynamically print etc"
   ;;We'd like to write `ewoc--node' instead of `vector' but
   ;;`ewoc--node' has no tag.
   '(or wookie:node vector))


;;;_ , Entry points

;;;_  . wookie:get-placeholder-contents
(defun wookie:get-placeholder-contents ()
   "Get contents appropriate for a placeholder node"
   'placeholder)

;;;_  . wookie:create
;;$$CHANGE ME args
;;$$CHANGE CALLERS
(defun* wookie:create (expand-func ewoc-print-func 
			 &key buf object
			 get-chewie-list
			 func-list ;;OBSOLESCENT
			 handlers)
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
	       :root            nil
	       :ewoc            ewoc
	       :expand-f        expand-func
	       :get-chewie-list get-chewie-list
	       :func-list       (append 
				   func-list
				   (list #'wookie:ewoc-handler))
	       :handlers	(append
				   handlers
				   (list wookie:ewoc-handler-alist))
	       )))
      
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
	    ;;$$RETHINK ME  This presumes wookie style expanding.  But
	    ;;Endor shouldn't presume that.  So instead, immediately
	    ;;expand one?  That changes the logic in that non-dynamics
	    ;;don't get expanded.  But they will in the mini-app that
	    ;;we test wookie with.
	    ;;So replace this with a 'make-node command.

	    ;;This redesign forces passing an "expanded" object.
	    ;;Chewie might wrap it in `dynamic'.
	    '(wookie:make-node
	       :parent   nil
	       :children placeholder
	       :data     object)))
      ;;This check is relevant to wookie only.
      ;;Replaced too
      '
      (wookie:check (wookie:th:children-linked-p node))

      

      (wookie:check (ewoc:th:linked-p            placeholder))
      ;;Safe even though root has not been set
      (setq node
	 (wookie:dispatch 'make-node wookie object placeholder nil))

      (setf (wookie:wookie->root wookie) node)

      ;;Replaced too
      ;;Expand the root node
      ;;(wookie:expand-one wookie node)
      
      ;;Re-enabled.
      (wookie:will-display-node wookie node)

      ;;Expand any pending parts
      (wookie:display-pending wookie)
      wookie))
;;;_  . Dispatchers
;;;_   , Alist version

(when nil
;;;_    . make-wookie:callback-table

   (defun* make-wookie:callback-table 
      (&key display delete get-left-ewoc make-node match-type-p)
      ""
      (list
	 (list 'display       display)
	 (list 'delete        delete)
	 (list 'get-left-ewoc get-left-ewoc)
	 (list 'make-node     make-node)
	 (list 'match-type-p  match-type-p)))
   

;;;_    . wookie:dispatch-x
   (defun wookie:dispatch-x (command halist wookie args)
      ""
   
      (let
	 ((cell
	     (assq command h)))
	 (unless cell (error "No handler for command `%s'" command))
	 (apply (second cell) wookie args)))


;;;_    . wookie:dispatch

   ;;$$CHANGE ME This will covaryingly change to expect
   ;;`wookie:callback-table' instead of alist.
   (defun wookie:dispatch (command wookie &rest args)
      ""

      (let
	 ((handler-sets-list (wookie:wookie->handlers wookie)))
	 (catch 'wookie:dispatch-done
	    (dolist (h handler-sets-list)
	       (if
		  (memq command '(make-node match-type-p))
		  ;;Some commands can't or shouldn't use a NODE argument.
		  ;;These immediately call the handler and if the result
		  ;;is `nil', fall thru to next set of handlers.
		  (let
		     ((answer (wookie:dispatch-x command h wookie args)))
		     (when answer (throw 'wookie:dispatch-done answer)))
	       
		  ;;Most commands pass NODE as next argument.  For them,
		  ;;check the node's type.  If NODE is the right type,
		  ;;call the real command and return its result.  If it
		  ;;isn't, fall thru.
		  (when
		     (wookie:dispatch-x 'match-type-p h wookie (list (car args)))
		     (throw 'wookie:dispatch-done 
			(wookie:dispatch-x command h wookie args)))))))))

;;;_   , Vtable version
(when t
;;;_    . wookie:callback-table
   (defstruct wookie:callback-table
      "Callback table."
      match-type-p
      display
      delete
      get-left-ewoc
      make-node)

;;;_    . wookie:dispatch-xx
   (defun wookie:dispatch-xx (accessor-f table wookie args)
      ""
   
      (let
	 ((func (funcall accessor-f table)))
	 (apply func wookie args)))

;;;_    . wookie:dispatch
   (defun wookie:dispatch (command wookie &rest args)
      ""

      (let
	 ((handler-sets-list (wookie:wookie->handlers wookie))
	    (accessor-f
	       (second
		  (assq command
		     `(
			 (match-type-p  ,#'wookie:callback-table-match-type-p)
			 (display       ,#'wookie:callback-table-display)
			 (delete        ,#'wookie:callback-table-delete)
			 (get-left-ewoc ,#'wookie:callback-table-get-left-ewoc)
			 (make-node     ,#'wookie:callback-table-make-node))))))
      
	 (catch 'wookie:dispatch-done
	    (dolist (h handler-sets-list)
	       (if
		  (memq command '(make-node match-type-p))
		  ;;Some commands can't or shouldn't use a NODE argument.
		  ;;Just call the handler and if the result is `nil',
		  ;;fall thru to next set of handlers.
		  (let
		     ((answer (wookie:dispatch-xx accessor-f h wookie args)))
		     (when answer (throw 'wookie:dispatch-done answer)))
	       
		  ;;Most commands pass NODE as next argument.  For them,
		  ;;check the node's type.  If NODE is the right type,
		  ;;call the real command and return its result.  If it
		  ;;isn't, fall thru.
		  (when
		     (wookie:dispatch-xx 
			#'wookie:callback-table-match-type-p
			h wookie (list (car args)))
		     (throw 'wookie:dispatch-done 
			(wookie:dispatch-xx accessor-f h wookie args)))))))))

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
;;So simple now that it might as well just be merged in.
(defun wookie:display-one (obj wookie)
   "Display or redisplay an ewoc node or a wookie node."
   '
   (etypecase obj
      (wookie:node
	 ;;$$REMOVE ME obsolete.
	 '(wookie:expand-one wookie obj)
	 (wookie:dispatch 'display wookie obj))
      
      (vector
	 ;;$$REMOVE ME obsolete.
	 '(ewoc-invalidate (wookie:wookie->ewoc wookie) obj)
	 (wookie:dispatch 'display wookie obj)))

   (wookie:dispatch 'display wookie obj))


;;;_  . Wookie children management
;;;_   , wookie:enter-new-children
;;$$MOVE ME and others inside wookie
(defun wookie:enter-new-children (wookie data-list following-ewoc-node parent)
   ""
   (wookie:check (ewoc:th:linked-p following-ewoc-node))

   ;;`mapcar' traverses elements in order, so it's OK to use.
   (mapcar
      ;;'make-node
      #'(lambda (o)

	   ;;All obsolete
	   '
	   (cond
	      ;;$$PRETTY ME UP Encap these clauses.  Really need to
	      ;;return an ewoc (for linking in) and an either (for
	      ;;returning, and it can be the same).
	      ;;NB, this all belongs inside chewie.  We still need a
	      ;;registering function.
	      ((and (consp o) (eq (car o) 'dynamic))
		 ;;If object is dynamic, store it for dynamic treatment.
		 (destructuring-bind (dummy obj data func)
		    o
		    (let*
		       (
			  (placeholder
			     (ewoc-enter-before 
				ewoc 
				following-ewoc-node 
				(wookie:get-placeholder-contents)))
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
		       (wookie:check (ewoc:th:linked-p placeholder))

		       ;;Queue the new node to be expanded later.
		       (wookie:will-display-node wookie wookie-node)

		       ;;Register it as a displayer
		       (chewie:register-display chewlist wookie-node)

		       ;;Return it
		       wookie-node)))
	      
	      
	      ;;Otherwise just make it as an ewoc
	      (t
		 ;;Command make-node
		 '

		 
		 (ewoc-enter-before 
		    ewoc 
		    following-ewoc-node 
		    o)
		 
		 (let
		    ((placeholder
			(ewoc-enter-before 
			   ewoc 
			   following-ewoc-node 
			   (wookie:get-placeholder-contents))))
		    (wookie:dispatch 'make-node wookie 
		       o placeholder parent))))
	   
	   ;;New way
	   (let*
	      ;;Make a placeholder.
	      ((placeholder
		  (ewoc-enter-before 
		     ewoc 
		     following-ewoc-node 
		     (wookie:get-placeholder-contents)))
		 (node
		    (wookie:dispatch 'make-node wookie 
		       o placeholder parent)))
	      
	      ;;Queue the new node to be expanded later.
	      (wookie:will-display-node wookie node)
	      node))
      
      data-list))
;;;_   , wookie:delete-either
(defun wookie:delete-either (wookie node)
   ""
   ;;Type-check is obsolete.
   (check-type node wookie:either)
   '  ;;$$REMOVE ME Obsolete
   (etypecase node
      (wookie:node
	 '
	 (wookie:delete-node wookie node)
	 (wookie:dispatch 'delete wookie node))
      
      (vector
	 ;;'delete
	 '
	 (ewoc-delete
	    (wookie:wookie->ewoc wookie)
	    node)
	 (wookie:dispatch 'delete wookie node)))
   

   (wookie:dispatch 'delete wookie node))
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
   (wookie:check (wookie:th:children-linked-p node))
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
   (wookie:check (wookie:th:children-linked-p node))
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
	    (wookie:check (ewoc:th:linked-p left-child))
	    left-child))
      ;;$$NEW
      (wookie:dispatch 'get-left-ewoc wookie left-child)))


;;;_   , wookie:expand-empty
;;$$MOVE ME and others inside wookie
(defun wookie:expand-empty (tree node)
   ""
   ;;Of course it has a singleton placeholder.
   (wookie:check (wookie:th:children-linked-p node))

   ;;The expand-f call was this plus wrapping as displayables:
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
      node))

;;;_   , wookie:expand-one
;;$$MOVE ME and others inside wookie
(defun wookie:expand-one (wookie node)
   "(Re)expand the node's children.

For now, we assume it always wants expansion.  Later we'll support
reprinting too, when it's not a placeholder."
   (wookie:check (wookie:th:children-linked-p node))
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
;;;_  . Handler functions
;;;_   , wookie:ewoc-handler-alist
'
(defconst wookie:ewoc-handler-alist 
   `(
      (display
	 ,#'(lambda (wookie node)
	      (ewoc-invalidate (wookie:wookie->ewoc wookie) node)))
      (delete
	 ,#'(lambda (wookie node)
	      (ewoc-delete
		 (wookie:wookie->ewoc wookie) node)))
      (get-left-ewoc
	 ,#'(lambda (wookie node)
	      node))
      (make-node
	 ;;Transform the placeholder into a "real" node.
	 ,#'(lambda (wookie o placeholder parent)
	      (ewoc-set-data placeholder o)
	      placeholder))

      ;;Can't typecase ewoc--node, so assume any vector is one.
      ;;This is why ewoc handling has to go last.

      ;;Returning a boolean here is correct.  If one fails, others
      ;;should try.
      (match-type-p
	 ,#'(lambda (wookie node)
	      (vectorp node))))
   "Alist from command symbol to ewoc-oriented function" )

;;;_   , wookie:ewoc-handler-alist Vtable version

(defconst wookie:ewoc-handler-alist
   (make-wookie:callback-table
      :match-type-p
      #'(lambda (wookie node)
	 (vectorp node))
      :display
      #'(lambda (wookie node)
	 (ewoc-invalidate (wookie:wookie->ewoc wookie) node))
      :delete
      #'(lambda (wookie node)
	 (ewoc-delete
	    (wookie:wookie->ewoc wookie) node))
      :get-left-ewoc
      #'(lambda (wookie node)
	 node)
      :make-node
      #'(lambda (wookie o placeholder parent)
	 (ewoc-set-data placeholder o)
	 placeholder)
      )
   "Alist from command symbol to ewoc-oriented function.
Vtable version."
   )

;;;_   , wookie:ewoc-handler Ewoc handler
'
(defun wookie:ewoc-handler (command wookie &rest args)
   ""
   (error "Don't call wookie:ewoc-handler")
   (let*
      ((cell
	  (assq command wookie:ewoc-handler-alist))
	 (handler
	    (second cell)))
      ;;Would like to put this in better place
      (unless cell (error "No handler for command `%s'" command))
      ;;$$REMOVE ME
      '((handler
	  (case command 
	 (display
	    #'(lambda (wookie node)
		 (ewoc-invalidate (wookie:wookie->ewoc wookie) node)))
	 (delete
	    #'(lambda (wookie node)
		 (ewoc-delete
		    (wookie:wookie->ewoc wookie) node)))
	 (get-left-ewoc
	    #'(lambda (wookie node)
		 node))
	 (make-node
	    ;;Transform the placeholder into a "real" node.
	    #'(lambda (wookie o placeholder parent)
		 (ewoc-set-data placeholder o)
		 placeholder))

	 ;;Can't typecase ewoc--node, so assume any vector is one.
	 ;;This is why ewoc handling has to go last.

	 ;;Returning a boolean here is correct.  If one fails, others
	 ;;should try.
	 (match-type-p
	    #'(lambda (wookie node)
		 (vectorp node))))
	  ))

   (apply handler wookie args)))


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
			 (wookie:check (ewoc:th:linked-p placeholder))


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

(defconst chewie:handler-alist 
   (make-wookie:callback-table
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
		    (wookie:check (ewoc:th:linked-p placeholder))


		    ;;Register it as a displayer.  But it hasn't
		    ;;displayed yet!  
		    ;;$$MOVED ME.  MAYBE BUG HERE.  So only register
		    ;;display when displaying.
		    '(chewie:register-display chewlist wookie-node)

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
			  (wookie:check (ewoc:th:linked-p placeholder))

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
