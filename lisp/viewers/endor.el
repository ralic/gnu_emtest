;;;_ viewers/endor.el --- Ewocs in an extensible structure

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

(require 'ewoc)
(unless
   (featurep 'ewoc 'variable-separator)
   ;;Fallback.  This will error if it doesn't succeed
   (load "viewers/231ewoc" nil))
(require 'utility/ttvtable)

;;;_ , Dummies for testing
;;$$DO ME MORE NEATLY to avoid compiler warning "not known".
(defmacro endor:check (form &rest args)
   "Assert FORM just if endor testhelp is available."
   `(when 
       (and
	  (featurep 'viewers/endor/testhelp)
	  endor:th:do-checks)
       (assert ,form ,@args)))
(eval-and-compile
   (unless (require 'emtest/testhelp/testpoint nil t)
      (defmacro emtp (id args &rest rest)
	 `(progn ,@rest))))

;;;_. Body
;;;_ , Data types

;;;_  . endor:endor
(defstruct (endor:endor
	      (:constructor endor:make-endor
		 (&key
		    other-handlers
		    ewoc-print-func
		    data
		    ;;`root' always starts as `nil'
		    &aux
		    (ewoc
		       (ewoc-create ewoc-print-func nil nil ""))
;; 		    (scratch
;; 		       (endor:cbtable:collect-handlers
;; 			  other-handlers))
;; 		    (vtable-alist
;; 		       (first scratch))
;; 		    (list-default-handler
;; 		       (second scratch))
		    (handlers
		       (append
			  other-handlers
			  (list endor:ewoc-handler-alist)))
		    (ttvtable
		       (ttvtable:make
			  handlers))))
	      
	      (:constructor nil)
	      (:conc-name endor:endor->))
   "A endor object"
   (root () :type t)  ;;Type covaries with handlers
   (ewoc () :type ewoc)
   (ttvtable () :type ttvtable)
   ;;$$REMOVE ME later.  Moving into ttvtable
;;    (handlers () 
;;       :type (repeat
;; 	       (list
;; 		  symbol
;; 		  function))
;;       :doc
;;       "Handler function.  Takes:
;;  * command
;;  * endor
;;  * zero or more other args.  If NODE is passed it will be passed as
;;    the first of these other args.
;;  * Returns non-nil if it handled the command, otherwise nil.

;; Command can be:
;;  * match-type-p: W C NODE Return non-nil just if NODE is the
;;    respective type. 
;;  * display: W C NODE Cause it to be displayed
;;  * delete: W C NODE Cause it to be undisplayed and deleted.
;;  * get-left-ewoc: W C NODE Get the leftmost ewoc owned by this node.
;;  * make-node W C DATA PLACEHOLDER.
;;    * Make a new node of the respective type.")
;;    ;;$$REMOVE ME later.  Moving into ttvtable
;;    (vtable-alist () :type 
;;       (repeat (list symbol endor:callback-table)))
;;    ;;$$REMOVE ME later.  Moving into ttvtable
;;    (list-default-handler () :type endor:callback-table)
   
   (data ()
      :doc "Blind data for callbacks.  
\(Type is covariant with the caller")

   (pending () :type (repeat wookie:either)
      :doc "List of nodes that are waiting to be displayed."))

;;;_ , Functions
;;;_  . endor:--set-root

(defun endor:--set-root (endor object)
   "Set the root of a endor.
Error if it has been set before.
This function should not usually be directly called."
   (when (endor:endor->root endor)
      (error "Endor root has already been set"))

   (let*
      (
	 (ewoc (endor:endor->ewoc endor))
	 (node 
	    ;;Safe even though root has not been set
	    (endor:dispatch
	       'make-node endor object 
	       (ewoc--set-buffer-bind-dll ewoc  
		  (ewoc--node-nth dll -1))
	       nil)))

      (setf (endor:endor->root endor) node)
      
      ;;Re-enabled.
      (endor:will-display-node endor node)

      ;;Expand any pending parts
      (endor:display-pending endor)
      endor))

;;;_  . endor:--clear-root
;;Untested, unused.
(defun endor:--clear-root (endor)
   ""
   ;;Delete it semantically
   (endor:dispatch 'delete endor (endor:endor->root endor))
   ;;Delete it physically
   (setf (endor:endor->root endor) nil)
   ;;Redraw
   (endor:display-pending endor))

;;;_  . Dispatchers

;;;_   , Vtable version
(when t
;;;_    . endor:callback-table
   (defstruct (endor:callback-table
		 (:include ttvtable:vtable)
		 (:constructor endor:make-callback-table))
      
      "Callback table.

Command can be:
 * match-type-p: ENDOR NODE Return non-nil just if NODE is the
   respective type. 
 * display: ENDOR NODE Cause it to be displayed
 * delete: ENDOR NODE Cause it to be undisplayed and deleted.
 * get-left-ewoc: ENDOR NODE Get the leftmost ewoc owned by this node.
 * make-node ENDOR DATA FOLLOWING-NODE PARENT
   * Make a new node of the respective type.
 * linked-p: ENDOR NODE.  For debugging, return non-nil if all the
component ewoc nodes are properly linked."
      match-type-p
      display
      delete
      get-left-ewoc
      make-node

;;       (node-tag  () :type (repeat symbol)
;; 	 :doc
;; 	 "List of structure-tags that a node might have.
;; For a structure created by defstruct, it would be in the variable
;; `cl-struct-NAME-tags'.")
      
;;       (node-type () :type (member list vector)
;; 	 :doc 
;; 	 "Broad type of nodes.  

;; For a structure created by defstruct, it would be the
;; cl-struct-type property of NAME."  )

      (linked-p ()
	 :doc
	 "For debugging.  Function that returns true if all the node's
	 ewocs are linked in properly." )
      )
;;;_    . endor:alist:command->accessor
   (defconst endor:alist:command->accessor 
      `(
	  (match-type-p  ,#'endor:callback-table-match-type-p)
	  (display       ,#'endor:callback-table-display)
	  (delete        ,#'endor:callback-table-delete)
	  (get-left-ewoc ,#'endor:callback-table-get-left-ewoc)
	  (get-next-ewoc ,#'endor:callback-table-get-next-ewoc)
	  (make-node     ,#'endor:callback-table-make-node)
	  (linked-p      ,#'endor:callback-table-linked-p))
      "" )
;;;_    . endor:cbtable:command->accessor
   ;;Use `endor:alist:command->accessor'
   (defun endor:cbtable:command->accessor (command)
      ""
      (second
	 (assq command ;;endor:alist:command->accessor
	    `(
		(match-type-p  ,#'endor:callback-table-match-type-p)
		(display       ,#'endor:callback-table-display)
		(delete        ,#'endor:callback-table-delete)
		(get-left-ewoc ,#'endor:callback-table-get-left-ewoc)
		(get-next-ewoc ,#'endor:callback-table-get-next-ewoc)
		(make-node     ,#'endor:callback-table-make-node)))))
;;;_    . endor:dispatch-xx
   (defun endor:dispatch-xx (command table endor args)
      ""
   
      (let*
	 (  (accessor-f
	       (endor:cbtable:command->accessor command))
	    (func (funcall accessor-f table)))
	 (apply func endor args)))

   (define-compiler-macro endor:dispatch-xx 
      (&whole body command table endor args)
      ""
      (let
	 ((literal-cmd-p
	     (and
		(consp command)
		(eq (car command) 'quote))))

	 ;;If COMMAND is literal, use its respective slot accessor
	 ;;at compile time.
	 (if literal-cmd-p
	    (let*
	       ((accessor-f
		   (endor:cbtable:command->accessor (eval command)))
		  (func-form `(,accessor-f ,table)))
	       `(apply ,func-form ,endor ,args))
	    ;;Otherwise use the original
	    body)))
;;;_    . High dispatch that naively just matches via the type predicate
   (when nil
;;;_     , endor:cbtable:collect-handlers
      (defun endor:cbtable:collect-handlers (&rest r)
	 ""
	 ;;Dummy.  This deliberately doesn't do anything.
	 (list nil nil))

;;;_     , endor:dispatch-xxx
      (defsubst endor:dispatch-xxx (command h endor args)
	 "Try to dispatch COMMAND via table H"
	 (if
	    (memq command '(make-node match-type-p))
	    ;;Some commands can't or shouldn't use a NODE argument.
	    ;;Just call the handler and if the result is `nil',
	    ;;fall thru to next set of handlers.
	    (let
	       ((answer
		   (endor:dispatch-xx command h endor args)))
	       (when answer
		  (throw 'endor:dispatch-done answer)))
	    ;;Most commands pass NODE as next argument.  For them,
	    ;;check the node's type.  If NODE is the right type,
	    ;;call the real command and return its result.  If it
	    ;;isn't, fall thru.
	    (when
	       (endor:dispatch-xx 'match-type-p h endor
		  (list (car args)))
	       (throw 'endor:dispatch-done
		  (endor:dispatch-xx command h endor args)))))

      (define-compiler-macro endor:dispatch-xxx
	 (&whole body command h endor args)
	 (let* 
	    ((literal-cmd-p
		(and
		   (consp command)
		   (eq (car command) 'quote))))
	    (if literal-cmd-p
	       ;;This is nearly repitition of the original form, but I
	       ;;see no alternative for now.
	       (if
		  (memq (eval command) '(make-node match-type-p))
		  ;;Some commands can't or shouldn't use a NODE argument.
		  ;;Just call the handler and if the result is `nil',
		  ;;fall thru to next set of handlers.
		  `(let
		      ((answer
			  (endor:dispatch-xx ,command ,h ,endor ,args)))
		      (when answer
			 (throw 'endor:dispatch-done answer)))
		  ;;Most commands pass NODE as next argument.  For them,
		  ;;check the node's type.  If NODE is the right type,
		  ;;call the real command and return its result.  If it
		  ;;isn't, fall thru.

		  ;;The repeatedly-evaluated forms here are not good,
		  ;;though in practice here they just refer to symbol so
		  ;;they should be safe enough, though not robust.
		  `(when
		      (endor:dispatch-xx 'match-type-p ,h ,endor
			 (list (car ,args)))
		      (throw 'endor:dispatch-done
			 (endor:dispatch-xx ,command ,h ,endor ,args))))

	       body)))
   
   
;;;_     , endor:dispatch

      (defun endor:dispatch (command endor &rest args)
	 ""
	 (emtp tp:7b7edqv0exe0
	    (command args)
	    (let
	       ((handler-sets-list (endor:endor->handlers endor)))
	 
	       (catch 'endor:dispatch-done
		  (dolist (h handler-sets-list)
		     (endor:dispatch-xxx command h endor args))))))

      (define-compiler-macro endor:dispatch 
	 (&whole body command endor &rest args)
	 ;;This omits the testpoint, so does not support certain tests.
	 `(let*
	     (  (endor ,endor)
		(handler-sets-list (endor:endor->handlers endor)))
	 
	     (catch 'endor:dispatch-done
		(dolist (h handler-sets-list)
		   (endor:dispatch-xxx ,command h endor (list
		,@args)))))))

;;;_   , Tagwise version (of high dispatch)
;;This relies on using structures.  It will require ctor to be
;;changed, not include endor:ewoc-handler-alist among the handlers
;;because now it's the default.  And construct `vtable-alist' in the
;;expected manner.
   (when nil
;;;_    . endor:cbtable:collect-handlers
      (defun endor:cbtable:collect-handlers (raw-handlers)
	 ""
	 (let*
	    ((list-default nil)
	       (raw-list
		  (mapcar
		     ;;Collect an alist, with possibly some nil cells,
		     ;;and possibly set list-default while doing so.
		     #'(lambda (x)
			  (let
			     ((tag
				 (endor:callback-table-node-tag x)))
			     (cond 
				(tag
				   (list tag x))
				((eq (endor:callback-table-node-type x) 'list)
				   (setq list-default x)
				   nil)
				(t nil))))
		     raw-handlers)))
	    (list (delq nil raw-list) list-default)))

;;;_     , endor:get-table-x
      (defun endor:get-table-x (vtable-alist sym)
	 ""
      
	 (let
	    ((cell (assq sym vtable-alist)))
	    (when cell (second cell))))
;;;_     , endor:get-table
      (defun endor:get-table (node vtable-alist vec-default list-default)
	 ""
	 (etypecase node
	    (vector
	       (or
		  (endor:get-table-x vtable-alist (aref node 0))
		  vec-default))
		     
	    (cons
	       (or
		  (endor:get-table-x vtable-alist (car node))
		  list-default
		  (error "No list default handler available")))))
   
;;;_     , endor:dispatch-until-success
      (defun endor:dispatch-until-success (vtable-alist command endor &rest args)
	 ""
	 (catch 'endor:dispatch-done
	    ;;Except it will become an alist or obarray so we
	    ;;must extract the table object.
	    (dolist (h vtable-alist)
	       (let
		  ((answer
		      (endor:dispatch-xx command (second h) endor args)))
		  (when answer
		     (throw 'endor:dispatch-done answer))))))

;;;_     , endor:dispatch
   
      (defun endor:dispatch (command endor &rest args)
	 ""
	 (emtp tp:7b7edqv0exe0
	    (command args)
	    (let
	       ((vtable-alist (endor:endor->vtable-alist endor)))
	       (if (memq command '(make-node match-type-p))
		  ;;If command does not have a node, loop thru
		  ;;all vtables.
		  (apply #'endor:dispatch-until-success
		     ;;Has to include the defaults too, as the last
		     ;;two items.  There's opportunity to make this
		     ;;faster and neater.
		     (append
			vtable-alist
			(let
			   ((list-default
			       (endor:endor->list-default-handler endor)
			       ))
			   (if list-default
			      (list
				 (list nil list-default)
				 (list nil endor:ewoc-handler-alist))
			      
			      (list
				 (list nil endor:ewoc-handler-alist))
			      )))
		     
		     command endor args)
	       
		  ;;Otherwise figure out vtable by shortcut.
		  (let*
		     ((node (car args))
			(vtable
			   (endor:get-table
			      node
			      vtable-alist
			      endor:ewoc-handler-alist 
			      (endor:endor->list-default-handler endor))))
		     (endor:dispatch-xx command vtable endor
		  args)))))))
;;;_    . ttvtable version

   (when t
;;;_    . endor:cbtable:collect-handlers
      ;;Temporary, just because ctor still uses it.
      (defun endor:cbtable:collect-handlers (raw-handlers)
	 ""
	 (list nil nil))

;;;_     , endor:dispatch
   
      (defun endor:dispatch (command endor &rest args)
	 ""
	 (emtp tp:7b7edqv0exe0
	    (command args)
	    (let
	       (
		  (ttvtable (endor:endor->ttvtable endor)))
	       
	       (if (memq command '(make-node match-type-p))
		  ;;If command does not have a node, try all vtables
		  ;;until success
		  (ttvtable:call-until-success
		     ttvtable 
		     endor:alist:command->accessor
		     command
		     (cons endor args))
	       
		  ;;Otherwise figure out the vtable that corresponds
		  ;;to node and use it.  Node, if present, is always
		  ;;the first of ARGS (but after the ubiquitous ENDOR
		  ;;arg)
		  (let*
		     ((node (car args)))
		     (ttvtable:dispatch-xx 
			(ttvtable:get-table node ttvtable)
			endor:alist:command->accessor 
			command 
			(cons endor args))))))))
   
   )




;;;_  . Endor pending-list functions

;;;_   , endor:will-display-node
(defun endor:will-display-node (endor node)
   ""
   
   (pushnew node (endor:endor->pending endor)))

;;;_   , endor:display-pending
(defun endor:display-pending (endor)
   ""
   (while (endor:endor->pending endor)
      (endor:display-one 
	 endor
	 (pop (endor:endor->pending endor)))))

;;;_   , endor:display-one
(defsubst endor:display-one (endor obj)
   "Display or redisplay an ewoc node or a wookie node."
   (endor:dispatch 'display endor obj))

;;;_  . Handler functions
;;;_   , endor:ewoc-handler-alist
;;$$RENAME ME endor:ewoc-handler-vtable
(defconst endor:ewoc-handler-alist
   (endor:make-callback-table
      :node-tag nil ;;cl-struct-NAME-tags
      :node-type (car (get 'ewoc--node 'cl-struct-type))
      :match-type-p
      #'(lambda (endor node)
	 (vectorp node))
      :display
      #'(lambda (endor node)
	   (emtp tp:j4rfxx-display
	      (node)
	      (ewoc-invalidate (endor:endor->ewoc endor) node)))
      :delete
      #'(lambda (endor node)
	   (emtp tp:9wr6as-delete
	      (node)
	      (ewoc-delete     (endor:endor->ewoc endor) node)))
      :get-left-ewoc
      #'(lambda (endor node)
	 node)
      :make-node
      #'(lambda (endor o following-ewoc-node parent)
	   (emtp tp:n3k5ro-make-node
	      (o)
	      (let
		 ((ewoc (endor:endor->ewoc endor)))
		 (ewoc-enter-before ewoc following-ewoc-node o))))

      ;;$$AUTOLOAD ME so we don't drag in testhelp unless it's
      ;;actually used.
      :linked-p
      #'(lambda (endor node)
	   (ewoc:th:linked-p node))
      
      )
   
   "Alist from command symbol to ewoc-oriented function.
Vtable version."
   )


;;;_. Footers
;;;_ , Provides

(provide 'viewers/endor)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/endor.el ends here
