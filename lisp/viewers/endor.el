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

(defmacro endor:check (form &rest args)
   "Assert FORM just if endor testhelp is available."
   `(when 
       (and
	  (featurep 'viewers/endor/testhelp)
	  endor:th:do-checks)
       (assert ,form ,@args)))
(eval-and-compile
   (unless (require 'tester/testhelp/testpoint nil t)
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
		    ;;get-dlist ;;Obsolescent
		    ;;((:root object))
		    ;;No `buf' argument now - it was misleading.
		    ;;`root' always starts as `nil'
		    &aux
		    (ewoc
		       (ewoc-create ewoc-print-func nil nil ""))
		    (handlers
		       (append
			  other-handlers
			  (list endor:ewoc-handler-alist)))
		    ;;$$REMOVE ME  This was just clunky.
;; 		    (root
;; 		       (when object
;; 			  (endor:set-root endor object)
;; 			  (endor:endor->root endor)))

		    ))
	      (:constructor nil)
	      (:conc-name endor:endor->))
   "A endor object"
   (root () :type t)  ;;Type covaries with handlers
   (ewoc () :type ewoc)
;;    ;;Obsolescent
;;    (func-list () :type (satisfies functionp)
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

   (handlers () 
      :type (repeat
	       (list
		  symbol
		  function))
      :doc
      "Handler function.  Takes:
 * command
 * endor
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

   ;;$$REMOVE ME.  Obsolete
   ;;Only used in calls that would be subsumed out anyways.  This is
   ;;really internal to chewie.
;;     (expand-f () :type (satisfies functionp)
;;        :doc
;;        "EXPAND-F takes 2 args and returns a list of `wookie:displayable'
;;  which will be the cores of ewocs or wookies.  Args are:

;;   * covariant node data
;;   * covariant overall data")

   (data ()
      :doc "Blind data for callbacks.  
\(Type is covariant with the caller")
   ;;$$REMOVE ME later
   ;;Really belongs in wookie
;;    (get-dlist () :type (satisfies functionp)
;;       :doc
;;       "Function, takes an object covariant with endor node data type
;; and returns a wookie:dlist" )
   
   (pending () :type (repeat wookie:either)
      :doc "List of nodes that are waiting to be displayed."))

;;;_ , Functions
;;;_  . endor:get-placeholder-contents
(defun endor:get-placeholder-contents ()
   "Get contents appropriate for a placeholder node"
   (error "Obsolete endor:get-placeholder-contents")
   'placeholder)

;;;_  . endor:create
;;$$CHANGE ME args
;;$$CHANGE CALLERS
'  ;;Obsolete
(defun* endor:create (expand-func ewoc-print-func 
			 &key buf object
			 get-dlist ;;OBSOLESCENT
			 handlers)
   "Create an endor.
EXPAND-FUNC is now obsolete.  Was a function of one argument.  It expands an object, returning a
list of `wookie:displayable'.

EWOC-PRINT-FUNC is a function taking 1 argument and inserting
some text at point.

OBJECT, if non-nil, is the root of the tree.  It must be a type that
EXPAND-FUNC accepts as an argument.

BUF is not handled yet."
   (error "Obsolete")
   (let*
      (
	 (ewoc 
	    (ewoc-create ewoc-print-func nil nil ""))

	 (endor
	    (endor:make-endor
	       :root            nil
	       :ewoc            ewoc
	       :get-dlist get-dlist
	       :handlers	(append
				   handlers
				   (list endor:ewoc-handler-alist))
	       )))
      
      ;;Set the root just if it was given (non-nil)
      (when object (endor:set-root endor object))
      endor))

;;;_  . endor:set-root
(defun endor:set-root (endor object)
   "Set the root of a endor.
Error if it has been set before."
   (when (endor:endor->root endor)
      (error "Endor root has already been set"))

   (let*
      (
	 (ewoc (endor:endor->ewoc endor))
;; 	 (placeholder
;; 	    '(ewoc-enter-last ewoc (endor:get-placeholder-contents)))
	 (node 
	    (progn
	       ;;(endor:check (ewoc:th:linked-p placeholder))
	       ;;Safe even though root has not been set
	       (endor:dispatch
		  'make-node endor object 
		  (ewoc--set-buffer-bind-dll ewoc  
		     (ewoc--node-nth dll -1))
		  ;;placeholder 
		  nil))))


      (setf (endor:endor->root endor) node)
      
      ;;Re-enabled.
      (endor:will-display-node endor node)

      ;;Expand any pending parts
      (endor:display-pending endor)
      endor))

;;;_  . endor:clear-root
;;Untested, unused.
(defun endor:clear-root (endor)
   ""
   ;;Delete it semantically
   (endor:dispatch 'delete endor (endor:endor->root endor))
   ;;Delete it physically
   (setf (endor:endor->root endor) nil)
   ;;Redraw
   (endor:display-pending endor))

;;;_  . Dispatchers
;;;_   , Alist version

(when nil
;;;_    . make-endor:callback-table

   (defun* make-endor:callback-table 
      (&key display delete get-left-ewoc make-node match-type-p)
      ""
      (list
	 (list 'display       display)
	 (list 'delete        delete)
	 (list 'get-left-ewoc get-left-ewoc)
	 (list 'display       display)
	 (list 'make-node     make-node)
	 (list 'match-type-p  match-type-p)))
   

;;;_    . endor:dispatch-x
   (defun endor:dispatch-x (command halist endor args)
      ""
   
      (let
	 ((cell
	     (assq command h)))
	 (unless cell (error "No handler for command `%s'" command))
	 (apply (second cell) endor args)))


;;;_    . endor:dispatch

   (defun endor:dispatch (command endor &rest args)
      ""
      (emtp tp:7b7edqv0exe0
	 (command (car args))


	 (let
	    ((handler-sets-list (endor:endor->handlers endor)))
	    (catch 'endor:dispatch-done
	       (dolist (h handler-sets-list)
		  (if
		     (memq command '(make-node match-type-p))
		     ;;Some commands can't or shouldn't use a NODE argument.
		     ;;These immediately call the handler and if the result
		     ;;is `nil', fall thru to next set of handlers.
		     (let
			((answer (endor:dispatch-x command h endor args)))
			(when answer (throw 'endor:dispatch-done answer)))
	       
		     ;;Most commands pass NODE as next argument.  For them,
		     ;;check the node's type.  If NODE is the right type,
		     ;;call the real command and return its result.  If it
		     ;;isn't, fall thru.
		     (when
			(endor:dispatch-x 'match-type-p h endor (list (car args)))
			(throw 'endor:dispatch-done 
			   (endor:dispatch-x command h endor args))))))))))

;;;_   , Vtable version
(when t
;;;_    . endor:callback-table
   (defstruct endor:callback-table
      "Callback table."
      match-type-p
      display
      delete
      get-left-ewoc
      make-node
      ;;$$Add certain fields for debugging purposes.  The
      ;;instantiations could be autoloaded so they don't always drag
      ;;the testhelp files in.
      )
;;;_    . endor:cbtable:command->accessor
   (defun endor:cbtable:command->accessor (command)
      ""
      (second
	 (assq command
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
;;;_    . endor:dispatch-xxx

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
   
   
;;;_    . endor:dispatch

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
	       (endor:dispatch-xxx ,command h endor (list ,@args)))))))

;;;_  . Endor pending-list functions

;;;_   , endor:will-display-node
(defun endor:will-display-node (endor node)
   ""
   
   (push node (endor:endor->pending endor)))


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

(defconst endor:ewoc-handler-alist
   (make-endor:callback-table
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
		 (ewoc-enter-before ewoc following-ewoc-node o)))))
   
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
