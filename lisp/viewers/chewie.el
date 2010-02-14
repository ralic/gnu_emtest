;;;_ chewie.el --- Chewie - dynamic display of tree-structured data

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp

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
(require 'wookie)
(require 'loal)
(require 'hiformat)
;;;_. Body
;;;_ , Types
;;;_  . chewie:map-cell
;;May merge with `chewie:dynamic-obj'
(defstruct (chewie:map-cell
	      (:type list)
	      (:constructor chewie:make-map-cell)
	      (:conc-name chewie:map-cell->)
	      (:copier nil))
   "One cell of a mapping from objects to ewocs or wookies.
This is basically an alist cell."
   key
   ;;May not even be useful here.
   (data () :type loal:single-alist)
   ;;This will become a list of "wookie:either"
   (ewoc () :type wookie:either))

;;;_  . chewie:dynamic-obj
;;May merge with `chewie:map-cell'
(defstruct (chewie:dynamic-obj
	      (:constructor chewie:make-dynamic-obj)
	      (:conc-name chewie:dynamic-obj->)
	      (:copier nil))
   "Chewie node, for use in a wookie"
   obj
   ;;Not clear whether singleton or full is wanted here.  May not even
   ;;be useful here.
   (data     () :type loal:single-alist)
   ;;$$RETHINKME If chewie serves instead of rules, this may not be
   ;;needed.
   (format-f () :type (satisfies functionp)
      :doc
      "This function xforms obj into a format list.
 * It takes 2 args
   * An object (as OBJ above)
   * Data (as DATA above)
 * It returns a list of objects suitable for transforming to
`wookie:displayable's."
      ))

;;;_  . chewie:chewie
(defstruct (chewie:chewie
	      (:constructor chewie:make-chewie)
	      (:conc-name chewie:chewie->))
   "A top-level chewie object"
   ;;$$CHANGING
   ;;The mapping will go away.
   ;;Formatter callbacks may be moved here.
   ;;Or an accessor callback which gets a chewie node from the object
   ;;that is passed into here.
   (mapping () :type (repeat chewie:map-cell))
   (wookie  () :type wookie:wookie))

;;;_ , Entry points
;;;_  . chewie:set-root
(defun chewie:set-root (chewie obj data func)
   ""

   (let
      ((wookie (chewie:chewie->wookie chewie)))

      ;;Wookie will (correctly) error if root was already set.
      (wookie:set-root 
	 wookie 
	 (chewie:make-dynamic-obj
	    :obj obj 
	    :data data
	    :format-f func))
      ;;Know the root node.  NB, `obj' is the correct argument.  The
      ;;dynamic object passed to wookie:set-root is *not* the key
      ;;object.
      (chewie:link-obj-node chewie obj (wookie:wookie->root wookie))))

;;;_  . chewie:setup-root 
;;$$RENAMEME chewie:setup
;;$$CHANGING:  Now func's return must merely be of type covariant with
;;chewie:get-expansion (?)  Which should become a param.
;;$$MOVEME maybe.  This may be just how emviewer constructs a wookie,
;;since chewie contains almost nothing now.
(defun chewie:setup-root (obj data func &optional buf)
   "Set up a chewie.
OBJECT can be any object that FUNC understands.  It will be the root
of the tree.
DATA is a chewie:loal - an empty list will work.
FUNC is a function taking 2 arguments:
 * Object to be formatted
 * A chewie:loal.  Note that each element is in the same format
   that DATA is in.
FUNC should return a format list, which must satisfy
`(typep x 'hiformat:format)'

BUF is the buffer to print in.  It is not handled yet."

   (let*
      ((wookie
	  (wookie:create
	     ;;Object expander for wookie.
	     #'chewie:get-expansion
	     ;;Printer for ewoc.
	     #'loformat:print
	     :buf buf
	     :showing-cb 
	     #'(lambda (node obj chewie)
		  ;;$$CHANGING
		  ;;This will use the new merged strategy
		  (chewie:link-obj-node 
		     chewie 
		     (chewie:dynamic-obj->obj obj)
		     node))
	     ;;For now, do nothing on unshowing.
	     ;;:unshowing-cb nil
	     ))
	 (chewie
	    (chewie:make-chewie
	       :wookie wookie
	       :mapping ())))
      ;;$$CHANGING
      ;;Set the field DATA now, after construction, because there's a
      ;;circular structure: The chewie holds the wookie and the
      ;;wookie's data field is that chewie.
      (setf (wookie:wookie->data wookie) chewie)
      (when obj
	 (chewie:set-root chewie obj data func))
      chewie))



;;;_ , Other functions
;;;_  . chewie:get-expansion

;;Big-data would be a chewie, if needed.  It's stored with the wookie
;;tree.

;;$$RETHINK MY PLACE Not clear that this belongs here.  But it does
;;use and need chewie dynamic objects.
(defun chewie:get-expansion (x &optional big-data)
   "Return a list of `wookie:displayable's, each corresponding to an
item that the formatting function returns.

X must be a `chewie:dynamic-obj'."

   (mapcar
      #'(lambda (o)
	   ;;$$RETHINKME This may becomes merely dispatching, inside
	   ;;hiformat but dispatching to chewie if dynamic is seen.
	   ;;But it has to know about wookie.  Every case must give a
	   ;;`wookie:displayable'
	   (cond
	      ((and (consp o) (eq (car o) 'dynamic))
		 ;;If object is dynamic, store it for dynamic treatment.
		 (destructuring-bind (dummy obj data func)
		    o
		    (wookie:make-displayable
		       :data
		       (chewie:make-dynamic-obj
			  :obj obj
			  :data data
			  :format-f func)
		       :held-outside-p
		       t
;; 		       ;;Obsolescent, still in use for the moment.
;; 		       :callback
;; 		       #'(lambda (node obj chewie)
;; 			    (chewie:link-obj-node chewie obj node))
;; 		       ;;Obsolescent, still in use for the moment.
;; 		       :cb-data
;; 		       obj
		       )))
	      
	      ;;Otherwise just return it
	      (t (wookie:make-displayable 
		    :data o 
		    :held-outside-p nil))))

      ;;This part doesn't belong here.  Chewie is really just about
      ;;dynamic objects.
      ;;This will change.  It may become an `apply' done in hiformat.
      (funcall
	 (chewie:dynamic-obj->format-f x)
	 (chewie:dynamic-obj->obj x)
	 (chewie:dynamic-obj->data x))))

;;;_  . Chewie object to displayer layer
;;;_   , chewie:with-map-cell
;;$$OBSOLESCENT This will go away
(defmacro chewie:with-map-cell (chewie obj cell-sym else &rest body)
   "Evaluate BODY with symbol CELL-SYM bound to the map cell of OBJ.
If there is none, evaluate ELSE instead."
   
   `(let
       ;;Find it.
       ((,cell-sym
	   (assq ,obj (chewie:chewie->mapping ,chewie))))
       (if ,cell-sym
	  (progn ,@body)
	  ,else)))


;;;_   , chewie:link-obj-node
;;$$CHANGING This will change, just operate on node
;;Was `chewie:know-dynamic-obj'
(defun chewie:link-obj-node (chewie obj node)
   "Register NODE as a rendering of OBJ.
NODE must be an ewoc node or a wookie node."

   ;;For now, we assume it's a one-to-one mapping.
   ;;Todo: Allow multiple nodes.
   ;;Todo: Find and use old key if it exists
   (push 
      (chewie:make-map-cell
	 :key obj
	 :data ()
	 :ewoc node)
      (chewie:chewie->mapping chewie)))

;;;_  . Chewie interface layer
;;;_   , chewie:freshen-obj
;;Could take a rest list of objects instead of just one
;;$$RETHINK MY PLACE This is not properly part of chewie.  It is how
;;pathtree invokes reprinting.
(defun chewie:freshen-obj (chewie obj)
   "Mark OBJ as needing reprinting.

Treats OBJ as an identity (via `eq'), not as a value."

   (chewie:with-map-cell chewie obj cell
      nil
      (wookie:will-display-node 
	 (chewie:chewie->wookie chewie)
	 (chewie:map-cell->ewoc cell)))

   (wookie:display-pending (chewie:chewie->wookie chewie)))




;;;_   , chewie:set-obj-display-parms
;;$$IGNOREME This won't even apply here.
;;Untested yet.
'
(defun chewie:set-obj-display-parms (chewie obj data)
   "Set the object-local display data about OBJ to DATA.

Note that object-local data applies to every rendering of the object
in a given tree."
   (chewie:with-map-cell chewie obj cell
      ;;If it doesn't exist, create it, so there is no timing
      ;;dependency - we can call this function before or after.  Doing
      ;;this will make more sense when we have a list of ewoc/dll
      ;;pairs, which will be zero here.
      ;;
      '(if (not cell)
	  (push 
	     (chewie:make-map-cell
		:key obj
		:data data
		:ewoc nil)
	     (chewie:chewie->mapping chewie))
	  ;;Else do the rest of this.
	  )
      
      (let
	 ((node (chewie:map-cell->ewoc cell)))
	 ;;Set that information in cell
	 (setf
	    (chewie:map-cell->data cell)
	    data)

	 ;;For now, there's just one item.
	 (etypecase node
	    (wookie:node
	       (setf
		  (chewie:dynamic-obj->data
		     (wookie:node->data node))
		  data)
	       ;;Set it to redisplay
	       )
	    ;;Can't typecase ewoc--node, so assume vector is one.  Can
	    ;;this case meaningfully occur, since ewocs are directly
	    ;;displayable and are things like strings?
	    (vector
	       (setf
		  (chewie:dynamic-obj->data
		     (ewoc-data node))
		  data)))

	 ;;Set it to redisplay
	 (wookie:will-display-node 
	    (chewie:chewie->wookie chewie)
	    node))))



;;;_. Footers
;;;_ , Provides

(provide 'chewie)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; chewie.el ends here
