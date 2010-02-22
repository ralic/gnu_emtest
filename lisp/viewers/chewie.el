;;;_ viewers/chewie.el --- Chewie - dynamic display of tree-structured data

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
(require 'viewers/wookie)
(require 'viewers/loal)
(require 'viewers/hiformat)
;;;_. Body
;;;_ , Types
;;;_  . chewie:map-cell
;;May merge with `chewie:dynamic-obj'
;;$$REMOVE ME later.  Obsolescent
'
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

;;;_  . chewie:2:list
(defstruct (chewie:2:list
	      ;;(:type list)
	      (:constructor chewie:2:make-list)
	      (:conc-name chewie:2:list->)
	      (:copier nil))
   "One cell of wookie.  Intended to live inside other types of data
nodes."
   ;;(data () :type loal:single-alist)
   ;;These nodes all know how to expand themselves.
   (displayers () :type (repeat wookie:either)))

;;;_  . chewie:dynamic-obj
(defstruct (chewie:dynamic-obj
	      (:constructor chewie:make-dynamic-obj)
	      (:conc-name chewie:dynamic-obj->)
	      (:copier nil))
   "Chewie node, for use in a wookie.
It fully contains the information used to redisplay the object."


   obj
   (list     () :type chewie:2:list)
   (data     () :type loal)
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
;;$$REMOVE ME later.  OBSOLESCENT
'
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
;;$$CHANGE ME
'
(defun chewie:set-root (chewie obj data func)
   ""

   (error "Obsolete")
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
;;$$CHANGE ME - just wraps a few things that we pass.
'
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
`(typep x 'viewers/hiformat:format)'

BUF is the buffer to print in.  It is not handled yet."

   (error "Obsolete")
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

;;;_  . chewie:create-wookie
(defun chewie:create-wookie (expander root get-chewie-list &rest r)
   ""
   ;;$$IMPROVE ME - some of these should be params.  Params should be
   ;;taken more neatly.
   (apply #'wookie:create
      #'chewie:get-expansion
      ;;Printer for ewoc.  Should be a param.
      #'loformat:print
      :object
      (chewie:make-dynamic-obj
	 :obj root 
	 :data () ;;Should be a param.
	 :format-f expander)
      :get-chewie-list get-chewie-list
      r))


;;;_ , Other functions
;;;_  . chewie:get-expansion

(defun chewie:get-expansion (x &optional big-data)
   "Return a list of items, each suitable as formatting input.

X must be a `chewie:dynamic-obj'."

   (funcall
      (chewie:dynamic-obj->format-f x)
      (chewie:dynamic-obj->obj x)
      (chewie:dynamic-obj->data x)))

;;;_  . Chewie object to displayer layer
;;;_   , chewie:with-map-cell
;;$$OBSOLESCENT This will go away
'
(defmacro chewie:with-map-cell (chewie obj cell-sym else &rest body)
   "Evaluate BODY with symbol CELL-SYM bound to the map cell of OBJ.
If there is none, evaluate ELSE instead."
   (error "Obsolete")
   `(let
       ;;Find it.
       ((,cell-sym
	   (assq ,obj (chewie:chewie->mapping ,chewie))))
       (if ,cell-sym
	  (progn ,@body)
	  ,else)))


;;;_   , chewie:link-obj-node
;;$$CHANGING This will change, just operate on node
;;Was `chewie:know-dynamic-obj'.  This will be replaced.
;;And renamed `chewie:new-display'
'
(defun chewie:link-obj-node (chewie obj node)
   "Register NODE as a rendering of OBJ.
NODE must be an ewoc node or a wookie node."
   (error "Obsolete")
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
;;$$CHANGE ME This will just call a redisplayer.  `obj' will be the list
;;Or it can entirely become obsolete.
;;$$REMOVE ME OBSOLETE.
'
(defun chewie:freshen-obj (chewie obj)
   "Mark OBJ as needing reprinting.

Treats OBJ as an identity (via `eq'), not as a value."
   (error "Obsolete")
   (chewie:with-map-cell chewie obj cell
      nil
      (wookie:will-display-node 
	 (chewie:chewie->wookie chewie)
	 (chewie:map-cell->ewoc cell)))

   (wookie:display-pending (chewie:chewie->wookie chewie)))
;;;_   , chewie:register-display
(defun chewie:register-display (chewlist display)
   ""
   (push display
      (chewie:2:list->displayers chewlist)))
;;;_   , chewie:unregister-display
(defun chewie:unregister-display (&rest r)
   ""
   (error "Not written yet"))

;;;_   , chewie:get-chewlist
(defun chewie:get-chewlist (wookie obj)
   ""
   (let*
      ((getter
	  (wookie:wookie->get-chewie-list
	     wookie))
	 (chewlist
	    (if getter
	       (funcall getter obj)
	       (error 
		  "Null get-chewie-list function in wookie"))))
				
      (check-type chewlist chewie:2:list)
      chewlist))

;;;_   , chewie:redisplay
(defun chewie:redisplay (wookie obj)
   ""

   (let* 
      ((chewlist (chewie:get-chewlist wookie obj)))
      
      (dolist (d (chewie:2:list->displayers chewlist))
	 (wookie:will-display-node wookie d)))
   
   ;;$$RETHINK ME Not sure this belongs here.  Sometimes we may want
   ;;to delay doing all this.
   (wookie:display-pending wookie))

;;;_   , chewie:display-gone
(defun chewie:display-gone (wookie obj)
   ""
   
   (let* 
      ((chewlist (chewie:get-chewlist wookie obj)))
      
      (dolist (d (chewie:2:list->displayers chewlist))
	 ;;Redisplay the node's parents (Assume it has parents,
	 ;;otherwise it's the root and we must take stronger measures)
	 (wookie:will-display-node 
	    wookie 
	    (wookie:node->parent d)))))


;;;_. Draft 2
;;;_ , Structures
;;;_  . chewie:2:cell
;;$$REMOVE ME UNUSED
'
(defstruct chewie:2:cell
   ""
   )

;;;_ , Functions
;;;_  . chewie:2:set-root
;;Basically what's left is to `chewie:link-obj-node' on the object,
;;which means incorporating a chewie:2:list into that object
;;;_  . chewie:2:setup-root

;;Little is left of this too.  Basically it sets up `:showing-cb' and
;;`:unshowing-cb' to call to wookie nodes.  This gives it a special
;;role wrt wookie.  I'm not sure that's so good.

;;And it specifies two format functions, but emviewer could just as
;;well.  Setting up the circular references is no longer needed.

;;;_  . chewie:2:get-expansion
;;Into 2 parts: Formatted-thing handler and re-caller for format
;;function.  That's so that formatted-thing handler can be
;;incorporated nicely into hiformat.
;;This puts info into the wookie data field, so we have some
;;flexibility.  

;;;_  . chewie:2:link-obj-node (chewie:2:register)
;;Now it's an operation on a chewie:2:list and it just pushes its
;;diplay into place.  Rename it

;;;_  . chewie:2:unregister
;;Remove a display from a chewie:2:list
;;;_  . chewie:2:freshen-obj (chewie:2:reshow)
;;Interacts with wookie.  Could get more info from cell, but need not.
;;Maybe the call to `wookie:display-pending' should be outside here.

;;To display, wookie basically calls its usual `expand-f' function and
;;calls `showing-cb' with the results.  So right now it doesn't do any
;;interpretation.


;;;_. Footers
;;;_ , Provides

(provide 'viewers/chewie)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/chewie.el ends here
