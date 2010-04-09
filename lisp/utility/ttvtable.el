;;;_ utility/ttvtable.el --- Type-tag dispatch-table functionality

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

;;;_. Body
;;;_ , Structures
;;;_  . ttvtable
(defstruct (ttvtable
	      (:constructor ttvtable:--make)
	      (:conc-name ttvtable->)
	      (:copier nil))
   
   "A type-tag multiple dispatch table"

   (vtable-alist () :type (repeat (list symbol ttvtable:vtable)))
   (list-default-handler () :type ttvtable:vtable)
   (vec-default-handler  () :type ttvtable:vtable))

;;;_  . ttvtable:vtable

(defstruct (ttvtable:vtable
	      (:constructor ttvtable:make-vtable)
	      (:conc-name   ttvtable:vtable->))
   "Base type for vtables (callback tables) that ttvtable handles."

   (node-tag  () :type (repeat symbol)
      :doc
      "List of structure-tags that a node might have.
For a structure created by defstruct, it would be in the variable
`cl-struct-NAME-tags'.")
      
   (node-type () :type (member list vector)
      :doc 
      "Broad type of node.  Ie, `list' or `vector'.

For a structure created by defstruct, it would be the
cl-struct-type property of NAME."  ))

;;;_ , Functionality

;;;_  . ttvtable:collect-handlers
(defun ttvtable:make (raw-handlers)
   ""
   (let*
      (  (ttvtable (ttvtable:--make))
	 (list-default nil)
	 (vec-default nil)
	 (raw-list
	    (mapcar
	       ;;Collect an alist, with possibly some nil cells,
	       ;;and possibly set list-default while doing so.
	       #'(lambda (x)
		    (let
		       ((tag
			   (ttvtable:vtable->node-tag x)))
		       (cond 
			  (tag
			     (list tag x))
			  ((eq (ttvtable:vtable->node-type x) 'list)
			     (setq list-default x)
			     nil)
			  ((eq (ttvtable:vtable->node-type x) 'vector)
			     (setq vec-default x)
			     nil)
			  (t nil))))
	       raw-handlers)))

      (setf
	 (ttvtable->vtable-alist         ttvtable)
	 (delq nil raw-list)
	 (ttvtable->list-default-handler ttvtable) 
	 list-default
	 (ttvtable->vec-default-handler  ttvtable) 
	 vec-default)
      ttvtable))


;;;_  . ttvtable:all-tables

(defun ttvtable:all-tables (ttvtable)
   ""
   (append
      (ttvtable->vtable-alist ttvtable)
      (let
	 ((list-default
	     (ttvtable->list-default-handler ttvtable))
	    (vec-default
	       (ttvtable->vec-default-handler ttvtable)))
	 
	 (if list-default
	    (list
	       (list nil list-default)
	       (list nil vec-default))
			      
	    (list
	       (list nil vec-default))))))

;;;_  . ttvtable:get-table-x
(defun ttvtable:get-table-x (ttvtable sym)
   "Get a table that corresponds to SYM.
The usual practice is that SYM is the tag of a structure."
   (catch 'ttvtable:table
      (dolist (h (ttvtable->vtable-alist ttvtable))
	 (let
	    ((table
		(if (memq sym (first h)) (second h))))
	    (when table
	       (throw 'ttvtable:table table))))))

;;;_  . ttvtable:get-table
;;Test that this works right with defstructs, inheritance, etc. 
(defun ttvtable:get-table (obj ttvtable)
   "Get a table that corresponds to OBJ.
The usual practice is that OBJ is of a type defined by defstruct."
   ;;We don't support an "atom-default-handler".
   ;;This doesn't distinguish lists and vectors, so it's possible that
   ;;a tag stored as the first element of a list could surprisingly
   ;;trigger the corresponding defstruct type's handler.
   (etypecase obj
      (vector
	 (or
	    (ttvtable:get-table-x ttvtable (aref obj 0))
	    (ttvtable->vec-default-handler ttvtable)
	    (error "No vector default handler available")))
		     
      (cons
	 (or
	    (ttvtable:get-table-x ttvtable (car obj))
	    (ttvtable->list-default-handler ttvtable)
	    (error "No list default handler available")))))

;;;_  . ttvtable:command->accessor
(defun ttvtable:command->accessor (command alist)
   ""
   (second (assq command alist)))

;;;_  . ttvtable:dispatch-xx
(defun ttvtable:dispatch-xx (table alist command args)
   ""
   
   (let*
      (  (accessor-f
	    (ttvtable:command->accessor command alist))
	 (func (funcall accessor-f table)))
      (apply func args)))

;;;_  . ttvtable:call-until-success
(defun ttvtable:call-until-success (ttvtable alist command args)
   ""
   (catch 'ttvtable:success
      (dolist (h (ttvtable:all-tables ttvtable))
	 (let
	    ((answer
		(ttvtable:dispatch-xx (second h) alist command args)))
	    (when answer
	       (throw 'ttvtable:success answer))))))

;;;_. Footers
;;;_ , Provides

(provide 'utility/ttvtable)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/ttvtable.el ends here
