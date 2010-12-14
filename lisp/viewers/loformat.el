;;;_ viewers/loformat.el --- Low-level structured inserting

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
(require 'pp)

;;;_. Body
;;;_  . Types
;;$$IMPROVE ME  parameterize on the symbol or on the third element,
;;which would then be an `and' of which it is now and 
(deftype loformat:tree ()
     '(or
	 string
	 character
	 (list* symbol (repeat *))
	 (repeat loformat:tree)))

;;;_  . Some inserters
;;;_  . loformat:insert:overlay
(defun loformat:insert:overlay (recurse-f properties &rest r)
   "Insert an overlay containing R
PROPERTIES gives the properties the overlay should have."

   (let
      ( ;;Remember where point was
	 (beg (set-marker (make-marker) (point))))
			
      ;;Recurse
      (mapcar recurse-f r)

      ;;Now we know the text the overlay must cover,
      ;;so we can create it.  Couldn't create it
      ;;earlier and let it advance, because
      ;;rear-advance must be nil.
      (let
	 ((ov
	     (make-overlay beg (point) nil nil nil)))
	 (dolist (prop properties)
	    (destructuring-bind (name value) prop
	       (overlay-put ov name value)))
	 ;;Now don't need the marker any more.
	 (set-marker beg nil))))
;;;_  . loformat:insert:sep
(defun loformat:insert:sep (recurse-f strength)
   "Insert a separator of the given STRENGTH.
STRENGTH ranges from 0 to 6."
   (funcall recurse-f
      (case strength
	 (0 " ")
	 (1 " - ")
	 (2 '(nl-if-none))
	 (3 "\n")
	 (4 '((nl-if-none) "\n"))
	 (5 (list '(nl-if-none) (make-string 20 ?-)"\n"))
	 (6 (list '(nl-if-none) "\n" (make-string 40 ?*)"\n\n")))))

;;;_  . loformat:insert:nl-if-none
(defun loformat:insert:nl-if-none (recurse-f)
   "Insert a newline just if there is none at point."
   (unless (bolp) (insert "\n")))
;;;_  . loformat:insert:object
(defun loformat:insert:object (recurse-f object props)
   "Insert printed representation of OBJECT, suitably demarcated.
PROPS is the property-list for the text, if any."
   
   (let*
      (  
	 (bare-str
	    ;;$$Possibly should be `custom-prin1-to-string' instead.
	    (pp-to-string object))
	 ;;$$IMPROVE ME Use loformat:insert:w/props instead.
	 (str
	    (if props
	       (apply #'propertize bare-str props)
	       bare-str)))
		  
      (mapcar recurse-f (list '(sep 2) str '(sep 2)))))
;;;_  . loformat:insert:w/props
(defun loformat:insert:w/props (recurse-f object props)
   ""
   (let
      ((loformat:text-properties props))
      (declare (special loformat:text-properties))
      (funcall recurse-f object)))

;;;_  . loformat:insert:w/face

(defun loformat:insert:w/face (recurse-f object face)
   "Insert OBJECT with the given face"
   ;;$$IMPROVE ME Make and use `loformat:insert:w/more-props' which
   ;;would add to existing properties.
   (loformat:insert:w/props recurse-f object (list 'face face)))


;;;_ , Defaults
;;;_  . loformat:default-alist
(defconst loformat:default-alist 
   '(
       (nl-if-none  loformat:insert:nl-if-none)
       (object      loformat:insert:object)
       (overlay     loformat:insert:overlay)
       (sep         loformat:insert:sep)
       (w/face      loformat:insert:w/face)
       (w/props     loformat:insert:w/props))
   "The default alist for `loformat:insert'" )
;;;_  . The insert function itself
;;;_   , loformat:insert
(defun loformat:insert (tree &optional alist loformat:default)
   ""
   ;;The arguments become special variables
   (declare (special 
	       loformat:alist loformat:text-properties loformat:default))
   (let
      ((loformat:alist (or alist loformat:default-alist))
	 (loformat:text-properties nil))
      (loformat:insert-x tree)))

;;;_   , loformat:insert-x
(defun loformat:insert-x (tree)
   "Print a format-tree into the current buffer.
TREE must be a format-tree.

ALIST is an alist from symbol to expander function.
Each such function takes a callback (to loformat:insert) and arbitrary
arguments.

The insert function is always `insert'."
   (declare (special loformat:alist loformat:text-properties))
   (cond
      ;;Do nothing for empty list
      ((null tree) nil)
      ;;For non-empty list, look for a governor.
      ((consp tree)
	 (let
	    ((gov (car tree)))
	 (if (symbolp gov)
	    (let
	       ((cell
		   (assoc gov loformat:alist)))
	       (if cell
		  (apply (second cell) #'loformat:insert-x (cdr tree))
		  (insert "No inserter found for " (symbol-name gov))))
	    ;;If the first element is not a symbol, treat it as a
	    ;;component, not a governor.
	    (mapcar #'loformat:insert-x tree))))
      ;;Just insert atoms
      ((stringp tree)
	 (insert
	    (if loformat:text-properties
	       (apply #'propertize tree loformat:text-properties)
	       tree)))
      (t (insert tree))))

;;;_. Footers
;;;_ , Provides

(provide 'viewers/loformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/loformat.el ends here
