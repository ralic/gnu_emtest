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
(require 'utility/loal)
;;;_. Body
;;;_ , Types

;;;_  . chewie:dynamic-obj
(defstruct (chewie:dynamic-obj
	      (:constructor chewie:make-dynamic-obj)
	      (:conc-name chewie:dynamic-obj->)
	      (:copier nil))
   "Chewie node, for use in a wookie.
It fully contains the information used to redisplay the object."


   obj
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


;;;_ , Entry points
;;;_  . chewie:make-chewie
(defun chewie:make-chewie (root top-data expander ewoc-printer get-dlist)
   ""

   (let
      ((wookie
	  (wookie:make-wookie
	     ;;Printer for ewoc.
	     :ewoc-print-func ewoc-printer
	     :get-dlist 
	     `(lambda (x)
		 (,get-dlist
		    (chewie:dynamic-obj->obj x)))
	     :expand-f
	     #'chewie:get-expansion
	     :alist-mk-node
	     (list
		(list 'dynamic
		   #'(lambda (wookie obj data func)
			(chewie:make-dynamic-obj
			   ;;Being rethought.
			   ;;:list     (wookie:obj->dlist wookie obj)
			   :obj      obj
			   :data     data
			   :format-f func))))
	     :other-handlers 
	     (list wookie:handler-alist))))
      (chewie:set-root wookie root top-data expander)
      wookie))

;;;_ , Support functions
;;;_  . chewie:set-root
(defun chewie:set-root (chewie root top-data expander)
   ""
   (endor:--set-root chewie `(dynamic ,root ,top-data ,expander)))

;;;_  . chewie:get-expansion
(defun chewie:get-expansion (x)
   "Return a list of items, each suitable as formatting input.

X must be a `chewie:dynamic-obj'."

   (funcall
      (chewie:dynamic-obj->format-f x)
      (chewie:dynamic-obj->obj x)
      (chewie:dynamic-obj->data x)))

;;;_  . chewie:dynamic-notnull (A support function for formatters)
(defun chewie:dynamic-notnull (obj data formatter)
   "Make a dynamic form for an object just if OBJ is non-nil."
   (when obj `(dynamic ,obj ,data ,formatter)))


;;;_. Footers
;;;_ , Provides

(provide 'viewers/chewie)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/chewie.el ends here
