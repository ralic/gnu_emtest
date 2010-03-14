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
(require 'viewers/hiformat)  ;;For loformat, actually, and temporary
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
   ;;(list     () :type wookie:dlist)  ;;$$RECONSIDER ME
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
;;$$CHANGER CALLERS get-dlist must change for this.  Or wrap it
;;in a lambda.
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
      (endor:set-root wookie `(dynamic ,root ,top-data ,expander))
      wookie))




;;;_ , Support functions
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

;;;_ , Chewie interface layer
;;;_  . chewie:get-dlist
;;Gone, adapted as wookie:node->dlist

;;;_  . chewie:get-dlist
;;$$REDESIGN ME  This info should be associated with wookie, not
;;chewie.  This should still be parameterized, but just for the
;;convenience of tests.  That basically means for wookie, endor data
;;must be a wookie object.

;;The getter will be gotten from wookie (typically
;;`chewie:node->dlist' which will then not call this).  BUT no, we
;;always field thru the 2 structures with this strategy.
'  ;;OBSOLESCENT
(defun chewie:get-dlist (wookie obj)
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

;;;_  . chewie:node->dlist
'(defun chewie:node->dlist (wookie node)
   ""
   ;;$$REDESIGN ME
   ;;Getting this is ODD, unneccessarily complex.  There are 2
   ;;routes.  Could have just extracted it from the chewie dynamic
   ;;object, which data directly is.  So we don't really need that
   ;;field.  It doesn't make anything easier.  Or it could just be
   ;;merged in directly.  And `chewie:get-dlist' can essentially
   ;;disappear. 

   (let* 
      (  (obj
	    (chewie:dynamic-obj->obj
	       (wookie:node->data node))))
      ;;$$REPLACE ME with a call about chewie dynamic obj
      (chewie:get-dlist wookie obj)))
'  ;;Obsolete
(defun chewie:data->dlist (wookie data)
   ""
   ;;$$REDESIGN ME
   ;;Getting this is ODD, unneccessarily complex.  There are 2
   ;;routes.  Could have just extracted it from the chewie dynamic
   ;;object, which data directly is.  So we don't really need that
   ;;field.  It doesn't make anything easier.  Or it could just be
   ;;merged in directly.  And `chewie:get-dlist' can essentially
   ;;disappear. 

   (let* 
      (  (obj
	    (chewie:dynamic-obj->obj data)))
      
      ;;$$REPLACE ME with a call about chewie dynamic obj
      ;;(chewie:get-dlist wookie obj)

      ))


;;;_. Footers
;;;_ , Provides

(provide 'viewers/chewie)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/chewie.el ends here
