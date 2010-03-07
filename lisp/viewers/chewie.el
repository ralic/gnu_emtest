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

;;;_  . chewie:2:list
;;$$RENAME ME
(defstruct (chewie:2:list
	      ;;(:type list)
	      (:constructor chewie:2:make-list)
	      (:conc-name chewie:2:list->)
	      (:copier nil))
   "List of current displayers.  Intended to live inside other
types of data nodes."
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
   (wookie  () :type endor:endor))

;;;_ , Entry points
;;;_  . chewie:create-wookie
;;$$IMPROVE ME - Maybe create a testhelp to check that these
;;parameters covary.
(defun chewie:create-wookie (expander root get-chewie-list &rest r)
   ""
   ;;$$IMPROVE ME - some of these should be params.  Params should be
   ;;taken more neatly.
   (let
      ((chewlist
	  (funcall get-chewie-list root)))
      
      (apply #'endor:create
	 ;;OBSOLETE.  Waiting for re-arrangement
	 #'chewie:get-expansion
	 ;;Printer for ewoc.  Should be a param.
	 #'loformat:print
	 ;;$$DESIGN ME - This seems to lead to typing error
	 ;;opportunity.
	 :object
	 `(dynamic ,root () ,expander)
	 :handlers (list chewie:handler-alist)
	 ;;OBSOLESCENT
	 :get-chewie-list get-chewie-list
	 r)))


;;;_ , Other functions
;;;_  . chewie:get-expansion
;;$$MOVE ME Belongs in wookie.
(defun chewie:get-expansion (x &optional big-data)
   "Return a list of items, each suitable as formatting input.

X must be a `chewie:dynamic-obj'."

   (funcall
      (chewie:dynamic-obj->format-f x)
      (chewie:dynamic-obj->obj x)
      (chewie:dynamic-obj->data x)))

;;;_  . Chewie object to displayer layer

;;;_  . Chewie interface layer
;;;_   , chewie:register-display
;;$$IMPROVE ME The functionality to get chewlist could move into here.
(defun chewie:register-display (chewlist display)
   ""
   (check-type chewlist chewie:2:list)
   (check-type display wookie:node)

   ;;Don't re-add the same display.
   (unless (memq display (chewie:2:list->displayers chewlist))
      (push display
	 (chewie:2:list->displayers chewlist))))

;;;_   , chewie:unregister-display
;;$$IMPROVE ME The functionality to get chewlist could move into here.
(defun chewie:unregister-display (chewlist display)
   ""
   (callf2 delq display 
      (chewie:2:list->displayers chewlist)))

;;;_   , chewie:get-chewlist
(defun chewie:get-chewlist (wookie obj)
   ""
   (let*
      ((getter
	  (endor:endor->get-chewie-list
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
	 (endor:check (wookie:either:th:all-linked-p d))
	 ;;$$FIX ME This causes REdisplay but we aren't indicating or
	 ;;reacting to showing it again, we're just displaying it as
	 ;;if for the first time.
	 (endor:will-display-node wookie d)))
   
   ;;$$RETHINK ME Not sure this belongs here.  Sometimes we may want
   ;;to delay doing all this.
   (endor:display-pending wookie))

;;;_   , chewie:display-gone
(defun chewie:display-gone (wookie obj)
   ""
   
   (let* 
      ((chewlist (chewie:get-chewlist wookie obj)))
      
      (dolist (d (chewie:2:list->displayers chewlist))
	 ;;Redisplay the node's parents (Assume it has parents,
	 ;;otherwise it's the root and we must take stronger measures)
	 (endor:will-display-node 
	    wookie 
	    (wookie:node->parent d)))))


;;;_. Draft 2 (Has been merged in)
;;;_ , Structures


;;;_. Footers
;;;_ , Provides

(provide 'viewers/chewie)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/chewie.el ends here
