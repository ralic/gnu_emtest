;;;_ viewers/wookie/testhelp.el --- Testhelp for wookie

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal, maint

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
(require 'viewers/endor/testhelp)

;;;_. Body
;;;_ , Structures
;;;_  . wookie:tht:dynamic-1s
(defstruct (wookie:tht:dynamic-1s
	      (:constructor wookie:make-tht:dynamic-1s)
	      (:conc-name wookie:tht:dynamic-1s->)
	      (:include endor:tht:1s))
   "Test-help:  ADT derived from `endor:tht:1s', but dynamic."
   list)
;;;_  . wookie:tht:dynamic-1s+1rec
(defstruct (wookie:tht:dynamic-1s+1rec
	      (:constructor wookie:make-tht:dynamic-1s+1rec)
	      (:conc-name wookie:tht:dynamic-1s+1rec->)
	      (:include endor:tht:1s+1rec))
   "Test-help:  ADT derived from `endor:tht:1s+1rec', but dynamic."
   list)

;;;_ , Dlist extractor (just wookie:tht:dynamic-1s+1rec->list)

;;;_ , Formatter functions
;;;_  . wookie:th:format-dynamic-1s+1rec
(defun wookie:th:format-dynamic-1s+1rec (obj)
   "Format prints a string field, then delegates to a child if any."
   `( 
      ,(wookie:tht:dynamic-1s+1rec->str obj)
      "("
      ,(let 
	 ((obj2 (wookie:tht:dynamic-1s+1rec->recurse obj)))
	 (when obj2 `(th:dynamic ,obj2)))
      ")"))
;;;_ , Testhelp
;;;_  . wookie:th:usual-printer
(defun wookie:th:usual-printer (x)
   ""
   
   (typecase x
      (cons
	 (mapcar #'wookie:th:usual-printer x))
      (string
	 (insert x))))
;;;_  . wookie:th:set-root
(defun wookie:th:set-root (wookie obj)
   ""
   (endor:--set-root wookie `(th:dynamic ,obj)))

;;;_  . wookie:th:make-usual-wookie
(defun wookie:th:make-usual-wookie 
   (expander root get-dlist &optional ewoc-printer)
   "Make a standard wookie for testing"
   (let
      ((endor
	  (wookie:make-wookie
	     ;;Printer for ewoc.
	     :ewoc-print-func 
	     (or ewoc-printer #'wookie:th:usual-printer)
	     :get-dlist get-dlist
	     :expand-f expander
	     :alist-mk-node
	     (list
		(list 'th:dynamic
		   #'(lambda (wookie obj) obj)))
	     :other-handlers 
	     (list wookie:handler-alist))))
      (when root
	 (wookie:th:set-root endor root))
      
      endor))




;;;_. Footers
;;;_ , Provides

(provide 'viewers/wookie/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/wookie/testhelp.el ends here
