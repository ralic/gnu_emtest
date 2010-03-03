;;;_ viewers/chewie/testhelp.el --- Testhelp for chewie

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
(require 'viewers/chewie)
(require 'viewers/wookie/testhelp)

;;;_. Body
;;;_ , Structures
;;;_  . chewie:tht:dynamic-1s
(defstruct (chewie:tht:dynamic-1s
	      (:constructor chewie:make-tht:dynamic-1s)
	      (:conc-name chewie:tht:dynamic-1s->)
	      (:include wookie:tht:1s))
   "Test-help:  ADT derived from `wookie:tht:1s', but dynamic."
   list)
;;;_  . chewie:tht:dynamic-1s+1rec
(defstruct (chewie:tht:dynamic-1s+1rec
	      (:constructor chewie:make-tht:dynamic-1s+1rec)
	      (:conc-name chewie:tht:dynamic-1s+1rec->)
	      (:include wookie:tht:1s+1rec))
   "Test-help:  ADT derived from `wookie:tht:1s+1rec', but dynamic."
   list)

;;;_ , Chewie list extractor (chewie:tht:dynamic-1s+1rec->list)

;;;_ , Formatter functions
;;;_  . chewie:th:format-dynamic-1s+1rec
(defun chewie:th:format-dynamic-1s+1rec (obj data)
   "Format prints a string field, then delegates to a child if any."
   `( 
      ,(chewie:tht:dynamic-1s+1rec->str obj)
      "("
      ;;This construction could be abbreviated, call it
      ;;`chewie:dynamic-notnull'
      ,(let 
	 ((obj2 (chewie:tht:dynamic-1s+1rec->recurse obj)))
	 (when obj2 `(dynamic ,obj2 () chewie:th:format-dynamic-1s+1rec)))
      ")"))
;;;_ , Testhelp
;;;_  . chewie:th:make-usual-chewie
(defun chewie:th:make-usual-chewie (expander root get-chewie-list)
   ""
   '
   (wookie:th:make-usual-wookie
      #'chewie:get-expansion
      `(dynamic ,root () ,expander)
;;       (chewie:make-dynamic-obj
;; 	 :obj root 
;; 	 :data () ;;data
;; 	 :format-f expander)
      get-chewie-list)
   ;;$$Maybe should use `chewie:create-wookie' instead.
   (wookie:create
      ;;Obsolescent
      #'chewie:get-expansion
      ;;Printer for ewoc.
      #'loformat:print
      :object 
      `(dynamic ,root () ,expander)
      ;;Obsolescent
      :get-chewie-list get-chewie-list
      :buf (current-buffer)
      ;;:func-list (list #'chewie:handler)

      :handlers (list chewie:handler-alist)
      )
   )


;;;_. Footers
;;;_ , Provides

(provide 'viewers/chewie/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/chewie/testhelp.el ends here
