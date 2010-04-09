;;;_ utility/align-lists/rtest.el --- Tests of align-lists

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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

(require 'utility/align-lists)

;;;_. Body

;;;_ , Examples
(emt:eg:define xmp:vbsfkhk0dxe0
   ((project emtest)
      (library align-lists))
   (group
      ((name 0))
      (doc () "No change")
      (item
	 ((type new-list))
	 '(a b c d))
      (item
	 ((type result))
	 '(
	     (both a a)
	     (both b b)
	     (both c c)
	     (both d d))))

   (group
      ((name 1))
      (doc () "All new, one item")
      (item
	 ((type new-list))
	 '(e))
      (item
	 ((type result))
	 '(
	     (a a)
	     (a b)
	     (a c)
	     (a d)
	     (b e))))

   (group
      ((name 2))
      (doc () "All new, two items")
      (item
	 ((type new-list))
	 '(e f))
      (item
	 ((type result))
	 '(
	     (a a)
	     (a b)
	     (a c)
	     (a d)
	     (b e)
	     (b f))))

   (group
      ((name 3))
      (doc () "Replace first el")
      (item
	 ((type new-list))
	 '(e b c d))
      (item
	 ((type result))
	 '(
	     (a a)
	     (b e)
	     (both b b)
	     (both c c)
	     (both d d))))

   (group
      ((name 4))
      (doc () "Replace some first els")
      (item
	 ((type new-list))
	 '(e f c d))
      (item
	 ((type result))
	 '(
	     (a a)
	     (a b)
	     (b e)
	     (b f)
	     (both c c)
	     (both d d))))

   (group
      ((name 5))
      (doc () "Insert before first el")
      (item
	 ((type new-list))
	 '(e a b c d))
      (item
	 ((type result))
	 '(
	     (b e)
	     (both a a)
	     (both b b)
	     (both c c)
	     (both d d))))

   (group
      ((name 6))
      (doc () "Insert after last el")
      (item
	 ((type new-list))
	 '(a b c d e))
      (item
	 ((type result))
	 '(
	     (both a a)
	     (both b b)
	     (both c c)
	     (both d d)	     
	     (b e))))

   (group
      ((name 7))
      (doc () "Replace last el")
      (item
	 ((type new-list))
	 '(a b c e))
      (item
	 ((type result))
	 '(
	     (both a a)
	     (both b b)
	     (both c c)
	     (a d)	     
	     (b e))))

   (group
      ((name 8))
      (doc () "Replace middle element")
      (item
	 ((type new-list))
	 '(a e c d))
      (item
	 ((type result))
	 '(
	     (both a a)
	     (a b)
	     (b e)
	     (both c c)
	     (both d d))))

   (group
      ((name 9))
      (doc () "Add middle element")
      (item
	 ((type new-list))
	 '(a e b c d))
      (item
	 ((type result))
	 '(
	     (both a a)
	     (b e)
	     (both b b)
	     (both c c)
	     (both d d))))

   (group
      ((name 10))
      (doc () "Add first and last element")
      (item
	 ((type new-list))
	 '(e a b c d f))
      (item
	 ((type result))
	 '(
	     (b e)
	     (both a a)
	     (both b b)
	     (both c c)
	     (both d d)
	     (b f))))

   (group
      ((name 11))
      (doc () "Add first and middle element")
      (item
	 ((type new-list))
	 '(e a f b c d))
      (item
	 ((type result))
	 '(
	     (b e)
	     (both a a)
	     (b f)
	     (both b b)
	     (both c c)
	     (both d d))))

   )

;;;_ , Example validator

(defun align-lists:th:->new-list (l)
   ""
   (delq nil
      (mapcar
	 #'(lambda (el)
	      (case (car el)
		 (a nil)
		 (b (second el))
		 (both (third el))))
	 l)))

(defun align-lists:th:validate-example ()
   "Expects the common narrowing to already be bound around it"
   (equal
      (emt:eg (type new-list))
      (align-lists:th:->new-list (emt:eg (type result)))))


;;;_ , align-lists
(rtest:deftest align-lists

   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 0))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 1))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 2))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 3))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 4))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 5))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))   
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 6))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 7))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 8))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 9))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 10))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))
   (  "Proves: The examples give the expected result."
      (emt:eg:narrow ((project emtest)(library align-lists)(name 11))
	 (assert (align-lists:th:validate-example))
	 (assert
	    (equal
	       (align-lists
		  (emt:eg:value 
		     :narrow ((name 0)(type new-list)) 
		     :ignore-tags (name))
		  (emt:eg (type new-list))
		  #'eq)
	       (emt:eg (type result )))
	    t)
	 t))   
   
   )

;;;_. Footers
;;;_ , Provides

(provide 'utility/align-lists/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/align-lists/rtest.el ends here
