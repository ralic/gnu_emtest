;;;_ emtest/runner/keepup.el --- Help to keep defined values up to date for tests

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

;; This code is for keeping defined values up to date while code being
;; developed changes.

;; Motivation: Sometimes you have test objects that depend on code that you
;; are developing.  When you change that code, the objects need to
;; change too, otherwise your tests will tell you the wrong thing.
;; That's Very Bad News because not only will they fail, it will not
;; be obvious why, since inspecting the objects' definitions tells you
;; nothing. 

;; But re-evalling each definition manually is a chore.  Even doing it
;; file-by-file is a chore, and is sometimes inadequate (eg for
;; defstruct)

;; With keepup, you just write 

;;   (emt:keep-up-to-date (SYMS...) ...) 

;;around each dependent definition, where SYMS are the symbols of
;;functions being depended on.

;;;_ , Requires

(eval-when-compile
   (require 'cl))

;;;_. Body

;;;_ , Keep-up-to-date definitions

;;$$MOVE ME maybe Is there a better place for this?  It's not just
;;testhelp because suite launches use it.  An associated define file?
;;;_  . emt:keep-up-to-date-x
(defun emt:keep-up-to-date-x (sym-list form-list)
   "See emt:keep-up-to-date"
   (dolist (sym sym-list)
      (put sym 'emt:keep-up-to-date
	 (list
	    (symbol-function sym)
	    (mapcar
	       #'(lambda (form)
		    (list nil form))
	       form-list)))))
;;;_  . emt:keep-up-to-date
(defmacro emt:keep-up-to-date (sym-list &rest form-list)
   "Set up to keep definitions up to date with the functions of
   SYM-LIST.
FORM-LIST should be a list of forms that each define something."
   
   `(progn
       (emt:keep-up-to-date-x ',sym-list ',form-list)
       ,@form-list))

;;;_  . emtd:update-for-sym
(defun emtd:update-for-sym (sym)
   "Update any keep-up-to-date definitions for SYM."

   (let
      ((data (get sym 'emt:keep-up-to-date)))
      (when data
	 (let
	    ((forms (second data)))
	    (unless (eq (first data) (symbol-function sym))
	       (mapcar
		  #'(lambda (x)
		       (eval (second x)))
		  forms))))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/keepup)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/keepup.el ends here
