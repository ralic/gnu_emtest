;;;_ utility/dynvars.el --- Dynamic special variables

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
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

(eval-when-compile (require 'cl))

;;;_. Body

;;;_ , Variables
;;;_  . utidyv:vars
;;$$OBSOLETE
(defconst utidyv:vars
   '()
   "Special variables that the formatters use.
These variables propagate thru `dynamic' bindings." )
;;;_  . utidyv:init-forms
;;$$OBSOLETE
(defconst utidyv:init-forms 
   '()
   "Init forms for the special variables." )
;;;_ , Variable registration
;;;_  . utidyv:register-var
;;$$OBSOLETE
(defun utidyv:register-var (sym init-form)
   "Register SYM as a special variable for `dynamic'.
If it's already registered, just change its init form."
   
   (let
      ((pos (position sym utidyv:vars :test #'eq)))
      (if pos
	 (setf (nth pos utidyv:init-forms) init-form)
	 (progn
	    (push sym utidyv:vars)
	    (push init-form utidyv:init-forms)))))
;;;_ , Functions
;;;_  . utidyv:top
(defmacro utidyv:top (sym-init-list &rest body)
   "Eval BODY with the special variables bound to their initial values."
   (let
      ((sym-init-list (eval sym-init-list)))
      `(progv 
	  ',(mapcar #'car sym-init-list)
	  (list ,@(mapcar #'second sym-init-list))
	  ,@body)))

;;;_  . utidyv:capture-vars

(defmacro utidyv:capture-vars (sym-init-list)
   "Capture the values of the formatter special variables."
   (let
      ((sym-list (eval sym-init-list)))
       `(list ',sym-list ,@sym-list)))
;;;_  . utidyv:with-vars
(defmacro utidyv:with-vars (data &rest body)
   "Eval BODY with the special variables bound according to DATA.
DATA should have been created by `utidyv:capture-vars'."
   
   `(progv (car data) (cdr data)
       ,@body))


;;;_. Footers
;;;_ , Provides

(provide 'utility/dynvars)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/dynvars.el ends here
