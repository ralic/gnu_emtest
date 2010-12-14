;;;_ utility/misc.el --- Miscellaneous utility functions

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

;; Miscellaneous utility functions


;;;_ , Requires

;;Nothing

;;;_. Body

;;;_ , utim:get-properties
(defun utim:get-properties (prop-sym prop-list)
   ""
   
   (let
      ((cell (assoc prop-sym prop-list)))
      (when cell
	 (second cell))))

;;;_  . utim:proper-list-p
;;Borrowed from `format'.
(defun utim:proper-list-p (list)
  "Return t if LIST is a proper list.
A proper list is a list ending with a nil cdr, not with an atom "
  (when (listp list)
    (while (consp list)
      (setq list (cdr list)))
    (null list)))

;;;_  . utim:constantp

;;We don't have real constants in Elisp.  This will have to suffice.

;;These symbols should be treated as literal, even tho they are
;;unbound.  So we simply make them self-evaluating.
(defconst utim:literal-symbols 
  '( &rest &body &optional &keys &aux)
  "" )
(dolist (sym utim:literal-symbols)
  (set sym sym))


;;This plays loosely with the difference between "constant" and
;;"literal".
(defun utim:constantp (form)
  "non-nil if FORM is a literal.
FORM must be an atom."

  (cond
    ;;A vector is constant if all its parts are constant
    ((vectorp form)
      (every #'utim:constantp form))

    ;;A symbol is considered constant if bound and self-evaluating.
    ((symbolp form)
      (and
	(boundp form)
	(eq
	  (symbol-value form)
	  form)
	;;`form' is self-evaluating, but it's an artifact of the fact
	;;that it's bound *here*, and it still isn't a literal.
	(not (eq form 'form))))

    ;;;;;;;;;;;
    ;;Do similarly for any other atoms that should be considered carefully.

    ;;All other atoms are considered constant.
    ((atom form)
      t)

    ;;Lists are only considered constant if quoted, otherwise they be
    ;;function calls.
    (t
      (eq
	(car form)
	'quote))))

;;;_ , utim:form-by-option

;;;###autoload
(defmacro utim:form-by-option (options key make-form &optional make-form-else)
   "Return a form that interprets KEY in OPTIONS.
MAKE-FORM and MAKE-FORM-ELSE should be functions of one variable that
return a form."

   `(let
      ((data (assoc ,key ,options)))
       ;;Only at eval-time do we know whether `key' is found in
       ;;`options'.
      (if data
	 ;;At expansion-time we know whether `make-form' and
	 ;;`make-form-else' are given.
	 ,(if make-form `(funcall ,make-form data))
	 ,(if make-form-else `(funcall ,make-form-else data)))))




;;;_. Footers
;;;_ , Provides

(provide 'utility/misc)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/misc.el ends here
