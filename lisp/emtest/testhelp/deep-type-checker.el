;;;_ emtest/testhelp/deep-type-checker.el --- Deep type checker for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: maint, lisp

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

;;To validate types, including structure slots.
;;See [[file:../docs/deep-type-checker.org]]

;;;_ , Requires

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(require 'cl)
(require 'emtest/runner/emt-funcall) ;;Obsolescent

;;;_. Body
;;;_ , Control variable

(defconst emty:use nil 
   "Control variable for deep checking.  Always globally nil." )

;;;_ , Type specs
;;;_  . type bool

(deftype bool ()
   '(member t nil))

;;;_  . type cons (with optional arguments)
;;This redefines a deftype that `cl' provides.  Keep args optional for
;;comptibility.

(deftype cons (&optional a b) 
   `(satisfies 
       (lambda (obj)
	  (emty:cons-f obj ',a ',b))))

;;;_   , Helper emty:cons-f

(defun emty:cons-f (obj a b)
   ""
   (and
      (consp obj)
      (if (eq a '*) t
	 (emt:funcall #'typep (car obj) a))
      (if (eq b '*) t
	 (emt:funcall #'typep (cdr obj) b))))

;;;_  . type list


(deftype list (&rest el-type-list) 
   `(satisfies 
       (lambda (obj)
	  (emty:list-f obj ',el-type-list))))

;;;_   , Helper emty:list-f

(defun emty:list-f (obj el-type-list)
   ""
   (if
      (null el-type-list)
      (null obj)
      (and
	 (if
	    (eq (car el-type-list) '*)
	    t
	    (emt:funcall #'typep (car obj) (car el-type-list)))
	 (emty:list-f (cdr obj) (cdr el-type-list)))))

;;;_  . type repeat

(deftype repeat (&optional el-type) 
   (if
      (eq el-type '*)
      '(or null cons)
      `(satisfies 
	  (lambda (obj)
	     (emty:repeat-f obj ',el-type)))))

;;;_   , Helper emty:repeat-f

(defun emty:repeat-f (obj el-type)
   ""
   (or
      (null obj)
      (and
	 (consp obj)
	 (emt:funcall #'typep (car obj) el-type)
	 (typep (cdr obj) `(repeat ,el-type)))))

;;;_  . type list*
(deftype list* (&rest r)
   `(satisfies 
       (lambda (obj)
	  (emty:list*-f obj ',r))))

;;;_  . Helper emty:list*-f
(defun emty:list*-f (obj r)
   ""
   (if (cdr r)
      (and
	 (emt:funcall #'typep (car obj) (car r))
	 (emty:list*-f (cdr obj) (cdr r)))
      (emt:funcall #'typep obj (car r))))


;;;_ , Entry point emty:check-f
;;;###autoload
(defun emty:check-f (value type &optional string)
   "Return non-nil if VALUE is deeply of type TYPE.
An alternate entry point for `emty:check'.
Here the args are values, not forms."
   (eval
      `(emty:check ',value ,type)))

;;;_  . Tests
;;It was tested thru another module (eg.el)

;;;_ , Entry point emty:check
;;;###autoload
(defmacro emty:check (form type &optional string)
   "Return non-nil if FORM evaluates to a value of type TYPE."
   `(let
       ((emty:use t))
       (check-type ,form ,type ,string)))
;;;_ , emty:get-type-pred-sym
;;Excerpted from cl-macs.  Don't need the other branches.
(defsubst emty:get-type-pred-sym (type-sym)
   "Return the type-predicate symbol for TYPE-SYM"
   (let*
      ((name (symbol-name type-sym)))
      (intern (concat name "-p"))))


;;;_ , emty:struct-slots-correct-p
(defun emty:struct-slots-correct-p (val type-sym)
   "Return non-nil just if VAL is of TYPE-SYM and has the correct types in
its slots, recursively."

   (and
      ;;Fail early if VAL is not of type TYPE-SYM at all.
      (funcall 
	 (emty:get-type-pred-sym type-sym)
	 val)
      (if emty:use
	 (let
	    (  (slots (get type-sym 'cl-struct-slots)))
	    (if slots
	       (every
		  #'(lambda (slot-spec slot-val)
		       (if
			  (and
			     (consp slot-spec)
			     (not (eq (car slot-spec) 'cl-tag-slot)))
			  (apply
			     (function* 
				(lambda (slot-name &optional init-form 
					   &key type)
				   (if type
				      (emt:funcall #'typep slot-val type)
				      ;;A typeless slot accepts anything
				      t)))
			     slot-spec)
			  ;;A bare slot is typeless.  Does this condition
			  ;;actually occur?
			  t))
		  slots 
		  val)

	       (error "Struct %s has no slot spec" type-sym)))
	 t)))


;;;_ , advice for `cl-make-type-test'

(defadvice cl-make-type-test (around emty:use-slot-types 
		    (value type)
		    act )
   "If type is a symbol that names a structure, also test the
structure's slots."

   (if
      ;;If it appear to be the name of a defstruct...
      (and
	 (symbolp type)
	 (get type 'cl-struct-type))
      ;;...test it as a structure
      (setq ad-return-value
	 `(emty:struct-slots-correct-p ,value ',type))
      ;;For anything else, test as normal.
      ad-do-it))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/deep-type-checker)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/deep-type-checker.el ends here
