;;;_ utility/field.el --- Field access shortcuts

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: 

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

;;;_. Body

;;;_ , Advice to defstruct

(defadvice defstruct (after emt:field:remember-names
		    (name &rest r)
		    act )
   "Store any arguments in the property `emt-field:struct-parameters'
For instance, conc-name or predicate."
   
   (if (consp name)
      (destructuring-bind
	 (struct-name &rest rest)
	 name
	 (if rest
	    (setq
	       ad-return-value
	       `(progn
		   (put
		      ',struct-name
		      'emt-field:struct-parameters
		      ',(emt-field:defstruct-args->key-form rest))
		   ,ad-return-value))))))

;;;_  . Helper emt-field:defstruct-args->key-form
(defun emt-field:defstruct-args->key-form (args)
   ""
   
   (apply #'append
      (mapcar 
	 #'(lambda (arg)
	      (if (listp arg) arg `(,arg t)))
	 args)))

;;;_   , Tests
(rtest:deftest emt-field:defstruct-args->key-form

   (  "Param: typical defstruct args
Response: Returns it as a keyable list."
      (progn
	 (assert
	    (equal
	       (emt-field:defstruct-args->key-form
		  '((:conc-name 'fizzbin))
		  )
	       '(:conc-name 'fizzbin))
	    t)
	 t))
   
   (  "Param: singleton defstruct arg
Response: Returns it as a keyable list."
      (progn
	 (assert
	    (equal
	       (emt-field:defstruct-args->key-form
		  '(:named)
		  )
	       '(:named t))
	    t)
	 t)))


;;;_  . emt-field:get-conc-name

(defun emt-field:get-conc-name (type)
   ""

   (apply
      (function*
	 (lambda (&key conc-name &allow-other-keys)
	    conc-name))
      (get type 'emt-field:struct-parameters)))
;;;_   , Tests
(put 'emt-field:get-conc-name 'rtest:test-thru
   'defstruct)

;;;_  . Tests

(rtest:deftest defstruct
   (  "Param: conc-name is given
Response: The property emt-field:conc-name holds that name."
      (let-noprops
	 '(struct1)
	 (defstruct (struct1 (:conc-name fizzbin))
	    field1)
	 
	 (assert
	    (equal
	       (emt-field:get-conc-name 'struct1)
	       'fizzbin)
	    t)
	 t)))

;;;_ , emt-field:get-slot-accessor


;;This doesn't expect to get slot-names predeceded by colon, like ":foo"
(defun emt-field:get-slot-accessor (struct-sym slot-sym &optional conc-name)
   "Get the slot accessor for SLOT-SYM in STRUCT-SYM.

This function tries hard to find it, using conc-name if it exists,
then trying different name-separators.
If CONC-NAME is given, it will be used instead of STRUCT-SYM's
conc-name."
   
   (let*
      (
	 (conc-name-sym
	    (or conc-name
	       (emt-field:get-conc-name struct-sym)
	       struct-sym))
	 (struct-name
	    (symbol-name struct-sym))
	 (slot-name
	    (symbol-name slot-sym)))
      (or
	 (intern-soft
	    (concat
	       (symbol-name conc-name-sym)
	       slot-name))
	 (intern-soft
	    (concat
	       struct-name
	       "-"
	       slot-name))
	 (intern-soft
	    (concat
	       struct-name
	       "-"
	       slot-name))
	 (error "Can't find an accessor for %s in %s.
Reevaluating %s's structure definition might help, because this package
advises defstruct to store that information."
	    slot-sym
	    struct-sym
	    struct-sym))))

;;;_  . Tests
(rtest:deftest emt-field:get-slot-accessor

   (  "Situation: A conc-name was given to the struct definition.
Response: The accessor is found."
      (let-noprops
	 '(struct1)
	 (defstruct (struct1 (:conc-name fizzbin->))
	    field1)
	 
	 (assert
	    (equal
	       (emt-field:get-slot-accessor 'struct1 'field1)
	       (intern-soft 
		  'fizzbin->field1))
	    t)
	 t))
   
   (  "Situation: No conc-name was given to the struct definition.
Response: The accessor is found."
      (let-noprops
	 '(struct1)
	 (defstruct (struct1)
	    field1)
	 
	 (assert
	    (equal
	       (emt-field:get-slot-accessor 'struct1 'field1)
	       (intern-soft 
		  'struct1-field1))
	    t)
	 t)))

;;;_ , emt-field:get-all-slot-syms
(defun emt-field:get-all-slot-syms (type-sym)
   ""
   (let
      ((slots (get type-sym 'cl-struct-slots)))
      (unless slots
	 (error "Struct %s has no slot spec" type-sym))

      (delq
	 nil
	 (mapcar
	    #'(lambda (slot-spec)
		 (cond
		    ((not (consp slot-spec))
		       ;;Can a singleton ever legitimately occur?
		       (error "Slot specs should be lists"))
		    ((eq (car slot-spec) 'cl-tag-slot)
		       nil)
		    (t
		       (apply
			  (function
			     (lambda (slot-name &rest dummy)
				slot-name))
			  slot-spec))))
	    slots))))

;;;_  . Tests
(rtest:deftest emt-field:get-all-slot-syms

   (  "Situation: defstruct has one field.
Response: That field is found."
      (let-noprops
	 '(struct1)
	 (defstruct (struct1)
	    field1)
	 
	 (assert
	    (equal
	       (emt-field:get-all-slot-syms 'struct1)
	       '(field1))
	    t)
	 t)))


;;;_ , with-slots

(defmacro with-slots (slot-list obj &rest body)
   "Evaluate BODY with the symbols in SLOT-LIST bound to the respective
slots of OBJ.

This is essentially Common Lisp's with-slots for defstructs.  
Two differences:
  * This does not work if the type is not given.  Use the form \(the
TYPE-SYM OBJ) for OBJ.
  * If the symbol `all' is passed for SLOT-LIST, all the slots will be
available."
   
   (if
      (and
	 (listp obj)
	 (eq (first obj) 'the))
      
      (destructuring-bind
	 (*the* type-sym object-form)
	 obj
	 (let 
	    ((slot-list
		(if
		   (eq slot-list 'all)
		   slot-list
		   (emt-field:get-all-slot-syms type-sym)))
	       (object (gensym))
	       (conc-name (emt-field:get-conc-name type-sym)))
	    `(let
		((,object ,object-form))
		(symbol-macrolet 
		   ,(mapcar
		       ;;Maybe encap this
		       #'(lambda (slot-name)
			    `(,slot-name 
				( ,(emt-field:get-slot-accessor
				      type-sym
				      slot-name
				      conc-name)
				   ,object)))
		       slot-list) 
		   ,@body))))
      
      (error "Cannot determine the type.  
Use the form (the TYPE OBJ).")))

;;;_  . Test helpers
(defmacro with-all-slots:th (slot-list obj type-sym &rest rest)
   ""
   
   `(with-slots 'all (the ,type-sym ,obj) ,@rest))


;;;_  . Tests
(rtest:deftest with-slots

       
   ("It runs on normal structures"
      (rtest:retvalue->status
	 (equal
	    (with-all-slots:th
	       (my-field my-second-field)
	       (make-rtest-struct :my-field "a")
	       rtest-struct
	       (list my-field my-second-field))
	    '("a" nil))))


   ("It runs on list structures"
      (rtest:retvalue->status
	 (equal
	    (with-all-slots:th
	       (my-field my-second-field)
	       (make-rtest-struct-list :my-field "a")
	       rtest-struct-list
	       (list my-field my-second-field))
	    '("a" nil))))

   ("It evaluates the object only once"
      (rtest:retvalue->status
	 (equal
	    (let
	       ((a 0))
	       (with-all-slots:th
		  (my-field my-second-field)
		  (make-rtest-struct :my-field
		     (incf a))
		  rtest-struct
		  (list my-field my-second-field))
	       a)
	    1)))

   ("Proves:  It can be used to set a new value.
Context: In the body form
Operation: setq the name to a different value.
Result: The field name now has that value."
      (rtest:retvalue->status
	 (progn
	    (let
	       ((obj (make-rtest-struct :my-field "a")))
	       (with-all-slots:th
		  ()
		  obj
		  rtest-struct
		  (setq my-field "b")
		  (assert
		     (equal my-field "b")
		     t))
	       (assert
		  (equal (rtest-struct-my-field obj) "b")
		  t)
	       t)))))




;;;_. Footers
;;;_ , Provides

(provide 'utility/field)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/field.el ends here
