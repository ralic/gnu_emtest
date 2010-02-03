;;;_ deep-type-checker.el --- Deep type checker for Emtest

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
(require 'emt-funcall "tester/emt-funcall")

;;;_. Body
;;;_ , Control variable

(defconst emt:deep-type:use nil 
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
	  (deep-type:cons-f obj ',a ',b))))

;;;_   , Helper deep-type:cons-f

(defun deep-type:cons-f (obj a b)
   ""
   (and
      (consp obj)
      (if (eq a '*) t
	 (emt:funcall #'typep (car obj) a))
      (if (eq b '*) t
	 (emt:funcall #'typep (cdr obj) b))))
(put 'deep-type:cons-f 'rtest:test-thru
   'cons)

;;;_   , Tests

(rtest:deftest cons
   
   (  "Situation: Spec args are not given
Response: Accept just cons cells."
      (progn
	 (assert
	    (not
	       (typep "Not a list" '(cons)))
	    t)
	 (assert
	    (typep '(1 2) '(cons))
	    t)
	 t))
   

   (  "Situation: Spec args are both `*'.
Response: Accept just cons cells."
      (progn
	 (assert
	    (not
	       (typep "Not a list" '(cons * *)))
	    t)
	 (assert
	    (typep '(1 2) '(cons * *))
	    t)
	 t))


   (  "Situation: Spec args are given.
Response: Accept just cons cells whose parts match the specs."
      (progn
	 (assert
	    (not
	       (typep '(13 . 13) '(cons integer string)))
	    t)
	 (assert
	    (not
	       (typep '("No" . "Nope") '(cons integer string)))
	    t)
	 (assert
	    (typep '(12 . "Yes") '(cons integer string))
	    t)
	 
	 t))
      
   
   )

;;;_  . type list


(deftype list (&rest el-type-list) 
   `(satisfies 
       (lambda (obj)
	  (deep-type:list-f obj ',el-type-list))))

;;;_   , Helper deep-type:list-f

(defun deep-type:list-f (obj el-type-list)
   ""
   (if
      (null el-type-list)
      (null obj)
      (and
	 (if
	    (eq (car el-type-list) '*)
	    t
	    (emt:funcall #'typep (car obj) (car el-type-list)))
	 (deep-type:list-f (cdr obj) (cdr el-type-list)))))

;;;_   , Tests
(rtest:deftest list

   (  "Situation: Spec is empty
Response: Accept only nil."
      (progn
	 (assert
	    (not
	       (typep "Not a list" '(list)))
	    t)
	 (assert
	    (not
	       (typep '("A list") '(list)))
	    t)
	 (assert
	 
	       (typep '() '(list))
	    t)
	 t))
   
   (  "Situation: Spec has one arg
Response: Accept only lists of one el that matches that type."
      (progn
	 (assert
	 
	       (typep '(12) '(list integer))
	    t)
	 (assert
	    (not
	       (typep '("Not an integer") '(list integer)))
	    t)
	 (assert
	    (not
	       (typep '() '(list integer)))
	    t)
	 (assert
	    (not
	       (typep '(13 13) '(list integer)))
	    t)
	 t))
   
   (  "Situation: Spec has three args.  One is wildcard `*'
Response: Accepts only lists of two els, where the respective one
matches each type."
      (progn
	 (assert
	    (typep '(12 "string" 12) '(list integer * integer))
	    t)
	 (assert
	    (typep '(12 'symbol 12) '(list integer * integer))
	    t)
	 (assert
	    (not (typep '(12 "string") '(list integer * integer)))
	    t)
	 (assert
	    (not (typep '(12 "string" 12 12) '(list integer * integer)))
	    t)
	 (assert
	    (not (typep '("A string" "string" 12) '(list integer * integer)))
	    t)
	 (assert
	    (not (typep '(12 "string" "A string") '(list integer * integer)))
	    t)
	 t))
   )

;;;_  . type repeat

(deftype repeat (&optional el-type) 
   (if
      (eq el-type '*)
      '(or null cons)
      `(satisfies 
	  (lambda (obj)
	     (deep-type:repeat-f obj ',el-type)))))

;;;_   , Helper deep-type:repeat-f

(defun deep-type:repeat-f (obj el-type)
   ""
   (or
      (null obj)
      (and
	 (consp obj)
	 (emt:funcall #'typep (car obj) el-type)
	 (typep (cdr obj) `(repeat ,el-type)))))

;;;_   , Tests
(rtest:deftest repeat

   (  "Situation: Spec is just any list.  Pass non-list
Response: Fail."
      (not
	 (typep "Not a list" '(repeat))))
   
   (  "Situation: Spec is just any list.  Pass a list
Response: Succeed."
      (typep '(12 "List of" "anything") '(repeat)))

   (  "Situation: Spec is a list of integer.  Pass non-list
Response: Fail."
      (not
	 (typep "Not a list" '(repeat integer))))
   
   (  "Situation: Spec is a list of integer.  Pass a list of non-integers
Response: Fail."
      (not
	 (typep '("Not an integer") '(repeat integer))))

   (  "Situation: Spec is a list of integer.  Pass a list of integers
Response: Succeed."
      (typep '(12 144) '(repeat integer)))

   (  "Situation: Spec is a list of integer.  Pass a mixed list of
integers and non-integers.
Response: Fail."
      (not
	 (typep '(12 "Not an integer") '(repeat integer))))
                  
   )
;;;_  . type list*
(deftype list* (&rest r)
   `(satisfies 
       (lambda (obj)
	  (deep-type:list*-f obj ',r))))

;;;_  . Helper deep-type:list*-f
(defun deep-type:list*-f (obj r)
   ""
   (if (cdr r)
      (and
	 (emt:funcall #'typep (car obj) (car r))
	 (deep-type:list*-f (cdr obj) (cdr r)))
      (emt:funcall #'typep obj (car r))))

(put 'deep-type:list*-f 'rtest:test-thru
   'list*)

;;;_   , Tests
(rtest:deftest list*

   (  "Situation: There's only the last element
Response: Last element must match the rest of the list."
      (progn
	 (assert
	    (typep '(12 144) '(list* (repeat integer)))
	    t)
	 t))

   (  "Situation: There's only the last element
Response: Last element must match the rest of the list."
      (progn
	 (assert
	    (typep '("Alpha" 144) '(list* (list string integer)))
	    t)
	 t))

      (  "Situation: There's only the last element
Response: Last element must match the rest of the list."
	 (progn
	    (assert
	       (not
		  (typep '(144 "Alpha") '(list* (list string integer))))
	       t)
	    t))
   
   (  "Situation: There are previous elements
Response: The initial elements must match them."
      (progn
	 (assert
	    (typep '(12 "Alpha" 144) '(list* integer string (repeat integer)))
	    t)
	 t))
   (  "Situation: There are previous elements
Response: The initial elements must match them."
      (progn
	 (assert
	    (not
	       (typep '(13 13 144) '(list* integer string (repeat integer))))
	    t)
	 t))      
   )
;;;_ , Entry point emt:deep-type:check-f
;;;###autoload
(defun emt:deep-type:check-f (value type &optional string)
   "Return non-nil if VALUE is deeply of type TYPE.
An alternate entry point for `emt:deep-type:check'.
Here the args are values, not forms."
   (eval
      `(emt:deep-type:check ',value ,type)))

;;;_  . Tests
;;It was tested thru another module (eg.el)

;;;_ , Entry point emt:deep-type:check
;;;###autoload
(defmacro emt:deep-type:check (form type &optional string)
   "Return non-nil if FORM evaluates to a value of type TYPE."
   `(let
       ((emt:deep-type:use t))
       (check-type ,form ,type ,string)))
;;;_  . Tests
(put 'emt:deep-type:check 'rtest:test-thru
   'cl-make-type-test)
;;;_ , emt:deep-type:get-type-pred-sym
;;Excerpted from cl-macs.  Don't need the other branches.
(defsubst emt:deep-type:get-type-pred-sym (type-sym)
   "Return the type-predicate symbol for TYPE-SYM"
   (let*
      ((name (symbol-name type-sym)))
      (intern (concat name "-p"))))


;;;_ , emt:deep-type:struct-slots-correct-p
(defun emt:deep-type:struct-slots-correct-p (val type-sym)
   "Return non-nil just if VAL is of TYPE-SYM and has the correct types in
its slots, recursively."

   (and
      ;;Fail early if VAL is not of type TYPE-SYM at all.
      (funcall 
	 (emt:deep-type:get-type-pred-sym type-sym)
	 val)
      (if emt:deep-type:use
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

;;;_  . Tests
(put 'emt:deep-type:struct-slots-correct-p 'rtest:test-thru
   'cl-make-type-test)


;;;_ , advice for `cl-make-type-test'

(defadvice cl-make-type-test (around emt:deep-type:use-slot-types 
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
	 `(emt:deep-type:struct-slots-correct-p ,value ',type))
      ;;For anything else, test as normal.
      ad-do-it))


;;;_  . Tests
(rtest:deftest cl-make-type-test

   ;;`typep' still works normally
   (  "`nil' type is not mangled."
      (not
	 (typep 12 nil)))
   

   (  "Built-in types work (integer)."
      (progn
	 (assert (typep 12 'integer) t)
	 (assert (not (typep "String" 'integer)) t)
	 t))
   
   (  "Compound types work."
      (progn
	 (assert (typep 12       '(or integer string)) t)
	 (assert (typep "String" '(or integer string)) t)
	 (assert (not (typep '(repeat) '(or integer string))) t)
	 t))

   ;;`emt:deep-type:check' works on built-in types

   (  "Built-in types work (integer)."
      (progn
	 (emt:deep-type:check 12 integer)
	 (emt:gives-error
	    (emt:deep-type:check "String" integer))
	 t))
   
   (  "Compound types work."
      (progn
	 (emt:deep-type:check 12       (or integer string))
	 (emt:deep-type:check "String" (or integer string))
	 (emt:gives-error
	    (emt:deep-type:check '(a list) (or integer string)))
	 t))

   ;;Sanity checks

   (  "Sanity checks for `cl-make-type-test'."
      (let
	 ((emt:deep-type:use t))
	 (let-noprops
	    '(struct1)
	    (defstruct struct1
	       field1)
	    ;;It makes something, not `nil'
	    (assert
	       (cl-make-type-test 
		  (make-struct1 :field1 12) 
		  'struct1)
	       t)
	 
	    ;;The form it makes returns non-nil on correct inputs.
	    (assert
	       (eval
		  (cl-make-type-test 
		     (make-struct1 :field1 12) 
		     'struct1))
	       t)

	    t)))
   

   ;;Normal structures.

   (  "Situation: A normal struct definition, no slot type spec.
Response: Typep passes it no matter what's in the field."
      (let-noprops
	 '(struct1)
	 (defstruct struct1
	    field1)

	 (emt:deep-type:check
	    (make-struct1
	       :field1 '("Any" thing "at" all))
	    struct1)
	 t))
   
   (  "Situation: A struct definition with a slot type spec.
`typep' is used and the flag `emt:deep-type:use' has its global
value of `nil.'
Response: Typep passes it no matter what's in the field."
      (let-noprops
	 '(struct1)
	 (defstruct struct1
	    (field1 nil :type integer))
	 (assert (not emt:deep-type:use))
	 (assert
	    (typep
	       (make-struct1
		  :field1 '("Any" thing "at" all))
	       'struct1)
	    t)
	 t))

   (  "Situation: A struct definition with a slot type spec.
`typep' is used, and the flag `emt:deep-type:use' is let non-nil.
Response: Typep passes it no matter what's in the field."
      (let
	 ((emt:deep-type:use t))
	 (let-noprops
	    '(struct1)
	    (defstruct struct1
	       (field1 nil :type integer))
	    (assert
	       (not
		  (typep
		     (make-struct1
			:field1 '("Any" thing "at" all))
		     'struct1))
	       t)
	    t)))

   (  "Situation: A struct definition with a simple slot type spec."
      (let-noprops
	 '(struct1)
	 (defstruct struct1
	    (field1 nil :type integer))

	 
	 ;;Object has the wrong type for that slot.
	 ;;Response: emt:deep-type:check rejects it.
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 '("Some" thing else))
		  struct1)))
	 ;;Object has the right type for that slot.
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1 12)
	       struct1)
	 t))

   (  "Situation: A struct definition with a simple slot type spec.
The name spec is not a bare symbol"
      (let-noprops
	 '(struct1)
	 (defstruct (struct1 (:type vector) :named)
	    (field1 nil :type integer))

	 ;;Object has the wrong type for that slot.
	 ;;Response: emt:deep-type:check rejects it.
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 '("Some" thing else))
		  struct1)))
	 ;;Object has the right type for that slot.
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1 12)
	       struct1)
	 t))

   (  "Situation: A struct definition with several slot type specs."
      (let-noprops
	 '(struct1)
	 (defstruct struct1
	    (field1 nil :type integer)
	    (field2 nil :type string)
	    )
	 ;;Object has the wrong type for one slot.
	 ;;Response: emt:deep-type:check rejects it.
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 '(something wrong)
		     :field2 "Correct")
		  struct1)))
	 ;;Object has the wrong type for the other slot.
	 ;;Response: emt:deep-type:check rejects it.
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 12
		     :field1 '(something wrong))
		  struct1)))
	 ;;Object has the right type for both slots.
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1 12
		  :field2 "Correct")
	       struct1)
	 t))


   (  "Situation: Slot type spec is to another struct."
      (let-noprops
	 '(struct0 struct1)
	 (defstruct struct0
	    field0)
	 (defstruct struct1
	    (field1 nil :type struct0))


	 ;;Object has the wrong type for that slot.
	 ;;Response: emt:deep-type:check rejects it.
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 '("Some" thing else))
		  struct1)))
	 ;;Object has the right type for that slot.
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1 (make-struct0 :field0 "Anything"))
	       struct1)
	 t))
   
   (  "Situation: Slot type spec is a list of a built-in type
Object has the right type for that slot.
Response: emt:deep-type:check accepts it."
      (let-noprops
	 '(struct1)
	 (defstruct struct1
	    (field1 nil :type (repeat integer)))


	 ;;Object has the empty list for that slot.
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1 '())
	       struct1)

	 ;;Object has the wrong type for that slot.
	 ;;Response: emt:deep-type:check rejects it.
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 '("Some" thing else))
		  struct1)))
	 ;;Object has the right type for that slot.
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1 '(12))
	       struct1)
	 t))
   
   (  "Situation: Slot type spec is to a list of another struct."
      (let-noprops
	 '(struct0 struct1)
	 (defstruct struct0
	    field0)
	 (defstruct struct1
	    (field1 nil :type (repeat struct0)))

	 ;;Object has the wrong type for that slot - not even a list. 
	 ;;Response: emt:deep-type:check rejects it.
	 
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 13)
		  struct1)))

	 ;;Object has the wrong element type for that slot.
	 ;;Response: emt:deep-type:check rejects it.
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 '(("Some" thing else)))
		  struct1)))
	 ;;Object has the right type for that slot.
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1 (list (make-struct0 :field0 "Anything")))
	       struct1)
	 t))


   (  "Situation: Slot type spec is a list of its own type.
Object has the right type for that slot.
Response: emt:deep-type:check accepts it."
      (let-noprops
	 '(struct1)
	 (defstruct struct1
	    (field1 nil :type (repeat struct1)))

	 ;;Object has the wrong type for that slot.
	 ;;Response: emt:deep-type:check rejects it.
	 (assert
	    (emt:gives-error
	       (emt:deep-type:check
		  (make-struct1
		     :field1 '("Any" thing "at" all))
		  struct1)))
	 ;;Object has the right type for that slot.  Shallow nesting,
	 ;;1 more ply..
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1
		  (list
		     (make-struct1 :field1 ())))
	       struct1)
	 ;;Object has the right type for that slot.  Deep nesting, 2
	 ;;more plies. 
	 ;;Response: emt:deep-type:check accepts it.
	 (emt:deep-type:check
	       (make-struct1
		  :field1
		  (list
		     (make-struct1 
			:field1 
			(list
			   (make-struct1 :field1 ())))))
	       struct1)
	 t))

   ;;Uses this module for type-checking it, so depends on many other
   ;;tests here.
   (  "Proves: Adds a result of the appropriate type to
`emt:trace:stored-diag'."
      (let
	 (emt:trace:stored-diag)
	 (let-noprops
	    '(struct1)
	    (defstruct struct1
	       (field1 nil :type integer))
	 
	    ;;Situation: We've made a check that succeeds (because it's
	    ;;the same as an earlier one that passed)
	    (emt:deep-type:check
	       (make-struct1
		  :field1 12)
	       struct1)
	 
	    (emt:trace:stored-diag:check-type)
	    t)))
   
   )


;;;_. Footers
;;;_ , Provides

(provide 'deep-type-checker)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; deep-type-checker.el ends here
