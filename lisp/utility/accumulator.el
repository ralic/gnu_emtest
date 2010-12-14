;;;_ utility/accumulator.el --- Support for form accumulators

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: extensions, lisp

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

(eval-when-compile
   (require 'cl))

;;;_. Body
;;;_ , Defining a structure
;;;_  . Helper type
;;Bootstrapping issue: This type can't be defined by `utiacc:define'
;;because it is used by it.
(defstruct (utiacc:formdata
	      (:type list)
   	      (:copier nil)
	      (:conc-name utiacc:formdata->)
	      (:constructor utiacc:make-formdata))
   "Form data to construct an accumulator constructor's arglist"
   key-arg-forms
   aux-arg-forms
   slot-def-forms)



;;;_  . utiacc:related-sym
(defsubst utiacc:related-sym (slot string)
   ""
   (intern (concat (symbol-name slot) string)))

;;;_  . one slot to arg definitions
(defun utiacc:slot->arg-forms (slot)
   ""
   (destructuring-bind 
      (name &optional default &key type readonly)
      (if (listp slot) slot (list slot))
      (let
	 (  
	    ;;If we weren't passed type, default to universal type.
	    ;;`destructuring-bind' doesn't support defaults or svars,
	    ;;so we convert TYPE just if it's `nil'.  Not 100% correct
	    ;;but the `nil' type is useless here anyways.
	    (type (or type '*))
	    ;;Make all the related names.
	    (name-SINGLE 
	       (utiacc:related-sym name "-SINGLE"))
	    (name-SINGLE-seen 
	       (utiacc:related-sym name "-SINGLE-seen"))
	    (name-LIST 
	       (utiacc:related-sym name "-LIST")))
	 (utiacc:make-formdata

	    ;;These are after &key in ctor arglist
	    :key-arg-forms
	    `(  (,name-SINGLE () ,name-SINGLE-seen) 
		(,name-LIST
		   (if ,name-SINGLE-seen
		      (list ,name-SINGLE))))

	    ;;These are after &aux in ctor arglist
	    :aux-arg-forms
	    ;;`((,name ,name-LIST))
	    ()

	    ;;These are slot definitions.
	    :slot-def-forms
	    `((,name-LIST ,default 
		 :type (repeat ,type)
		 ,@(if readonly '(:readonly readonly))))))))



;;;_   , Tests
(rtest:deftest utiacc:slot->arg-forms


   (  "Param: A slot symbol.
Return: A list of forms for an arglist."
      (let
	 (  (formdata (utiacc:slot->arg-forms 'foo)))
	 
	 ;;This is too fragile, should be an inspection in case this
	 ;;changes.
	 (assert
	    (equal
	       (utiacc:formdata->key-arg-forms
		  formdata)
	       '(  (foo-SINGLE () foo-SINGLE-seen) 
		   (foo-LIST
		      (if foo-SINGLE-seen
			 (list foo-SINGLE)))))
	    t)
	 ;;Again too fragile, should be an inspection.
	 (assert
	    (equal
	       (utiacc:formdata->aux-arg-forms
		  formdata)  ;;'((foo foo-LIST))
	       '())
	    t)

	 ;;For aux
	 (let (dummy-func)
	    (eval
	       `(defun* dummy-func 
		   ,(utiacc:slots->arglist formdata) 
		   foo-LIST))
	    (assert
	       (equal
		  (dummy-func :foo-LIST '(12))
		  '(12))
	       t)
	    (assert
	       (equal
		  (dummy-func :foo-SINGLE 12)
		  '(12))
	       t))
	 
	 t))
   
   
   )
;;;_  . utiacc:slots->arglist
(defun utiacc:slots->arglist (formdata)
   ""
   `(&key 
       ,@(utiacc:formdata->key-arg-forms formdata)
       ;;`&aux' are no longer used for this.
;;        &aux
;;        ,@(utiacc:formdata->aux-arg-forms formdata)
       ))


;;;_  . utiacc:define

(defmacro utiacc:define (name-x &rest slots)
   "Define an accmulator type.
Basically a specialized `defstruct'"
   
   (let*
      (
	 (name-form (if (listp name-x) name-x (list name-x)))
	 (name
	    (car name-form))
	 (docstring (if (stringp (car slots)) (car slots)))
	 (slots (if docstring (cdr slots) slots))
	 (formdata
	    (utiacc:collect
	       #'utiacc:slot->arg-forms
	       (mapcar #'list slots)
	       'utiacc:formdata))
	 ;;NAME-FORM may contain (:constructor name)
	 (ctor-spec
	    (assoc :constructor name-form))
	 (ctor-sym
	    (if
	       (and ctor-spec (not (cddr ctor-spec)))
	       ;;We may have been given ctor name.
	       (second ctor-spec)
	       ;;Otherwise build constructor name.
	       (intern (concat "make-" (symbol-name name))))))
      
      `(defstruct
	  (,name
	     (:constructor ,ctor-sym
		,(utiacc:slots->arglist formdata))
	     ;;Include unchanged the args other than the ctor
	     ,@(remove* ctor-spec (cdr name-form) :test #'eq))
	  ,@(utiacc:formdata->slot-def-forms formdata))))

;;;_   , Tests
(rtest:deftest utiacc:define

   (  "Situation: Dummy-type is defined.  Bare name, bare slot-names.
Sub-situation: Cted, no parameters given
Response: All fields are empty lists."

      (let-noprops '(dummy-type)
	 (utiacc:define dummy-type a b)
	 
	 (let
	    ((obj (make-dummy-type)))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  ())
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 ;;List keys assign OK
	 (let
	    ((obj (make-dummy-type :a-LIST '(12))))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  '(12))
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 (let
	    ((obj (make-dummy-type :a-SINGLE 12)))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  '(12))
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 ;;Non-list slots cause `wrong-type-argument' error (either in
	 ;;construction or in type-checking, test doesn't commit to
	 ;;which)
	 (assert
	    (emth:gives-error
	       (emty:check
		  (make-dummy-type :a-LIST 13)
		  dummy-type)
	       wrong-type-argument))
	 
	 t))
   
   (  "Situation: Dummy-type is defined.  
Name is list-y and includes a constructor name (no arglist).
Bare slot-names.
Passes same tests as before."

      (let-noprops '(dummy-type)
	 (utiacc:define 
	    (dummy-type 
	       (:constructor make-under-a-different-name)) 
	    a b)
	 
	 (let
	    ((obj (make-under-a-different-name)))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  ())
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 ;;List keys assign OK
	 (let
	    ((obj (make-under-a-different-name :a-LIST '(12))))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  '(12))
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 (let
	    ((obj (make-under-a-different-name :a-SINGLE 12)))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  '(12))
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 (assert
	    (emth:gives-error
	       (emty:check
		  (make-under-a-different-name :a-LIST 13)
		  dummy-type)
	       wrong-type-argument))
	 t))

   (  "Situation: Dummy-type is defined.  Bare name, bare slot-names.
Definition has a docstring.
Passes same tests as before."

      (let-noprops '(dummy-type)
	 (utiacc:define dummy-type
	    "A docstring"
	    a b)
	 
	 (let
	    ((obj (make-dummy-type :a-SINGLE 12)))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  '(12))
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))
	 t))

   ("Situation: Dummy-type is defined.  
Name is simple
Slot-names are list-y and give type.
Passes essentially same tests as before."
      (let-noprops '(dummy-type)
	 (utiacc:define dummy-type 
	    (a 1 :type integer)
	    (b "bee" :type string))
	 
	 (let
	    ((obj (make-dummy-type)))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  ())
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 ;;List keys assign OK
	 (let
	    ((obj (make-dummy-type :a-LIST '(12))))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  '(12))
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 (let
	    ((obj (make-dummy-type :a-SINGLE 12)))
	    (assert
	       (equal
		  (dummy-type-a-LIST obj)
		  '(12))
	       t)
	    (assert
	       (equal
		  (dummy-type-b-LIST obj)
		  ())
	       t))

	 ;;Stronger slotwise type-checking is done.  The slots expect
	 ;;a list of whatever type was defined for them.
	 (assert
	    (emth:gives-error
	       (emty:check
		  (make-dummy-type :a-LIST '("string"))
		  dummy-type)
	       wrong-type-argument))
	 (assert
	    (not
	       (emth:gives-error
		  (emty:check
		     (make-dummy-type :b-LIST '("string"))
		     dummy-type)
		  wrong-type-argument)))
      
	 
	 t))
   
   )

;;;_ , utiacc:collect
(defun utiacc:collect (func clauses type)
   "Collect the aggregate return value from applying func to clauses.
FUNC is the function to transform a clause.  It takes any number of
arguments and returns a value of type TYPE.
CLAUSES is a list of clauses, each clause being a list.
TYPE is the type of the return value, given as a symbol."
   
   (utiacc:list->object
      (mapcar 
	 #'(lambda (form)
	      (apply func form))
	 clauses)
      type))

(defun utiacc:list->object (formdata-list type)
   "Make a list of values of TYPE into an object of TYPE.  
Each field in the result is a list which contains all the contents of the
respective fields of the input.
TYPE is the type of the return value, given as a symbol."
   (let*
      (
	 (struct-type ;;;'list or 'vector
	    (car (get type 'cl-struct-type)))
	 (slot-specs
	    (get type 'cl-struct-slots))
	 (named-p
	    (equal (car slot-specs) '(cl-tag-slot)))
	 (first-slot
	    (if named-p 1 0))
	 (num-phys-slots
	    (length slot-specs))
	 (num-slots
	    (- num-phys-slots first-slot))
	 (new-vals
	    (utiacc:transpose 
	       formdata-list 
	       num-slots 
	       (case struct-type
		  (list
		     #'(lambda (i el)
			  (nth (+ first-slot i) el)))
		  (vector
		     #'(lambda (i el)
			  (aref el (+ first-slot i)))))))
	 (new-slots
	    ;;If type is named, first slot is type tag.
	    (if named-p
	       (let
		  ((tag (intern (concat "cl-struct-" (symbol-name type)))))
		  (cons tag new-vals))
	       ;;Otherwise the slots are just the values.
	       new-vals)))
      (if
	 (eq struct-type 'vector)
	 (apply #'vector new-slots)
	 new-slots)))
;;;_  . Helper utiacc:transpose
(defun utiacc:transpose (row-list num-slots get-val)
   ""
   (let
      ((vals ()))
      ;;Could shortcut if (= num-slots 1), but there is little to be
      ;;gained.
      (dotimes (i num-slots (nreverse vals))
	 (push
	    (apply #'append
	       (mapcar 
		  #'(lambda (el)
		       (funcall get-val i el))
		  row-list))
	    vals))))



;;;_  . Test data (Not written)
;;;_   , Structure
;;;_   , Clause handler
;;;_  . Tests
;;Tested thru tp (where it originally came from) and tagnames


;;;_. Footers
;;;_ , Provides

(provide 'utility/accumulator)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/accumulator.el ends here
