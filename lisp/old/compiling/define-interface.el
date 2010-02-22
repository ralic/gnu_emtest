;;;_ define-interface.el --- Utility to define interfaces

;;;_. Headers
;;;_ , License
;; Copyright (C) 2008  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: tools, lisp

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

(when (not (fboundp 'emt:deftest))
    (defmacro emt:deftest (&rest dummy))
    (defmacro emt:if-avail (&rest dummy)))
(eval-when-compile
   (require 'cl))


;;;_. Body
;;;_ , define-interface
(defmacro define-interface (name form-f-form doc &rest items)
   "Define an interface with the functions ITEMS.  

FORM-F-FORM should be a form that evaluates to a function that takes
an accessor and a list of args and returns a list consisting of

 * a form naming the extra leading elements of the overt arglist

 * a form that does the work of forwarding, possibly using additional
   objects that the formal arglist doesn't know about.

For now, the function arguments in ITEMS are strictly mnemonic"
   (let
      ;;Append "-vtable" to make the structure name so that it (and
      ;;particularly its accessor's names) will not be confused with
      ;;the generated function names.
      (
	 (vtable-name
	    (concat (symbol-name name) "-vtable")))
      
   `(progn
       (defstruct (,(intern vtable-name) )
	  ,doc 
	  ,@(mapcar #'car items))
       ,@(mapcar
	    #'(lambda (item)
		 (let*
		    ((field-name (car item))
		       (accessor-name
			  (intern
			     (concat vtable-name "-" (symbol-name field-name))))
		       (func-name
			  (intern 
			     (concat 
				(symbol-name name)
				"-"
				(symbol-name field-name))))
		       		       
		       (forms
			  (funcall (eval form-f-form)
			     accessor-name
			     '(r))))
		    
		    ;;func-name
		    `(defun ,func-name (,@(car forms) &rest r)
			"Automatically generated forwarding function"
			,(cadr forms))))
	    items))))

;;;_  . Tests

(rtest:deftest define-interface
   (  "Situation:  Extra arg list is empty and form maker punts, just
returns a constant.
Response:  Functions can be called and return that constant"
      (let
	 ;;Bind these locally so we don't see or disturb other bindings
	 (my-interface-foo my-interface-bar my-interface)

	 ;;Define an interface
	 (define-interface my-interface 
	    #'(lambda (&rest dummy)
		 (list
		    ()
		    100))
	    "Some docstring" 
	    (foo) 
	    (bar))
	 
	 (and
	    (equal (my-interface-foo) 100)
	    (equal (my-interface-bar) 100))))
   (  "Situation:  Extra arg list has one param and form maker just
returns it.
Response:  Functions can be called and return the same arg they give"
      (let
	 ;;Bind these locally so we don't see or disturb other bindings
	 (my-interface-foo my-interface-bar my-interface)

	 ;;Define an interface
	 (define-interface my-interface 
	    #'(lambda (accessor-form rest-form)
		 (list
		    '(this)
		    'this))
	    "Some docstring" 
	    (foo) 
	    (bar))
	 
	 (and
	    (equal (my-interface-foo 333) 333)
	    (equal (my-interface-bar 12) 12))))

   (  "Situation:  Single arg object, a cons cell of two parts."
      ;;This test is legacy from an older design, slightly adapted,
      ;;but it still helps test.
      (let
	 ;;Bind these locally so we don't see or disturb other bindings
	 (my-interface-foo my-interface-bar my-interface)

	 ;;Define an interface
	 (define-interface my-interface 
	    #'(lambda (accessor-form rest-form)
		 (list
		    '(this)
		    `(apply
			(funcall
			   #',accessor-form
			   (car this))
			(cdr this)
			,@rest-form)))
	    "Some docstring" 
	    (foo a) 
	    (bar a &optional b))

	 (let*
	    (  (interface
		  ;;Define an implementation whose functions just
		  ;;return what they got.
		  (make-my-interface-vtable 
		     :foo #'list
		     :bar #'list))
	       ;;Make an object of this type.  Here its content is just a
	       ;;number for simplicity.
	       (object
		  (cons interface 5000)))
      
	    (and
	       (equal (my-interface-foo object 1)    '(5000 1))
	       (equal (my-interface-bar object 10 2) '(5000 10 2))
	       (equal (my-interface-bar object 0)    '(5000 0))))))
   
   )

;;;_ , Define a dispatch-on-type style interface
;;;_  . Table element type

(defstruct emt:dispatch:table-el
   "The structure of an element in the dispatch table"
   match-p
   data)
;;;_  . Test data is later in this section
;;;_  . emt:dispatch:get-match

(defun emt:dispatch:get-match (obj table)
   "Return the data field of a match in the table.
If none is found, return `nil'."
   
   (let
      ((result nil))
      (dolist (entry table result)
	 (if
	    (funcall 
	       (emt:dispatch:table-el-match-p entry) 
	       obj)
	    (setq result (emt:dispatch:table-el-data entry))))))


;;;_   , Tests

(rtest:deftest emt:dispatch:get-match
   (  "Situation: Using the test-helper table.  There's no match.
Response: Returns nil."
      (let
	 ((obj
	     (emt:dispatch:get-match
		'dont-match-me
		emt:dispatch:thd:table)))
	 (null obj)))

   (  "Situation: Using the test-helper table.  Matches the first.
Response: The one it finds works."
      (let
	 ((obj
	     (emt:dispatch:get-match
		'one
		emt:dispatch:thd:table)))

	 (equal
	    (funcall
	       (emt:dispatch:thd:interface-vtable-list-args obj)
	       2 4 6)
	    '(2 4 6))))
   

   (  "Situation: Using the test-helper table.  Matches the first.
Response: Finds the right one."
      (let
	 ((obj
	     (emt:dispatch:get-match
		'one
		emt:dispatch:thd:table)))
	 (equal
	    (funcall
	       (emt:dispatch:thd:interface-vtable-name obj))
	    'module-one)))
   
   (  "Situation: Using the test-helper table.  Matches the second.
Response: Finds the right one."
      (let
	 ((obj
	     (emt:dispatch:get-match
		'two
		emt:dispatch:thd:table)))
	 
	 (equal
	    (funcall
	       (emt:dispatch:thd:interface-vtable-name obj))
	    'module-two))))


;;;_  . emt:dispatch:dispatch-func
(defun emt:dispatch:dispatch-func-x (vtable accessor &rest r)
   ""
   (apply (funcall accessor vtable) r))

(defun emt:dispatch:dispatch-func (obj table accessor &rest r)
   ""
   (let
      ((vtable
	  (emt:dispatch:get-match obj table)))
      (if vtable
	 (apply #'emt:dispatch:dispatch-func-x 
	    vtable accessor r)
	 (error "Vtable not found"))))

;;;_   , Tests

(rtest:deftest emt:dispatch:dispatch-func

   (  "Action: Dispatch on `one' which matches the first object.  Tell
it to use the `list-args' field.
Response: Function call works."
      (equal
	 (emt:dispatch:dispatch-func 
	    'one 
	    emt:dispatch:thd:table
	    #'emt:dispatch:thd:interface-vtable-list-args
	    2 4 6)
	 '(2 4 6)))
   
   )

;;;_  . emt:dispatch:make-dispatcher-form-maker

;;Careful of the backquotes in this section.  elisp has a problem with
;;nested backquotes, so these need to be separate

(defun emt:dispatch:make-dispatcher-form-maker-x (obj table accessor-form rest-form)
   "Make the body part of a form.
This is the compile-interface time function"
   `
   (apply #'emt:dispatch:dispatch-func
      ,obj
      ,table
      #',accessor-form
      ,@rest-form))

;;Can't be a macro because it is called by funcall.
(defun emt:dispatch:make-dispatcher-form-maker (table-form)
   "Make a form-maker pertaining to TABLE-FORM.
TABLE-FORM must be a form that can be evalled while creating the
interface.  Generally it's just a symbol."
   `
   (lambda (accessor-form rest-form)
       ""
       (list
	  '(obj)
	  (emt:dispatch:make-dispatcher-form-maker-x
	     'obj 
	     ',table-form
	     accessor-form
	     rest-form))))

;;;_   , Tests of emt:dispatch:make-dispatcher-form-maker

(rtest:deftest emt:dispatch:make-dispatcher-form-maker

   (  "Situation: Vtable punts, defines no functions.  Dispatch table
is the empty list.
Response: Structure gets defined and can be made."
      (let
	 ;;Bind these locally so we don't see or disturb other bindings
	 (my-interface)

	 ;;Define an interface
	 (define-interface my-interface 
	    (emt:dispatch:make-dispatcher-form-maker ())
	    "Some docstring" )
	 (make-my-interface-vtable)
	 t))

   ;;`emt:dispatch:make-dispatcher-form-maker' is used earlier in
   ;;defining `emt:dispatch:thd:interface', so tests using the latter
   ;;test the former.

   (  "Situation: Using `emt:dispatch:thd:interface' which was defined
with `emt:dispatch:make-dispatcher-form-maker'
Action: Call the `list-args' function, which just returns what it got.
Response: It returns as expected."

      (equal
	 (emt:dispatch:thd:interface-list-args 'one 1 3 5)
	 '(1 3 5)))
   
   (  "Situation: Using `emt:dispatch:thd:interface' which was defined
with `emt:dispatch:make-dispatcher-form-maker'
Action: Call the `name' function, which tells the name.
Response: It returns as expected."

      (equal
	 (emt:dispatch:thd:interface-name 'one)
	 'module-one))
   
   )

;;;_  . Test data

(rtest:if-avail
   ;;Uses `emt:dispatch:make-dispatcher-form-maker' but that is only
   ;;tested later in this file.  Can't be helped.  Earlier tests don't
   ;;rely on it, but this definition relies on it not dying.
   (define-interface emt:dispatch:thd:interface
      (emt:dispatch:make-dispatcher-form-maker 
	 'emt:dispatch:thd:table)
      "An interface for test purposes.  It has these functions:

    name, which should just return the name
    list-args, which should just return what it got." 
      (name) 
      (list-args &rest r))


   (defconst emt:dispatch:thd:table
      (list
	 (make-emt:dispatch:table-el
	    :match-p 
	    #'(lambda (x)
		 (equal x 'one))
	    :data 
	    (make-emt:dispatch:thd:interface-vtable
	       :name 
	       #'(lambda ()
		    'module-one)
	       :list-args #'list))
	 (make-emt:dispatch:table-el
	    :match-p 
	    #'(lambda (x)
		 (equal x 'two))
	    :data 
	    (make-emt:dispatch:thd:interface-vtable
	       :name 
	       #'(lambda ()
		    'module-two)
	       :list-args #'list))
	 )
      "A mock dispatch table for testing" ))



;;;_. Footers
;;;_ , Provides

(provide 'define-interface)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; define-interface.el ends here
