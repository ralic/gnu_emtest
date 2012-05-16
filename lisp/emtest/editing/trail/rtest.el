;;;_ emtest/editing/trail/rtest.el --- Rtest tests for trail

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
(require 'emtest/editing/trail)

;;;_. Body

;;;_   , emt:ed:trl:build-code
(rtest:deftest emt:ed:trl:build-code

   (  "Situation: Consists of a single direct should-item.
Simple: The arg to that is always t - can't fail.
Operation: Build form
Operation: Eval the form.
When that var is non-nil, form has no error.
When that var is nil, form has error."
      (emt:ed:trl:build-code:thm2
	 (:inputs
	    '((t . t))
	    :bindings
	    () ;;No bindings
	    :precedence-pairs
	    '()
	    :sym
	    code)
	 
	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    () t code)
	    

	 t))

   (  "Situation: Consists of a single direct should-item.
Simple: The arg to that is a variable from outside.
Operation: Build form
Operation: Eval the form.
Sub-situation: That var is non-nil.  
Sub-response: form passes (has no error)
Sub-situation: that var is nil.
Sub-response: form fails."
      (emt:ed:trl:build-code:thm2
	 (:inputs
	    ;;Inputs are (SHOULD . EXPRESSION)
	    '(  (t . my-var))
	    :bindings
	    ((my-var t))
	    :precedence-pairs
	    '()
	    :sym
	    code)
	 
	    
	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((my-var t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((my-var nil)) nil code)	    

	 t))
   

   (  "Situation: Should is an `equal' fed from two items, which are
simple variables.
Sub-situation: Those variables are equal, and same as the originals.
Sub-response: Form passes.
Sub-situation: Those variables are equal, and different than the originals.
Sub-response: Form passes.
Sub-situation: Those variables are not equal.
Sub-response: Form fails."
      (emt:ed:trl:build-code:thm2
	 (:inputs
	    ;;Inputs are (SHOULD . EXPRESSION)
	    '((t . (equal var1 var2)))
	    :bindings
	    ((var1 t)
	       (var2 t))
	    :precedence-pairs
	    '()
	    :sym
	    code)
	 
	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))
   
   (  "Shows: Works in the presence of previous items
Situation: An `equal' should and two dummy elements.
Response: The sub-situations behave as expected."
      (emt:ed:trl:build-code:thm2
	 (:inputs
	    '(  (nil . 100)
		(nil . 123)
		(t . (equal var1 var2)))
	    :bindings
	    ((var1 t)
	       (var2 t))
	    :precedence-pairs
	    '()
	    :sym
	    code)

	 
	    
	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))


   (  "Shows: Use of previous trail items.
Situation: An `equal' fed from two items, which directly are variables
Response: The sub-situations behave as expected."
      (emt:ed:trl:build-code:thm2
	 (:inputs
	    '(  (nil . var1)
		(nil . var2)
		(t . (equal (emt:ed:trl:value 0)(emt:ed:trl:value 1))))
	    :bindings
	    ((var1 t)
	       (var2 t))
	    :precedence-pairs
	    '((2 1 emt:ed:trl:value)
		(2 0 emt:ed:trl:value))
	    :sym
	    code)
	 
	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)
	  
	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))

   (  "Shows: Can use chains of input: 
Situation: An `equal' fed from two items, which directly are variables
Response: The sub-situations behave as expected."
      (emt:ed:trl:build-code:thm2
	 (:inputs
	    ;;Inputs are (SHOULD . EXPRESSION)
	    '(  (nil . var1)
		(nil . (emt:ed:trl:value 0))
		(nil . var2)
		(t . (equal (emt:ed:trl:value 1)(emt:ed:trl:value 2))))
	    :bindings
	    ((var1 t)
	       (var2 t))
	    :precedence-pairs
	    '((3 1 emt:ed:trl:value)
		(3 2 emt:ed:trl:value)
		(1 0 emt:ed:trl:value))
	    :sym
	    code)
	 
	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))

   (  "Shows: Multiple uses of the same item don't cause problems. 
Situation: An `equal' fed from twice each from two items, which
directly are variables.
Response: The sub-situations behave as expected."
      (emt:ed:trl:build-code:thm2
	 (:inputs
	    ;;Inputs are (SHOULD . EXPRESSION)
	    '(  (nil . var1)
		(nil . var2)
		(t . (equal 
			(list
			   (emt:ed:trl:value 0)
			   (emt:ed:trl:value 0))
			(list
			   (emt:ed:trl:value 1)
			   (emt:ed:trl:value 1)))))
	    :bindings
	    ((var1 t)
	       (var2 t))
	    :precedence-pairs
	    ;;Would rather compare them with duplicates removed.
	    '((2 1 emt:ed:trl:value)
		(2 1 emt:ed:trl:value)
		(2 0 emt:ed:trl:value)
		(2 0 emt:ed:trl:value))
	    :sym
	    code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt:ed:trl:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))


   ;;Shows: values are usable as well.
   ;;An `equal' is fed from an expression and a value.  The expression
   ;;is just a variable.  Bind the variable to the same as the value,
   ;;it passes.  Bind to different, it fails.

   ;;Shows: It supports multiple shoulds.


   ;;Indirect should items, for equal-to-value, equal sets, and others.
   
   )


;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/trail/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/trail/rtest.el ends here
