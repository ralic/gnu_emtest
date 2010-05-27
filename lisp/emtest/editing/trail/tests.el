;;;_ emtest/editing/trail/tests.el --- Rtest tests for trail

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

;;;_   , emter:build-code
(emt:deftest-3 emter:build-code
   (nil
      (progn
	 (emt:doc "Situation: Consists of a single direct should-item.
Simplified: The arg to that is always t - can't fail.")
	 (emt:doc "Operation: Build form")
	 (emt:doc "Operation: Eval the form.
When that var is non-nil, form has no error.
When that var is nil, form has error.")
	 (emter:build-code:thm2
	    (:inputs
	       '((t . t))
	       :bindings nil :precedence-pairs 'nil :sym code)
	    (emter:build-code:th:w-bindings-passes-p nil t code)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Consists of a single direct should-item.
Simplified: The arg to that is a variable from outside.")
	 (emt:doc "Operation: Build form")
	 (emt:doc "Operation: Eval the form.")
	 (emt:doc "Sub-situation: That var is non-nil.  ")
	 (emt:doc "Sub-response: form passes (has no error)")
	 (emt:doc "Sub-situation: that var is nil.")
	 (emt:doc "Sub-response: form fails.")
	 (emter:build-code:thm2
	    (:inputs
	       '((t . my-var))
	       :bindings
	       ((my-var t))
	       :precedence-pairs 'nil :sym code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((my-var t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((my-var nil))
	       nil code)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Should is an `equal' fed from two items, which are
simple variables.")
	 (emt:doc "Sub-situation: Those variables are equal, and same as the originals.")
	 (emt:doc "Sub-response: Form passes.")
	 (emt:doc "Sub-situation: Those variables are equal, and different than the originals.")
	 (emt:doc "Sub-response: Form passes.")
	 (emt:doc "Sub-situation: Those variables are not equal.")
	 (emt:doc "Sub-response: Form fails.")
	 (emter:build-code:thm2
	    (:inputs
	       '((t equal var1 var2))
	       :bindings
	       ((var1 t)
		  (var2 t))
	       :precedence-pairs 'nil :sym code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 13)
		   (var2 14))
	       nil code)
	    t)))
   (nil
      (progn
	 (emt:doc "Shows: Works in the presence of previous items")
	 (emt:doc "Situation: An `equal' should and two dummy elements.")
	 (emt:doc "Response: The sub-situations behave as expected.")
	 (emter:build-code:thm2
	    (:inputs
	       '((nil . 100)
		   (nil . 123)
		   (t equal var1 var2))
	       :bindings
	       ((var1 t)
		  (var2 t))
	       :precedence-pairs 'nil :sym code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 13)
		   (var2 14))
	       nil code)
	    t)))
   (nil
      (progn
	 (emt:doc "Shows: Use of previous trail items.")
	 (emt:doc "Situation: An `equal' fed from two items, which directly are variables")
	 (emt:doc "Response: The sub-situations behave as expected.")
	 (emter:build-code:thm2
	    (:inputs
	       '((nil . var1)
		   (nil . var2)
		   (t equal
		      (emter:value 0)
		      (emter:value 1)))
	       :bindings
	       ((var1 t)
		  (var2 t))
	       :precedence-pairs
	       '((2 1 emter:value)
		   (2 0 emter:value))
	       :sym code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 13)
		   (var2 14))
	       nil code)
	    t)))
   (nil
      (progn
	 (emt:doc "Shows: Can use chains of input:")
	 (emt:doc "Situation: An `equal' fed from two items, which directly are variables")
	 (emt:doc "Response: The sub-situations behave as expected.")
	 (emter:build-code:thm2
	    (:inputs
	       '((nil . var1)
		   (nil emter:value 0)
		   (nil . var2)
		   (t equal
		      (emter:value 1)
		      (emter:value 2)))
	       :bindings
	       ((var1 t)
		  (var2 t))
	       :precedence-pairs
	       '((3 1 emter:value)
		   (3 2 emter:value)
		   (1 0 emter:value))
	       :sym code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 13)
		   (var2 14))
	       nil code)
	    t)))
   (nil
      (progn
	 (emt:doc "Shows: Multiple uses of the same item don't cause problems.")
	 (emt:doc "Situation: An `equal' fed from twice each from two items, which
directly are variables.")
	 (emt:doc "Response: The sub-situations behave as expected.")
	 (emter:build-code:thm2
	    (:inputs
	       '((nil . var1)
		   (nil . var2)
		   (t equal
		      (list
			 (emter:value 0)
			 (emter:value 0))
		      (list
			 (emter:value 1)
			 (emter:value 1))))
	       :bindings
	       ((var1 t)
		  (var2 t))
	       :precedence-pairs
	       '((2 1 emter:value)
		   (2 1 emter:value)
		   (2 0 emter:value)
		   (2 0 emter:value))
	       :sym code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 t)
		   (var2 t))
	       t code)
	    (emter:build-code:th:w-bindings-passes-p
	       '((var1 13)
		   (var2 14))
	       nil code)
	    t))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/trail/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/trail/tests.el ends here
