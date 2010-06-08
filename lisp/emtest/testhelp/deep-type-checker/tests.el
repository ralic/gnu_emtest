;;;_ emtest/testhelp/deep-type-checker/tests.el --- Tests of deep-type-checker

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

;;;_. Body

;;;_   , emty:cons-f
(put 'emty:cons-f 'emt:test-thru 'cons)


;;;_   , Tests

(emt:deftest-3 cons
   (nil
      (progn
	 (emt:doc "Situation: Spec args are not given")
	 (emt:doc "Response: Accept just cons cells.")
	 (progn
	    (assert
	       (not
		  (typep "Not a list"
		     '(cons)))
	       t)
	    (assert
	       (typep
		  '(1 2)
		  '(cons))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Spec args are both `*'.")
	 (emt:doc "Response: Accept just cons cells.")
	 (progn
	    (assert
	       (not
		  (typep "Not a list"
		     '(cons * *)))
	       t)
	    (assert
	       (typep
		  '(1 2)
		  '(cons * *))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Spec args are given.")
	 (emt:doc "Response: Accept just cons cells whose parts match the specs.")
	 (progn
	    (assert
	       (not
		  (typep
		     '(13 . 13)
		     '(cons integer string)))
	       t)
	    (assert
	       (not
		  (typep
		     '("No" . "Nope")
		     '(cons integer string)))
	       t)
	    (assert
	       (typep
		  '(12 . "Yes")
		  '(cons integer string))
	       t)
	    t))))

;;;_   , emty:list-f
(put 'emty:list-f 'emt:test-thru 'list)

;;;_   , list
(emt:deftest-3 list
   (nil
      (progn
	 (emt:doc "Situation: Spec is empty")
	 (emt:doc "Response: Accept only nil.")
	 (progn
	    (assert
	       (not
		  (typep "Not a list"
		     '(list)))
	       t)
	    (assert
	       (not
		  (typep
		     '("A list")
		     '(list)))
	       t)
	    (assert
	       (typep 'nil
		  '(list))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Spec has one arg")
	 (emt:doc "Response: Accept only lists of one el that matches that type.")
	 (progn
	    (assert
	       (typep
		  '(12)
		  '(list integer))
	       t)
	    (assert
	       (not
		  (typep
		     '("Not an integer")
		     '(list integer)))
	       t)
	    (assert
	       (not
		  (typep 'nil
		     '(list integer)))
	       t)
	    (assert
	       (not
		  (typep
		     '(13 13)
		     '(list integer)))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Spec has three args.  One is wildcard `*'")
	 (emt:doc "Response: Accepts only lists of two els, where the respective one
matches each type.")
	 (progn
	    (assert
	       (typep
		  '(12 "string" 12)
		  '(list integer * integer))
	       t)
	    (assert
	       (typep
		  '(12 'symbol 12)
		  '(list integer * integer))
	       t)
	    (assert
	       (not
		  (typep
		     '(12 "string")
		     '(list integer * integer)))
	       t)
	    (assert
	       (not
		  (typep
		     '(12 "string" 12 12)
		     '(list integer * integer)))
	       t)
	    (assert
	       (not
		  (typep
		     '("A string" "string" 12)
		     '(list integer * integer)))
	       t)
	    (assert
	       (not
		  (typep
		     '(12 "string" "A string")
		     '(list integer * integer)))
	       t)
	    t))))

;;;_   , emty:repeat-f
(put 'emty:repeat-f 'emt:test-thru 'repeat)


;;;_   , repeat

(emt:deftest-3 repeat
   (nil
      (progn
	 (emt:doc "Situation: Spec is just any list.  Pass non-list")
	 (emt:doc "Response: Fail.")
	 (not
	    (typep "Not a list"
	       '(repeat)))))
   (nil
      (progn
	 (emt:doc "Situation: Spec is just any list.  Pass a list")
	 (emt:doc "Response: Succeed.")
	 (typep
	    '(12 "List of" "anything")
	    '(repeat))))
   (nil
      (progn
	 (emt:doc "Situation: Spec is a list of integer.  Pass non-list")
	 (emt:doc "Response: Fail.")
	 (not
	    (typep "Not a list"
	       '(repeat integer)))))
   (nil
      (progn
	 (emt:doc "Situation: Spec is a list of integer.  Pass a list of non-integers")
	 (emt:doc "Response: Fail.")
	 (not
	    (typep
	       '("Not an integer")
	       '(repeat integer)))))
   (nil
      (progn
	 (emt:doc "Situation: Spec is a list of integer.  Pass a list of integers")
	 (emt:doc "Response: Succeed.")
	 (typep
	    '(12 144)
	    '(repeat integer))))
   (nil
      (progn
	 (emt:doc "Situation: Spec is a list of integer.  Pass a mixed list of
integers and non-integers.")
	 (emt:doc "Response: Fail.")
	 (not
	    (typep
	       '(12 "Not an integer")
	       '(repeat integer))))))


;;;_   , emty:list*-f

(put 'emty:list*-f 'emt:test-thru 'list*)


;;;_   , list*
(emt:deftest-3 list*
   (nil
      (progn
	 (emt:doc "Situation: There's only the last element")
	 (emt:doc "Response: Last element must match the rest of the list.")
	 (progn
	    (assert
	       (typep
		  '(12 144)
		  '(list*
		      (repeat integer)))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: There's only the last element")
	 (emt:doc "Response: Last element must match the rest of the list.")
	 (progn
	    (assert
	       (typep
		  '("Alpha" 144)
		  '(list*
		      (list string integer)))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: There's only the last element")
	 (emt:doc "Response: Last element must match the rest of the list.")
	 (progn
	    (assert
	       (not
		  (typep
		     '(144 "Alpha")
		     '(list*
			 (list string integer))))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: There are previous elements")
	 (emt:doc "Response: The initial elements must match them.")
	 (progn
	    (assert
	       (typep
		  '(12 "Alpha" 144)
		  '(list* integer string
		      (repeat integer)))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: There are previous elements")
	 (emt:doc "Response: The initial elements must match them.")
	 (progn
	    (assert
	       (not
		  (typep
		     '(13 13 144)
		     '(list* integer string
			 (repeat integer))))
	       t)
	    t))))



;;;_  . emty:check
(put 'emty:check 'emt:test-thru 'cl-make-type-test)


;;;_  . emty:struct-slots-correct-p
(put 'emty:struct-slots-correct-p 'emt:test-thru 'cl-make-type-test)


;;;_  . cl-make-type-test
(emt:deftest-3 cl-make-type-test
   (nil
      (progn
	 (emt:doc "`nil' type is not mangled.")
	 (not
	    (typep 12 nil))))
   (nil
      (progn
	 (emt:doc "Built-in types work (integer).")
	 (progn
	    (assert
	       (typep 12 'integer)
	       t)
	    (assert
	       (not
		  (typep "String" 'integer))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Compound types work.")
	 (progn
	    (assert
	       (typep 12
		  '(or integer string))
	       t)
	    (assert
	       (typep "String"
		  '(or integer string))
	       t)
	    (assert
	       (not
		  (typep
		     '(repeat)
		     '(or integer string)))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Built-in types work (integer).")
	 (progn
	    (emty:check 12 integer)
	    (emth:gives-error
	       (emty:check "String" integer))
	    t)))
   (nil
      (progn
	 (emt:doc "Compound types work.")
	 (progn
	    (emty:check 12
	       (or integer string))
	    (emty:check "String"
	       (or integer string))
	    (emth:gives-error
	       (emty:check
		  '(a list)
		  (or integer string)))
	    t)))
   (nil
      (progn
	 (emt:doc "Sanity checks for `cl-make-type-test'.")
	 (let
	    ((emty:*use* t))
	   ;;$$IMPROVE ME Insulate better, sensitive to struct
	   ;;definitions.
	    (emth:let-noprops
	       '(struct1)
	       (defstruct struct1 field1)
	       (assert
		  (cl-make-type-test
		     (make-struct1 :field1 12)
		     'struct1)
		  t)
	       (assert
		  (eval
		     (cl-make-type-test
			(make-struct1 :field1 12)
			'struct1))
		  t)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: A normal struct definition, no slot type spec.")
	 (emt:doc "Response: Typep passes it no matter what's in the field.")
	 (emth:let-noprops
	    '(struct1)
	    (defstruct struct1 field1)
	    (emty:check
	       (make-struct1 :field1
		  '("Any" thing "at" all))
	       struct1)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: A struct definition with a slot type spec.
`typep' is used and the flag `emty:*use*' has its global
value of `nil.'")
	 (emt:doc "Response: Typep passes it no matter what's in the field.")
	 (emth:let-noprops
	    '(struct1)
	    (defstruct struct1
	       (field1 nil :type integer))
	    (assert
	       (not emty:*use*))
	    (assert
	       (typep
		  (make-struct1 :field1
		     '("Any" thing "at" all))
		  'struct1)
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: A struct definition with a slot type spec.
`typep' is used, and the flag `emty:*use*' is let non-nil.")
	 (emt:doc "Response: Typep passes it no matter what's in the field.")
	 (let
	    ((emty:*use* t))
	    (emth:let-noprops
	       '(struct1)
	       (defstruct struct1
		  (field1 nil :type integer))
	       (assert
		  (not
		     (typep
			(make-struct1 :field1
			   '("Any" thing "at" all))
			'struct1))
		  t)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: A struct definition with a simple slot type spec.")
	 (emth:let-noprops
	    '(struct1)
	    (defstruct struct1
	       (field1 nil :type integer))
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1
			'("Some" thing else))
		     struct1)))
	    (emty:check
	       (make-struct1 :field1 12)
	       struct1)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: A struct definition with a simple slot type spec.
The name spec is not a bare symbol")
	 (emth:let-noprops
	    '(struct1)
	    (defstruct
	       (struct1
		  (:type vector)
		  :named)
	       (field1 nil :type integer))
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1
			'("Some" thing else))
		     struct1)))
	    (emty:check
	       (make-struct1 :field1 12)
	       struct1)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: A struct definition with several slot type specs.")
	 (emth:let-noprops
	    '(struct1)
	    (defstruct struct1
	       (field1 nil :type integer)
	       (field2 nil :type string))
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1
			'(something wrong)
			:field2 "Correct")
		     struct1)))
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1 12 :field1
			'(something wrong))
		     struct1)))
	    (emty:check
	       (make-struct1 :field1 12 :field2 "Correct")
	       struct1)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Slot type spec is to another struct.")
	 (emth:let-noprops
	    '(struct0 struct1)
	    (defstruct struct0 field0)
	    (defstruct struct1
	       (field1 nil :type struct0))
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1
			'("Some" thing else))
		     struct1)))
	    (emty:check
	       (make-struct1 :field1
		  (make-struct0 :field0 "Anything"))
	       struct1)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Slot type spec is a list of a built-in type
Object has the right type for that slot.")
	 (emt:doc "Response: emty:check accepts it.")
	 (emth:let-noprops
	    '(struct1)
	    (defstruct struct1
	       (field1 nil :type
		  (repeat integer)))
	    (emty:check
	       (make-struct1 :field1 'nil)
	       struct1)
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1
			'("Some" thing else))
		     struct1)))
	    (emty:check
	       (make-struct1 :field1
		  '(12))
	       struct1)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Slot type spec is to a list of another struct.")
	 (emth:let-noprops
	    '(struct0 struct1)
	    (defstruct struct0 field0)
	    (defstruct struct1
	       (field1 nil :type
		  (repeat struct0)))
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1 13)
		     struct1)))
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1
			'(("Some" thing else)))
		     struct1)))
	    (emty:check
	       (make-struct1 :field1
		  (list
		     (make-struct0 :field0 "Anything")))
	       struct1)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Slot type spec is a list of its own type.
Object has the right type for that slot.")
	 (emt:doc "Response: emty:check accepts it.")
	 (emth:let-noprops
	    '(struct1)
	    (defstruct struct1
	       (field1 nil :type
		  (repeat struct1)))
	    (assert
	       (emth:gives-error
		  (emty:check
		     (make-struct1 :field1
			'("Any" thing "at" all))
		     struct1)))
	    (emty:check
	       (make-struct1 :field1
		  (list
		     (make-struct1 :field1 nil)))
	       struct1)
	    (emty:check
	       (make-struct1 :field1
		  (list
		     (make-struct1 :field1
			(list
			   (make-struct1 :field1 nil)))))
	       struct1)
	    t)))
   (nil
      (progn
	 (emt:doc "Proves: Adds a result of the appropriate type to
`emt:trace:stored-diag'.")
	 (let
	    (emt:trace:stored-diag)
	    (emth:let-noprops
	       '(struct1)
	       (defstruct struct1
		  (field1 nil :type integer))
	       (emty:check
		  (make-struct1 :field1 12)
		  struct1)
	       (emt:trace:stored-diag:check-type)
	       t)))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/deep-type-checker/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/deep-type-checker/tests.el ends here
