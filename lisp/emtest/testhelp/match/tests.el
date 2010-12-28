;;;_ emtest/testhelp/match/tests.el --- Tests for match

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


;;;_ , Requires

(require 'emtest/runner/define)
(require 'emtest/runner/keepup)
(require 'emtest/testhelp/match)
(require 'emtest/testhelp/match/testhelp)
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/misc)

;;;_. Body

;;;_ , emtm:govs:list
;;
(emt:deftest-3 emtm:govs:list
   (nil
      (progn
	 (emt:doc "Situation: Object is not a list
Two items in the pattern list.
One item in the list object.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:list
		  '(list 12 144)
		  '13)))))
   (nil
      (progn
	 (emt:doc "Situation: List object is shorter:
Two items in the pattern list.
One item in the list object.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:list
		  '(list 12 144)
		  '(12))))))
   (nil
      (progn
	 (emt:doc "Situation:  List object is longer:
Two items in the pattern list.
Three items in the list object.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:list
		  '(list 12 144)
		  '(12 144 1728))))))
   (nil
      (progn
	 (emt:doc "Situation: Zero items in the list.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Match.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:list
	       '(list)
	       'nil))))
   (nil
      (progn
	 (emt:doc "Situation: One item in the list.
It matches.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Match.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:list
	       '(list 12)
	       '(12)))))
   (nil
      (progn
	 (emt:doc "Situation: One item in the list.
It mismatches.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:list
		  '(list 12)
		  '(13))))))
   (nil
      (progn
	 (emt:doc "Situation: Two items in the list.
They both match.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Match.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:list
	       '(list 12 144)
	       '(12 144)))))
   (nil
      (progn
	 (emt:doc "Situation: Two items in the list.
First one mismatches.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:list
		  '(list 12 144)
		  '(13 144))))))
   (nil
      (progn
	 (emt:doc "Situation: Two items in the list.
Second one mismatches.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:list
		  '(list 12 144)
		  '(12 133))))))
   (nil
      (progn
	 (emt:doc "Situation: A list item is itself a pattern.")
	 (emt:doc "Response: Matches in the expected way.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:list
	       '(list x 144)
	       '(12 144)))))
   (nil
      (progn
	 (emt:doc "Situation: Two list items are the same symbol.")
	 (emt:doc "Response: Matches in the expected way.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:list
	       '(list x x)
	       '(12 12)))))
   (nil
      (progn
	 (emt:doc "Situation: Two list items are the same symbol.")
	 (emt:doc "Response: Matches in the expected way.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:list
		  '(list x x)
		  '(13 169))))))
   (nil
      (progn
	 (emt:doc "Situation: A list item is itself a list pattern.")
	 (emt:doc "Response: Matches in the expected way.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:list
	       '(list
		   (list x x))
	       '((12 12))))))
   (nil
      (progn
	 (emt:doc "Situation: A list item is itself a list pattern.")
	 (emt:doc "Response: Matches in the expected way.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:list
		  '(list
		      (list x x))
		  '((13 169))))))))


;;;_ , emtm:govs:set
'  ;;$$DORMANT
(emt:deftest-3 emtm:govs:set
   (nil
      (progn
	 (emt:doc "Situation: Object is not a list
Two items in the pattern list.
One item in the list object.
Forcing emtm:govs:set.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:set
		  '(set 12 144)
		  '13)))))
   (nil
      (progn
	 (emt:doc "Situation: List object is shorter:
Two items in the pattern list.
One item in the list object.
Forcing emtm:govs:set.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:set
		  '(set 12 144)
		  '(12))))))
   (nil
      (progn
	 (emt:doc "Situation:  List object is longer:
Two items in the pattern list.
Three items in the list object.
Forcing emtm:govs:set.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:set
		  '(set 12 144)
		  '(12 144 1728))))))
   (nil
      (progn
	 (emt:doc "Situation: Zero items in the list.
Forcing emtm:govs:list.")
	 (emt:doc "Response: Match.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:set
	       '(set)
	       'nil))))
   (nil
      (progn
	 (emt:doc "Situation: One item in the list.
It matches.
Forcing emtm:govs:set.")
	 (emt:doc "Response: Match.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:set
	       '(set 12)
	       '(12)))))
   (nil
      (progn
	 (emt:doc "Situation: One item in the list.
It mismatches.
Forcing emtm:govs:set.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:set
		  '(set 12)
		  '(13))))))
   (nil
      (progn
	 (emt:doc "Situation: Two items in the list.
They both match.
Forcing emtm:govs:set.")
	 (emt:doc "Response: Match.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:set
	       '(set 12 144)
	       '(12 144)))))
   (nil
      (progn
	 (emt:doc "Situation: Two items in the list.
First one mismatches.
Forcing emtm:govs:set.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:set
		  '(set 12 144)
		  '(13 144))))))
   (nil
      (progn
	 (emt:doc "Situation: Two items in the list.
Second one mismatches.
Forcing emtm:govs:set.")
	 (emt:doc "Response: Mismatch.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:set
		  '(set 12 144)
		  '(12 133))))))
   (nil
      (progn
	 (emt:doc "Situation: Two items.  They match in other order")
	 (emt:doc "Response: Match.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:set
	       '(set 12 144)
	       '(144 12)))))
   (nil
      (progn
	 (emt:doc "Situation: Two items.  One pattern element matches both, the
other matches neither.")
	 (emt:doc "Response: Error (Not just mismatch).")
	 (emt:assert
	    (emth:gives-error
	       (emtm:ts:single-gov #'emtm:govs:set
		  '(set 12 144)
		  '(12 12)))))))


;;;_ , emtm:govs:satisfies
(emt:deftest-3 emtm:govs:satisfies
   (nil
      (progn
	 (emt:doc "Proves: Whether it succeeds is whether pred succeeds.
Param: Pred `list'")
	 (emt:doc "Response: Satisfies it (because list with 1 arg always gives non-nil).")
	 (emt:assert
	    (and
	       (emtm 0
		  (satisfies #'list
		     (a)))))))
   (nil
      (progn
	 (emt:doc "Proves: Whether it succeeds is whether pred succeeds.
Param: Pred `ignore'")
	 (emt:doc "Response: Fails (because `ignore' always gives nil).")
	 (emt:assert
	    (not
	       (emtm 0
		  (satisfies #'ignore))))))
   (nil
      (progn
	 (emt:doc "Proves: Uses the ret-patterns.
Object doesn't match the sub-patterns.")
	 (emt:doc "Response: Returns nil.")
	 (emt:assert
	    (not
	       (emtm 13
		  (satisfies #'list
		     (12)))))))
   (nil
      (progn
	 (emt:doc "Proves: Uses the ret-patterns.
Object matches the sub-patterns.")
	 (emt:doc "Response: Returns non-nil.")
	 (emt:assert
	    (and
	       (emtm 12
		  (satisfies #'list
		     (12)))))))
   (nil
      (progn
	 (emt:doc "Proves: Makes bindings in the ret-patterns available to the
rest of the pattern.
Bindings in the sub-patterns are used.")
	 (emt:doc "Behavior: Does not error.  The correct values are seen.")
	 (emt:assert
	    (and
	       (equal
		  (emtm-let 12
		     (satisfies #'list
			(a))
		     a)
		  12)))))
   (nil
      (progn
	 (emt:doc "Proves: The predicate sees the other arguments.
Param: Pred `list'.")
	 (emt:doc "Response: Produces the expected result.")
	 (emt:assert
	    (and
	       (equal
		  (emtm-let 'nil
		     (satisfies #'list
			(obj a b)
			12 144)
		     (list a b))
		  '(12 144))))))
   (nil
      (progn
	 (emt:doc "Proves: The extra argument are forms, not values.
Param: Pred `list'")
	 (emt:doc "Response: Produces the expected result.")
	 (emt:assert
	    (and
	       (equal
		  (emtm-let 'nil
		     (satisfies #'list
			(obj a b)
			(+ 6 6)
			(* 12 12))
		     (list a b))
		  '(12 144))))))
   (nil
      (progn
	 (emt:doc "Situation: The pred succeeds and returns a non-list.  Specifically, 12.
We make no bindings.")
	 (emt:doc "Response: Succeeds.")
	 (emt:assert
	    (and
	       (emtm 12
		  (satisfies #'identity)))))))


;;;_ , emtm:govs:eval
(emt:deftest-3 emtm:govs:eval
   (nil
      (progn
	 (emt:doc "Situation: Arg, when evalled, matches the object.")
	 (emt:doc "Response: Succeed.")
	 (emt:assert
	    (and
	       (emtm 12
		  (eval
		     '(+ 6 6)))))))
   (nil
      (progn
	 (emt:doc "Situation: Arg, when evalled, does not match the object.")
	 (emt:doc "Response: Fail.")
	 (emt:assert
	    (not
	       (emtm 13
		  (eval
		     '(+ 6 6)))))))
   (nil
      (progn
	 (emt:doc "Situation: Arg, unevalled, matches the object, but evalled it
does not.")
	 (emt:doc "Response: Fail.")
	 (emt:assert
	    (not
	       (emtm
		  '(+ 6 6)
		  (eval
		     '(+ 6 6)))))))
   (nil
      (progn
	 (emt:doc "Param: Arg has a non-constant object.")
	 (emt:doc "Response: Error.")
	 (emt:assert
	    (emth:gives-error
	       (let
		  ((a 0))
		  (emtm 0
		     (eval a)))))))
   (nil
      (progn
	 (emt:doc "Situation: Arg, when evalled, matches the object.
Object is a form, so it can be further evalled.")
	 (emt:doc "Response: Succeed.")
	 (emt:assert
	    (and
	       (emtm
		  '(+ 6 6)
		  (eval
		     ''(+ 6 6)))))))
   (nil
      (progn
	 (emt:doc "Proves: Eval occurs when pattern is compiled, not at match time.")
	 (emt:doc "Situation: At pattern-compile time, `a' is bound to 12.
At match time, `a' is bound to 13.
Object is 12.")
	 (emt:doc "Response: Passes.
Object is 13.")
	 (emt:doc "Response: Fails")
	 (emt:assert
	    (let
	       ((comparand-f
		   (let
		      ((a 12))
		      (emtm:lambda
			 (eval 'a)
			 t))))
	       (let
		  ((a 13))
		  (and
		     (emtm 12
			(satisfies comparand-f))
		     (not
			(emtm 13
			   (satisfies comparand-f))))))))))


;;;_   , emtm:sort-bindings

(put 'emtm:sort-bindings 'emt:test-thru 'emtm:build-form)

;;$$WRITE MY TEST: Error if the same form or binding occurs more than once.

;;;_ , emtm:build-form
(emt:deftest-3 emtm:build-form
   (nil
      (progn
	 (emt:doc "Param: formdata list is an empty list.")
	 (emt:doc "Response: Form acts as just core.")
	 (emt:assert
	    (equal
	       (eval
		  (emtm:build-form
		     '(x)
		     (emtm:make-formdata :form-LIST nil)
		     12))
	       12))))
   (nil
      (progn
	 (emt:doc "Situation: formdata list gives dependencies out of order.
But they are not intrinsically circular, just out of order.
The bindings are given in an inconsistent order, so we're not just
accidentally processing them in the right order.
The bindings are required and used, so we're not just testing
simplification of redundant bindings.")
	 (emt:doc "Response: Still works OK.")
	 (emt:assert
	    (equal
	       (let
		  ((x 12))
		  (eval
		     (emtm:build-form
			'(x)
			(emtm:make-formdata :form-LIST
			   (list
			      (emtm:make-binding-form-data :bind 'z :uses
				 '(y)
				 :form 'y)
			      (emtm:make-binding-form-data :bind 'y :uses
				 '(x)
				 :form 'x)
			      (emtm:make-binding-form-data :bind 'w :uses
				 '(z)
				 :form 'z)))
			'(list w x y z))))
	       (list 12 12 12 12)))))
   (nil
      (progn
	 (emt:doc "Situation: Same as the bindings, but now with tests.

formdata list gives dependencies out of order.
But they are not intrinsically circular, just out of order.
The bindings are given in an inconsistent order, so we're not just
accidentally processing them in the right order.
The bindings are required and used, so we're not just testing
simplification of redundant bindings.")
	 (emt:doc "Response: Still works OK.")
	 (emt:assert
	    (equal
	       (let
		  ((x 12)
		     (form-y 1)
		     (form-z 2)
		     (form-w 3))
		  (eval
		     (emtm:build-form
			'(x)
			(emtm:make-formdata :form-LIST
			   (list
			      (emtm:make-test-form-data :uses
				 (list form-y)
				 :form form-z)
			      (emtm:make-test-form-data :uses
				 '(x)
				 :form form-y)
			      (emtm:make-test-form-data :uses
				 (list form-z)
				 :form form-w)))
			''ok)))
	       'ok)))))


;;;_ , emtm:build-form--1
(put 'emtm:build-form--1 'emt:test-thru 'emtm)


;;;_ , emtm:ts:single-gov

;;This is to validate that its results agree with known results, so
;;it's OK to use this for testing governor functions in isolation.
(emt:deftest-3 emtm:ts:single-gov
   (nil
      (progn
	 (emt:doc "Situation: Function is `emtm:govs:literal'
Object matches the literal pattern.")
	 (emt:doc "Response: Gives non-nil.")
	 (emt:assert
	    (and
	       (emtm:ts:single-gov #'emtm:govs:literal 12 12)))))
   (nil
      (progn
	 (emt:doc "Situation: Function is `emtm:govs:literal'
Object mismatches the literal pattern.")
	 (emt:doc "Response: Gives nil.")
	 (emt:assert
	    (not
	       (emtm:ts:single-gov #'emtm:govs:literal 12 13)))))
   (nil
      (progn
	 (emt:doc "Situation: Function is `emtm:govs:symbol'.
Object of course matches the symbol pattern.")
	 (emt:doc "Response: gives non-nil.")
	 (emt:assert
	    (emtm:ts:single-gov #'emtm:govs:symbol 'a 12)))))

;;;_ , emtm
(emt:deftest-3 emtm
   (nil
      (progn
	 (emt:doc "Situation: A literal pattern matches.")
	 (emt:doc "Response: Gives non-nil.")
	 (emt:assert
	    (emtm 12 12))))
   (nil
      (progn
	 (emt:doc "Situation: A literal pattern matches.
That pattern is a quoted list.")
	 (emt:doc "Response: Gives non-nil.")
	 (emt:assert
	    (and
	       (emtm
		  '(12)
		  '(12))))))
   (nil
      (progn
	 (emt:doc "Situation: A literal pattern mismatches.")
	 (emt:doc "Response: Gives nil.")
	 (emt:assert
	    (not
	       (emtm 13 12)))))
   (nil
      (progn
	 (emt:doc "Situation: A symbol")
	 (emt:doc "Response: Gives non-nil.")
	 (emt:assert
	    (and
	       (emtm 12 a)))))
   (nil
      (progn
	 (emt:doc "Situation: Pattern is a governed list")
	 (emt:doc "Response: Works.")
	 (emt:assert
	    (and
	       (emtm
		  '(12)
		  (list 12))))))
   (nil
      (progn
	 (emt:doc "Situation: Pattern is a governed list")
	 (emt:doc "Response: Works.")
	 (emt:assert
	    (not
	       (emtm
		  '(13)
		  (list 12))))))
   '(nil
       (progn
	  (emt:doc "Situation: Governed by `is-a'.")
	  (emt:doc "Response: Gives non-nil.")
	  (emt:assert
	     (and
		(emtm 12
		   (is-a 'integer))))))
   '(nil
       (progn
	  (emt:doc "Situation: Governed by `equal'.  
Both pattern arguments match the object.")
	  (emt:doc "Response: Gives non-nil.")
	  (emt:assert
	     (and
		(emtm 12
		   (equal a 12))))))
   '(nil
       (progn
	  (emt:doc "Situation: Governed by `equal'.  
Only one of the pattern arguments matches the object.")
	  (emt:doc "Response: Gives non-nil.")
	  (emt:assert
	     (and
		(emtm 12
		   (equal a 13))))))
   (nil
      (progn
	 (emt:doc "Situation: Pattern has the same variable occuring
   twice, bound to equal objects")
	 (emt:doc "Response: Matches.")
	 (emt:assert
	    (emtm
	       '(12 12)
	       (list a a)))))
   (nil
      (progn
	 (emt:doc "Situation: Pattern has the same variable occuring
   twice, bound to equal objects")
	 (emt:doc "Response: Matches.")
	 (emt:assert
	    (emtm
	       '(144 144)
	       (list a a)))))
   (nil
      (progn
	 (emt:doc "Situation: Pattern has the same variable occuring
   twice, but bound to different things.")
	 (emt:doc "Response: Mismatches.")
	 (emt:assert
	    (not
	       (emtm
		  '(12 13)
		  (list a a))))))

   (nil
      (let
	 ((pat-f
	     (emtm:make-general-lambda 
		;;Pattern
		(list a)
		;;Bindings
		(a)
		;;Body
		t)))
	 
	 (emt:doc 
	    "Situation: Pattern is made by a general lambda, 1 extra arg.
Then it's instantiated, with the extra binding being an object.")
	 (emt:doc "Behavior: Succeeds if `a' is equal to that object.")

	 (emt:assert
	    (funcall pat-f '(12) 12))
	 (emt:assert
	    (funcall pat-f '(144) 144))
	 (emt:assert
	    (not (funcall pat-f '(13) 12)))))
   (nil
      (let* 
	 ((pat-f
	     (emtm:make-general-lambda 
		;;Pattern
		(list a)
		;;Bindings
		(a)
		;;Body
		t))
	    
	    ;;Boxed pattern 1
	    (boxed-pat-1 (emtm:make-pattern 12))
	    ;;Boxed pattern 2
	    (boxed-pat-2 (emtm:make-pattern 144)))
	 (emt:doc 
	    "Situation: Pattern is made by a general lambda, 1 extra arg.
Then it's instantiated, with the extra binding being an object.")
	 (emt:doc 
	    "Situation: Two boxed patterns are made based on that,
	    with different args.")

	 (emt:assert
	    (funcall pat-f '(12) boxed-pat-1))
	 (emt:assert
	    (funcall pat-f '(144) boxed-pat-2))
	 (emt:assert
	    (not (funcall pat-f '(13) boxed-pat-1)))))
   
   )

;;;_ , emtm:lambda-binds
(emt:deftest-3 emtm:lambda-binds
   (nil
      (progn
	 (emt:doc "Situation: No bindings")
	 (emt:doc "Behavior: Returns the expected success.")
	 (let
	    ((func
		(emtm:lambda-binds 12)))
	    (emt:assert
	       (and
		  (funcall func 12)
		  (not
		     (funcall func 13)))))))
   (nil
      (progn
	 (emt:doc "Situation: Some bindings.
Specifically, one binding.")
	 (emt:doc "Behavior: Returns the expected success.
When successful, returns the expected list of values.")
	 (let
	    ((func
		(emtm:lambda-binds
		   (list 12 a)
		   a)))
	    (emt:assert
	       (and
		  (funcall func
		     '(12 144))
		  (equal
		     (funcall func
			'(12 144))
		     '(144))
		  (not
		     (funcall func
			'(13 13)))))))))


;;;_ , emtm-let
(emt:deftest-3 emtm-let
   (nil
      (progn
	 (emt:doc "Proves: The body's return value is returned.")
	 (emt:doc "Situation: Trivial PATTERN matches OBJECT-FORM")
	 (emt:doc "Response: The body's return value is returned.")
	 (emt:assert
	    (equal
	       (emtm-let 0 0 12)
	       12))))
   (nil
      (progn
	 (emt:doc "Proves: The bindings are available in BODY.")
	 (emt:doc "Situation: Trivial pattern matches object.")
	 (emt:doc "Response: The body's return value is returned.")
	 (emt:assert
	    (equal
	       (emtm-let
		  '(12 144)
		  (list a b)
		  (list b a))
	       (list 144 12))))))

;;;_ , emtm:pattern
(emt:deftest-3 emtm:pattern
   (nil
      (progn
	 (emt:doc "Proves: We can test for type.")
	 (emt:doc "Response: typep's pass/fail is as expected.")
	 (let
	    ((pat
		(emtm:make-pattern 12)))
	    (emt:assert
	       (typep pat
		  '(emtm:pattern *)))
	    (emt:assert
	       (not
		  (typep 13
		     '(emtm:pattern *))))
	    t))))

;;;_ , emtm:define-struct-governor*
(put 'emtm:define-struct-governor* 'emt:test-thru 'emths:struct-AxB:gov-high-level)

;;;_ , emtm:define-struct-governor
;;;_  . Test strategy
;;Test this on the usual test structure type `emths:struct-AxB'.  In
;;rtest-tools.

;;We want to use the same tests (and more) on the general function
;;that worked for the particular one.

;;Could rebuild them with the same data.  But without emtest yet, then
;;it's tough to see what parts failed.  For now, I am just
;;rebuilding the function both ways, to test it.


;;;_  . Test data
;;$$MOVE ME  Move data to new file testhelp/structs, prefix emths:
;;For now, no examples are defined, we just repeat all the data for
;;every test.

;;The structure definitions, and some aliases to be conformant.
(defstruct (emths:struct-AxB
	      (:constructor emths:item-struct-AxB)
	      (:conc-name emths:struct-AxB->))
   "An example structure with two untyped fields"
   field-A
   field-B)


;;;_   , Setup
(emt:keep-up-to-date 
   (emtm:define-struct-governor
      emtm:make-struct-governor 
      emtm:time2:make-struct-governor)
   (emtm:define-struct-governor
      (emths:struct-AxB
	 (:constructor emths:struct-AxB-high-gov)
	 (:predicate emths:struct-AxB-p)
	 (:conc-name emths:struct-AxB->))
    
      field-A 
      field-B))

;;;_  . Tests

(put 'emtm:make-struct-governor 'emt:test-thru 'emths:struct-AxB:gov-high-level)
(put 'emtm:define-struct-governor 'emt:test-thru 'emths:struct-AxB:gov-high-level)


(emt:deftest-3 emths:struct-AxB:gov-high-level
   (nil
      (progn
	 (emt:doc "Situation: Wrong type of object is given.")
	 (emt:doc "Response: Match fails.")
	 (emt:assert
	    (not
	       (emtm
		  (list 13)
		  (emths:struct-AxB-high-gov :field-A 12))))))
   (nil
      (progn
	 (emt:doc "Situation: Field-A field is given, matches.")
	 (emt:doc "Response: Match succeeds.")
	 (emt:assert
	    (emtm
	       (emths:item-struct-AxB :field-A 12)
	       (emths:struct-AxB-high-gov :field-A 12)))))
   (nil
      (progn
	 (emt:doc "Situation: Field-A field is given, mismatches.")
	 (emt:doc "Response: Match fails.")
	 (emt:assert
	    (not
	       (emtm
		  (emths:item-struct-AxB :field-A 13)
		  (emths:struct-AxB-high-gov :field-A 12))))))
   (nil
      (progn
	 (emt:doc "Situation: Field-A field is not given.")
	 (emt:doc "Response: Match succeeds.")
	 (emt:assert
	    (emtm
	       (emths:item-struct-AxB :field-A 12)
	       (emths:struct-AxB-high-gov)))))
   (nil
      (progn
	 (emt:doc "Situation: lambda-ized and used as lambda arg to `satisfies'.
Field-A field is given, matches.")
	 (emt:doc "Response: Match succeeds.")
	 (let
	    ((comparand-f
		(emtm:lambda
		   (emths:struct-AxB-high-gov :field-A 12)
		   t)))
	    (emt:assert
	       (emtm
		  (emths:item-struct-AxB :field-A 12)
		  (satisfies comparand-f))))))
   (nil
      (progn
	 (emt:doc "Situation: lambda-ized and used as lambda arg to `satisfies'.
Field-A field is given, mismatches.")
	 (emt:doc "Response: Match fails.")
	 (emt:assert
	    (not
	       (let
		  ((comparand-f
		      (emtm:lambda
			 (emths:struct-AxB-high-gov :field-A 12)
			 t)))
		  (emtm
		     (emths:item-struct-AxB :field-A 13)
		     (satisfies comparand-f)))))))
   (nil
      (progn
	 (emt:doc "Situation: lambda-ized and used as lambda arg to `satisfies'.
Field-A field is not given.")
	 (emt:doc "Response: Match succeeds.")
	 (let
	    ((comparand-f
		(emtm:lambda
		   (emths:struct-AxB-high-gov)
		   t)))
	    (emt:assert
	       (emtm
		  (emths:item-struct-AxB :field-A 12)
		  (satisfies comparand-f))))))
   (nil
      (progn
	 (emt:doc "Situation: field-B is given, mismatches.")
	 (emt:doc "Response: Match fails.")
	 (emt:assert
	    (not
	       (emtm
		  (emths:item-struct-AxB :field-B 13)
		  (emths:struct-AxB-high-gov :field-B 12))))))
   (nil
      (progn
	 (emt:doc "Situation: Both fields are given, match.")
	 (emt:doc "Response: Match succeeds.")
	 (emt:assert
	    (emtm
	       (emths:item-struct-AxB :field-A 12 :field-B 144)
	       (emths:struct-AxB-high-gov :field-A 12 :field-B 144)))))
   (nil
      (progn
	 (emt:doc "Proves: pattern-ctor works with `emtm-f' and
`emtm:make-pattern'.")
	 (emt:doc "Situation: A pattern-object is cted.
emtm-f param: That pattern-object.
emtm-f param: An object of that type.")
	 (emt:doc "Response: Pass/fail result is as expected.")
	 (let
	    ((pat
		(emtm:make-pattern
		   (emths:struct-AxB-high-gov :field-A 12))))
	    (emt:assert
	       (emtm-f
		  (emths:item-struct-AxB :field-A 12)
		  pat))
	    (emt:assert
	       (not
		  (emtm-f
		     (emths:item-struct-AxB :field-A 13)
		     pat)))
	    t))))


;;;_ , emths:struct-AxB:gov-literal

(emt:deftest-3 emths:struct-AxB:gov-literal
   (nil
      (progn
	 (emt:doc "Situation: Field-A field is given, matches.")
	 (emt:doc "Response: Match succeeds.")
	 (emt:assert
	    (emtm
	       (emths:item-struct-AxB :field-A 12)
	       (emths:struct-AxB-literal-gov :field-A 12)))))
   (nil
      (progn
	 (emt:doc "Situation: Field-A field is given, mismatches.")
	 (emt:doc "Response: Match fails.")
	 (emt:assert
	    (not
	       (emtm
		  (emths:item-struct-AxB :field-A 13)
		  (emths:struct-AxB-literal-gov :field-A 12))))))
   (nil
      (progn
	 (emt:doc "Situation: Field-A field is not given.")
	 (emt:doc "Response: Match succeeds.")
	 (emt:assert
	    (emtm
	       (emths:item-struct-AxB :field-A 12)
	       (emths:struct-AxB-literal-gov)))))
   (nil
      (progn
	 (emt:doc "Situation: Wrong type of object is given.")
	 (emt:doc "Response: Match fails.")
	 (emt:assert
	    (not
	       (emtm
		  (list 13)
		  (emths:struct-AxB-literal-gov :field-A 12))))))
   (nil
      (progn
	 (emt:doc "Situation: lambda-ized and used as lambda arg to `satisfies'.
Field-A field is given, matches.")
	 (emt:doc "Response: Match succeeds.")
	 (let
	    ((comparand-f
		(emtm:lambda
		   (emths:struct-AxB-literal-gov :field-A 12)
		   t)))
	    (emt:assert
	       (emtm
		  (emths:item-struct-AxB :field-A 12)
		  (satisfies comparand-f))))))
   (nil
      (progn
	 (emt:doc "Situation: lambda-ized and used as lambda arg to `satisfies'.
Field-A field is given, mismatches.")
	 (emt:doc "Response: Match fails.")
	 (emt:assert
	    (not
	       (let
		  ((comparand-f
		      (emtm:lambda
			 (emths:struct-AxB-literal-gov :field-A 12)
			 t)))
		  (emtm
		     (emths:item-struct-AxB :field-A 13)
		     (satisfies comparand-f)))))))
   (nil
      (progn
	 (emt:doc "Situation: lambda-ized and used as lambda arg to `satisfies'.
Field-A field is not given.")
	 (emt:doc "Response: Match succeeds.")
	 (let
	    ((comparand-f
		(emtm:lambda
		   (emths:struct-AxB-literal-gov)
		   t)))
	    (emt:assert
	       (emtm
		  (emths:item-struct-AxB :field-A 12)
		  (satisfies comparand-f))))))
   (nil
      (progn
	 (emt:doc "Proves: pattern-ctor works with `emtm-f' and
`emtm:make-pattern'.")
	 (emt:doc "Situation: A pattern-object is cted.
emtm-f param: That pattern-object.
emtm-f param: An object of that type.")
	 (emt:doc "Response: Pass/fail result is as expected.")
	 (let
	    ((pat
		(emtm:make-pattern
		   (emths:struct-AxB-literal-gov :field-A 12))))
	    (emt:assert
	       (emtm-f
		  (emths:item-struct-AxB :field-A 12)
		  pat))
	    (emt:assert
	       (not
		  (emtm-f
		     (emths:item-struct-AxB :field-A 13)
		     pat)))
	    t))))


;;$$WRITE ME - tests of reporting informative TESTRAL notes.
;;Or add those checks where we are already creating failure.
;;Strategy:
;;Intercept `emtm:report-false' and check prefix.
(emt:deftest-3 emtest/testhelp/match/tests
   (nil
      (progn
	 (emt:doc "Situation: WRITEME.")
	 (emt:doc "Response: WRITEME."))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/match/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/match/tests.el ends here
