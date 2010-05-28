;;;_ emtest/testhelp/match/rtest.el --- Rtest tests for match

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

;; $$CONVERT ME


;;;_ , Requires
(require 'rtest-define)
(require 'emtest/testhelp/match)
(require 'emtest/testhelp/match/testhelp)
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/misc)
(require 'utility/accumulator)

;;;_. Body

;;;_ , emtm:govs:list
;;
(rtest:deftest emtm:govs:list

   ;;Object's list character
   (  "Situation: Object is not a list
Two items in the pattern list.
One item in the list object.
Forcing emtm:govs:list.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:list 
	    '(list 12 144)
	    '13)))

   (  "Situation: List object is shorter:
Two items in the pattern list.
One item in the list object.
Forcing emtm:govs:list.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:list 
	    '(list 12 144)
	    '(12))))

   (  "Situation:  List object is longer:
Two items in the pattern list.
Three items in the list object.
Forcing emtm:govs:list.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:list 
	    '(list 12 144)
	    '(12 144 1728))))

   ;;Match each item in the list.
   (  "Situation: Zero items in the list.
Forcing emtm:govs:list.
Response: Match."
      (emtm:ts:single-gov
	 #'emtm:govs:list 
	 '(list)
	 '()))

   (  "Situation: One item in the list.
It matches.
Forcing emtm:govs:list.
Response: Match."
      (emtm:ts:single-gov
	 #'emtm:govs:list 
	 '(list 12)
	 '(12)))

   (  "Situation: One item in the list.
It mismatches.
Forcing emtm:govs:list.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:list 
	    '(list 12)
	    '(13))))
      
   (  "Situation: Two items in the list.
They both match.
Forcing emtm:govs:list.
Response: Match."
      (emtm:ts:single-gov
	 #'emtm:govs:list 
	 '(list 12 144)
	 '(12 144)))

   (  "Situation: Two items in the list.
First one mismatches.
Forcing emtm:govs:list.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:list 
	    '(list 12 144)
	    '(13 144))))

   (  "Situation: Two items in the list.
Second one mismatches.
Forcing emtm:govs:list.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:list 
	    '(list 12 144)
	    '(12 133))))

   
   (  "Situation: A list item is itself a pattern.
Response: Matches in the expected way."
      (emtm:ts:single-gov
	 #'emtm:govs:list 
	 '(list x 144)
	 '(12 144)))
   
   (  "Situation: Two list items are the same symbol.
Response: Matches in the expected way."
      (emtm:ts:single-gov
	 #'emtm:govs:list 
	 '(list x x)
	 '(12 12)))
   
   (  "Situation: Two list items are the same symbol.
Response: Matches in the expected way."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:list 
	    '(list x x)
	    '(13 169))))
   
   
   (  "Situation: A list item is itself a list pattern.
Response: Matches in the expected way."
      (emtm:ts:single-gov
	 #'emtm:govs:list 
	 '(list (list x x))
	 '((12 12))))
   (  "Situation: A list item is itself a list pattern.
Response: Matches in the expected way."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:list 
	    '(list (list x x))
	    '((13 169))))

      )

   )

;;;_ , emtm:govs:set
'
(rtest:deftest emtm:govs:set
   ;;Can share data with list tests.  Just need "match in other
   ;;order", and to choose (subtype set) instead of (subtype list)


   ;;Object's list character
   (  "Situation: Object is not a list
Two items in the pattern list.
One item in the list object.
Forcing emtm:govs:set.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:set 
	    '(set 12 144)
	    '13)))

   (  "Situation: List object is shorter:
Two items in the pattern list.
One item in the list object.
Forcing emtm:govs:set.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:set 
	    '(set 12 144)
	    '(12))))

   (  "Situation:  List object is longer:
Two items in the pattern list.
Three items in the list object.
Forcing emtm:govs:set.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:set 
	    '(set 12 144)
	    '(12 144 1728))))

   ;;Match each item in the list.
   (  "Situation: Zero items in the list.
Forcing emtm:govs:list.
Response: Match."
      (emtm:ts:single-gov
	 #'emtm:govs:set 
	 '(set)
	 '()))

   (  "Situation: One item in the list.
It matches.
Forcing emtm:govs:set.
Response: Match."
      (emtm:ts:single-gov
	 #'emtm:govs:set 
	 '(set 12)
	 '(12)))

   (  "Situation: One item in the list.
It mismatches.
Forcing emtm:govs:set.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:set 
	    '(set 12)
	    '(13))))
      
   (  "Situation: Two items in the list.
They both match.
Forcing emtm:govs:set.
Response: Match."
      (emtm:ts:single-gov
	 #'emtm:govs:set 
	 '(set 12 144)
	 '(12 144)))

   (  "Situation: Two items in the list.
First one mismatches.
Forcing emtm:govs:set.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:set 
	    '(set 12 144)
	    '(13 144))))

   (  "Situation: Two items in the list.
Second one mismatches.
Forcing emtm:govs:set.
Response: Mismatch."
      (not
	 (emtm:ts:single-gov
	    #'emtm:govs:set 
	    '(set 12 144)
	    '(12 133))))

   ;;Specific to set functionality:
   (  "Situation: Two items.  They match in other order
Response: Match."
      (emtm:ts:single-gov
	 #'emtm:govs:set 
	 '(set 12 144)
	 '(144 12)))
   

   (  "Situation: Two items.  One pattern element matches both, the
other matches neither.
Response: Error (Not just mismatch)."
      (emth:gives-error
	 (emtm:ts:single-gov
	    #'emtm:govs:set 
	    '(set 12 144)
	    '(12 12))))
   )

;;;_ , emtm:govs:satisfies
(rtest:deftest emtm:govs:satisfies
   ;;NB, `list' that we're testing this with will return the object as
   ;;the first arg, other args as further args.

   (  "Proves: Whether it succeeds is whether pred succeeds.
Param: Pred `list'
Response: Satisfies it (because list with 1 arg always gives non-nil)."

      (and
	 (emtm 0
	    (satisfies #'list (a)))))

   (  "Proves: Whether it succeeds is whether pred succeeds.
Param: Pred `ignore'
Response: Fails (because `ignore' always gives nil)."

      (not
	 (emtm 0
	    (satisfies #'ignore))))
   
   (  "Proves: Uses the ret-patterns.
Object doesn't match the sub-patterns.
Response: Returns nil."
      (not
	 (emtm 13
	    (satisfies #'list (12)))))
   
   (  "Proves: Uses the ret-patterns.
Object matches the sub-patterns.
Response: Returns non-nil."
      (and
	 (emtm 12
	    (satisfies #'list (12)))))
   
   ;;Bindings in ret-patterns are available to other objects.
   (  "Proves: Makes bindings in the ret-patterns available to the
rest of the pattern.
Bindings in the sub-patterns are used.
Behavior: Does not error.  The correct values are seen."
      (and
	 (equal
	    (emtm-let 12
	       (satisfies #'list (a))
	       a)
	    12)))
   

   (  "Proves: The predicate sees the other arguments.
Param: Pred `list'.
Response: Produces the expected result."
      (and
	 (equal
	    (emtm-let '()
	       (satisfies #'list (obj a b) 12 144)
	       (list a b))
	    '(12 144))))

   (  "Proves: The extra argument are forms, not values.
Param: Pred `list'
Response: Produces the expected result."
      (and
	 (equal
	    (emtm-let '()
	       (satisfies #'list (obj a b) (+ 6 6) (* 12 12))
	       (list a b))
	    '(12 144))))

   
   (  "Situation: The pred succeeds and returns a non-list.  Specifically, 12.
We make no bindings.
Response: Succeeds."
      (and
	 (emtm 12
	    (satisfies #'identity))))

   
   ;;NOT provided: Given the binding pattern as something other than a
   ;;list, adapts OK.  This would require normal bindings to be headed
   ;;by `list'

   ;;Does not contemplate letting `pred' be a binding.
   )

;;;_ , emtm:govs:eval
(rtest:deftest emtm:govs:eval

   (  "Situation: Arg, when evalled, matches the object.
Response: Succeed."
      (and
	 (emtm 12
	    (eval '(+ 6 6)))))
   
   (  "Situation: Arg, when evalled, does not match the object.
Response: Fail."
      (not 
	 (emtm 13
	    (eval '(+ 6 6)))))

   (  "Situation: Arg, unevalled, matches the object, but evalled it
does not.
Response: Fail."
      (not 
	 (emtm '(+ 6 6)
	    (eval '(+ 6 6)))))

   ;;For now at least.  Later such objects might be bound at match
   ;;time.
   (  "Param: Arg has a non-constant object.
Response: Error."
      (emth:gives-error
	 (let
	    ((a 0))
	    (emtm 0
	       (eval a)))))
   
   ;;Tests dealing with expansion time

   (  "Situation: Arg, when evalled, matches the object.
Object is a form, so it can be further evalled.
Response: Succeed."
      (and
	 (emtm '(+ 6 6)
	    (eval ''(+ 6 6)))))

   (  "Proves: Eval occurs when pattern is compiled, not at match time.
Situation: At pattern-compile time, `a' is bound to 12.
At match time, `a' is bound to 13.
Object is 12.
Response: Passes.
Object is 13.
Response: Fails"
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
	       (emtm
		  12
		  (satisfies comparand-f))
	       (not
		  (emtm
		     13
		     (satisfies comparand-f)))))))
   
   
   )

;;;_   , emtm:sort-bindings

(put 'emtm:sort-bindings 'rtest:test-thru
   'emtm:build-form)
;;$$WRITE MY TEST: Error if the same form or binding occurs more than once.

;;;_ , emtm:build-form
(rtest:deftest emtm:build-form

   (  "Param: formdata list is an empty list.
Response: Form acts as just core."
      (equal
	 (eval
	    (emtm:build-form 
	       '(x) 
	       (emtm:make-formdata :form-LIST ())
	       12))
	 12))
   
   

   (  "Situation: formdata list gives dependencies out of order.
But they are not intrinsically circular, just out of order.
The bindings are given in an inconsistent order, so we're not just
accidentally processing them in the right order.
The bindings are required and used, so we're not just testing
simplification of redundant bindings.

Response: Still works OK."

      (equal
	 (let
	    ((x 12))
	    (eval
	       (emtm:build-form 
		  '(x) 
		  (emtm:make-formdata 
		     :form-LIST 
		     (list
			(emtm:make-binding-form-data
			   :bind 'z
			   :uses '(y)
			   :form 'y)
			(emtm:make-binding-form-data
			   :bind 'y
			   :uses '(x)
			   :form 'x)
			(emtm:make-binding-form-data
			   :bind 'w
			   :uses '(z)
			   :form 'z)))
		  '(list w x y z))))
	 (list 12 12 12 12)))
   
   (  "Situation: Same as the bindings, but now with tests.

formdata list gives dependencies out of order.
But they are not intrinsically circular, just out of order.
The bindings are given in an inconsistent order, so we're not just
accidentally processing them in the right order.
The bindings are required and used, so we're not just testing
simplification of redundant bindings.

Response: Still works OK."

      (equal
	 (let
	    ((x 12)
	       ;;Some forms that are always satisfied.  All different,
	       ;;lest they trigger merging or ambiguity errors
	       (form-y 1)
	       (form-z 2)
	       (form-w 3))
	    (eval
	       (emtm:build-form 
		  '(x) 
		  (emtm:make-formdata 
		     :form-LIST 
		     (list
			(emtm:make-test-form-data
			   :uses (list form-y)
			   :form form-z)
			(emtm:make-test-form-data
			   :uses '(x)
			   :form form-y)
			(emtm:make-test-form-data
			   :uses (list form-z)
			   :form form-w)))
		  ''ok)))
	 'ok))

   )

;;;_ , emtm:build-form--1
(put 'emtm:build-form--1 'rtest:test-thru
   'emtm)

;;;_ , emtm:ts:single-gov

;;This is to validate that its results agree with known results, so
;;it's OK to use this for testing governor functions in isolation.
(rtest:deftest emtm:ts:single-gov

   ("Situation: Function is `emtm:govs:literal'
Object matches the literal pattern.
Response: Gives non-nil."
      (and (emtm:ts:single-gov #'emtm:govs:literal 12 12)))

   ("Situation: Function is `emtm:govs:literal'
Object mismatches the literal pattern.
Response: Gives nil."
      (not (emtm:ts:single-gov #'emtm:govs:literal 12 13)))

   ("Situation: Function is `emtm:govs:symbol'.
Object of course matches the symbol pattern.
Response: gives non-nil."
      (emtm:ts:single-gov #'emtm:govs:symbol 'a 12))    
  
   )
;;;_ , emtm
(rtest:deftest emtm

   ("Situation: A literal pattern matches.
Response: Gives non-nil."

      (emtm 12 12))

   ("Situation: A literal pattern matches.
That pattern is a quoted list.
Response: Gives non-nil."

      (and (emtm '(12) '(12))))

   ("Situation: A literal pattern mismatches.
Response: Gives nil."

      (not (emtm 13 12)))

   ("Situation: A symbol
Response: Gives non-nil."

      (and (emtm 12 a)))

   ;;Governed lists.  Data should be from tagname examples.
   
   (  "Situation: Pattern is a governed list
Response: Works."
      (and
	 (emtm
	    '(12)
	    (list 12)))
      )

   (  "Situation: Pattern is a governed list
Response: Works."
      (not
	 (emtm
	    '(13)
	    (list 12)))
      )


   '
   ("Situation: Governed by `is-a'.
Response: Gives non-nil."

      (and (emtm 12 (is-a 'integer))))
   '
   ("Situation: Governed by `equal'.  
Both pattern arguments match the object.
Response: Gives non-nil."

      (and (emtm 12 (equal a 12))))
      
   '
   ("Situation: Governed by `equal'.  
Only one of the pattern arguments matches the object.
Response: Gives non-nil."

      (and (emtm 12 (equal a 13))))

   ;;Maybe re-order this to test the builder instead.
   ;;Binding multiple things
   (  "Situation: Pattern has the same variable occuring twice
Response: Works."
      (and
	 (emtm
	    '(12 12)
	    (list a a)))
      )
   (  "Situation: Pattern has the same variable occuring twice
Response: Works."
      (and
	 (emtm
	    '(144 144)
	    (list a a)))
      )
   (  "Situation: Pattern has the same variable occuring twice
Response: Works."
      (not
	 (emtm
	    '(12 13)
	    (list a a))))

   (  "Situation: Pattern is made by a general lambda, 1 extra arg.
Then it's instantiated, with the extra binding being an object.
Behavior: `a' must be equal to that object.
Response: Works."
      (let* 
	 ((pat-f
	     (emtm:make-general-lambda 
		;;Pattern
		(list a)
		;;Bindings
		(a)
		;;Body
		t
		)))
	 (assert
	    (funcall pat-f '(12) 12))
	 (assert
	    (funcall pat-f '(144) 144))
	 (assert
	    (not (funcall pat-f '(13) 12)))
	 t))

   ;;The extra args can be (boxed) patterns.  Then the part being
   ;;accessed has to match that pattern.  
   (  "Situation: Pattern is made by a general lambda, 1 extra arg.
Then it's instantiated, with the extra binding being a match box.
Behavior: `a' must be match that match-box's pattern.
Response: Works."
      (let* 
	 ((pat-f
	     (emtm:make-general-lambda 
		;;Pattern
		(list a)
		;;Bindings
		(a)
		;;Body
		t
		))
	    ;;Boxed pattern 1
	    (boxed-pat-1 (emtm:make-pattern 12))
	    ;;Boxed pattern 2
	    (boxed-pat-2 (emtm:make-pattern 144)))
	 (assert
	    (funcall pat-f '(12) boxed-pat-1))
	 (assert
	    (funcall pat-f '(144) boxed-pat-2))
	 (assert
	    (not (funcall pat-f '(13) boxed-pat-1)))
	 t))


   ;;Not written yet:

   ;;Binding, one inside `equal':
   ;;second one inside
   ;;first one inside

   ;;Binding and testing `satisfies'

   ;;Vectors


   ;;Structural mismatches don't cause error, they just cause failure.


   ;;Test reporting.  Report potentially many misses until hard
   ;;failure makes us stop.  How to test test-reporting?
   '
   ("Situation: Comparing lists of 2 items
Mismatches on both items.

Only one of the pattern arguments matches the object.
Response: Notes 2 mismatches, gives nil."

      (not (emtm '(12 12) '(13 13))))
   )

'
(emt:deftest-2 emtm
   ;;Literals
   ("Situation: A literal pattern matches.
Response: Gives non-nil."
      (should 
	 (emtm 12 12)))

   ("Situation: A literal pattern mismatches.
Response: Gives nil."
      (should
	 (not (emtm 13 12))))

   )

;;;_ , emtm-f
(put 'emtm-f 'rtest:test-thru
   'emtm:make-pattern)

;;;_ , emtm-either
(put 'emtm-either 'rtest:test-thru
   'emtm:make-pattern)

;;;_   , emtm:make-pattern
(rtest:deftest emtm:make-pattern

   (  "Proves: `emtm-f' and `emtm:make-pattern' work together.
Situation: A pattern-object is cted.
emtm-f param: That pattern-object.
Response: Pass/fail result is as expected."
      (let ((pat (emtm:make-pattern 12)))
	 (assert (emtm-f 12 pat))
	 (assert (not (emtm-f 13 pat)))
	 t))

   (  "Proves: The boxed pattern can be used in other patterns.
Situation: A pattern-object is cted.
It is used as part of another pattern.
Response: Pass/fail result is as expected."
      (let* ((pat (emtm:make-pattern 12))
	       (func
		  (emtm:lambda
		     (list (eval 'pat) 144)
		     t)))
	 (assert (funcall func '(12 144)))
	 (assert (not (funcall func '(13 144))))
	 t)))
;;;_ , emtm:lambda
(rtest:deftest emtm:lambda

   (  "Situation: Body returns `a'
Behavior: Returns the expected success.
When successful, returns the expected value.
When unsuccessful, returns `nil'"
      (let
	 ((func 
	     (emtm:lambda
		(list 12 a)
		(list a 1728))))
	 (and
	    (funcall func '(12 144))
	    ;;The returned value should be as expected.
	    (equal
	       (funcall func '(12 144))
	       '(144 1728))
	    (not (funcall func '(13 13)))))))
;;;_ , emtm:lambda-binds
(rtest:deftest emtm:lambda-binds

   (  "Situation: No bindings
Behavior: Returns the expected success."
      (let
	 ((func 
	     (emtm:lambda-binds 12)))
	 (and
	    (funcall func 12)
	    (not (funcall func 13)))))
   
   
   (  "Situation: Some bindings.
Specifically, one binding.
Behavior: Returns the expected success.
When successful, returns the expected list of values."
      (let
	 ((func 
	     (emtm:lambda-binds
		(list 12 a)
		a)))
	 (and
	    (funcall func '(12 144))
	    ;;The returned value should be as expected.
	    (equal
	       (funcall func '(12 144))
	       '(144))
	    (not (funcall func '(13 13)))))))

;;;_ , emtm-let
(rtest:deftest emtm-let

   (  "Proves: The body's return value is returned.
Situation: Trivial PATTERN matches OBJECT-FORM
Response: The body's return value is returned."
      (equal
	 (emtm-let 0 0
	    12)
	 12))

   (  "Proves: The bindings are available in BODY.
Situation: Trivial pattern matches object.
Response: The body's return value is returned."
      (equal
	 (emtm-let '(12 144) (list a b)
	    (list b a))
	 (list 144 12)))
   
   ;;Behavior on mis-match ... this is TBD, see docs.
   )
;;;_ , emtm:pattern
(rtest:deftest emtm:pattern

   (  "Proves: We can test for type.
Response: typep's pass/fail is as expected."
      (let ((pat (emtm:make-pattern 12)))
	 (assert (typep pat '(emtm:pattern *)))
	 (assert (not (typep 13 '(emtm:pattern *))))
	 t))
   ;;We punt more exact type checks for now.
   ;;For `repeat' 
   ;;For `list'.
   ;;For where we have defined the underlying type and a pattern-ctor.

   )
;;;_ , emtm:define-struct-governor*
(put 'emtm:define-struct-governor* 'rtest:test-thru
   'rtest-struct:gov-high-level)
;;;_ , emtm:define-struct-governor
;;;_  . Test strategy

;;Test this on the usual test structure type `rtest-struct'.  In
;;rtest-tools.

;;We want to use the same tests (and more) on the general function
;;that worked for the particular one.

;;Could rebuild them with the same data.  But without emtest yet, then
;;it's tough to see what parts failed.  For now, I am just
;;rebuilding the function both ways, to test it.


;;;_  . Test data

;;For now, no examples are defined, we just repeat all the data for
;;every test.

;;The structure definitions, and some aliases to be conformant.
(require 'rtest-tools)
(defalias 'rtest-struct->my-field 'rtest-struct-my-field)
(defalias 'rtest-struct->my-second-field 'rtest-struct-my-second-field)
(defalias 'rtest-struct-make-item 'make-rtest-struct)

;;;_   , Setup

'  ;;The old way, which was incomplete.
(setf (symbol-function 'rtest-struct:gov-high-level)
   (emtm:make-struct-governor 
      rtest-struct-p 
      rtest-struct-> 
      (my-field my-second-field)))

;;Assign it to a governor.
'  ;;The medium-old way
(put 'rtest-struct-high-gov 'emtm:makepattern
   (emtm:make-struct-governor 
      rtest-struct-p 
      rtest-struct-> 
      (my-field my-second-field)))

;;The slightly old way
'
(emtm:define-struct-governor-oldstyle
   rtest-struct-high-gov
   rtest-struct-p 
   rtest-struct-> 
   (my-field my-second-field))


(emtm:define-struct-governor
    (rtest-struct
       (:constructor rtest-struct-high-gov)
       (:predicate rtest-struct-p)
       (:conc-name rtest-struct->))
    
    my-field 
    my-second-field)

;;;_  . Tests

(put 'emtm:make-struct-governor 'rtest:test-thru
   'rtest-struct:gov-high-level)

(rtest:deftest rtest-struct:gov-high-level

   ;;Proves: Object type is checked
   (  "Situation: Wrong type of object is given.
Response: Match fails."
      (not
	 (emtm
	    (list 13)
	    (rtest-struct-high-gov :my-field 12))))

   ;;Proves: A field is matched on, unless not given.
   (  "Situation: My-Field field is given, matches.
Response: Match succeeds."
      (emtm
	 (rtest-struct-make-item :my-field 12)
	 (rtest-struct-high-gov :my-field 12)))
   
   (  "Situation: My-Field field is given, mismatches.
Response: Match fails."
      (not
	 (emtm
	    (rtest-struct-make-item :my-field 13)
	    (rtest-struct-high-gov :my-field 12))))

   (  "Situation: My-Field field is not given.
Response: Match succeeds."
      (emtm
	 (rtest-struct-make-item :my-field 12)
	 (rtest-struct-high-gov)))



   ;;Group: `emtm-lamba' version can be used as lambda arg to
   ;;`satisfies'.  These tests repeat the earlier data, except the
   ;;comparand (Here it's a function, not an object)
   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is given, matches.
Response: Match succeeds."
      (let
	 ;;`comparand-f' is a variable, not a function, so don't
	 ;;function-quote it.
	 ((comparand-f
	     (emtm:lambda
		(rtest-struct-high-gov :my-field 12)
		t)))
	 (emtm
	    (rtest-struct-make-item :my-field 12)
	    (satisfies comparand-f))))

   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is given, mismatches.
Response: Match fails."
      (not
	 (let
	    ((comparand-f
		(emtm:lambda
		   (rtest-struct-high-gov :my-field 12)
		   t)))
	    (emtm
	       (rtest-struct-make-item :my-field 13)
	       (satisfies comparand-f)))))

   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is not given.
Response: Match succeeds."
      (let
	 ((comparand-f
	     (emtm:lambda
		(rtest-struct-high-gov)
		t)))
	 (emtm
	    (rtest-struct-make-item :my-field 12)
	    (satisfies comparand-f))))

   ;;Proves: Works with other fields

   (  "Situation: my-second-field is given, mismatches.
Response: Match fails."
      (not
	 (emtm
	    (rtest-struct-make-item :my-second-field 13)
	    (rtest-struct-high-gov :my-second-field 12))))

   ;;Proves: Works with multiple fields

   (  "Situation: Both fields are given, match.
Response: Match succeeds."
      (emtm
	 (rtest-struct-make-item :my-field 12 :my-second-field 144)
	 (rtest-struct-high-gov :my-field 12 :my-second-field 144)))

   (  "Proves: pattern-ctor works with `emtm-f' and
`emtm:make-pattern'. 
Situation: A pattern-object is cted.
emtm-f param: That pattern-object.
emtm-f param: An object of that type.
Response: Pass/fail result is as expected."
      (let ((pat 
	       (emtm:make-pattern 
		  (rtest-struct-high-gov :my-field 12))))
	 (assert (emtm-f 
		    (rtest-struct-make-item :my-field 12) 
		    pat))
	 (assert (not 
		    (emtm-f
		       (rtest-struct-make-item :my-field 13) 
		       pat)))
	 t))

   )

;;;_ , rtest-struct:gov-literal

(rtest:deftest rtest-struct:gov-literal
   ;;Proves: My-Field fields are matched on, unless not given.
   (  "Situation: My-Field field is given, matches.
Response: Match succeeds."
      (emtm
	 (rtest-struct-make-item :my-field 12)
	 (rtest-struct-literal-gov :my-field 12)))
   
   (  "Situation: My-Field field is given, mismatches.
Response: Match fails."
      (not
	 (emtm
	    (rtest-struct-make-item :my-field 13)
	    (rtest-struct-literal-gov :my-field 12))))

   (  "Situation: My-Field field is not given.
Response: Match succeeds."
      (emtm
	 (rtest-struct-make-item :my-field 12)
	 (rtest-struct-literal-gov)))
   
   (  "Situation: Wrong type of object is given.
Response: Match fails."
      (not
	 (emtm
	    (list 13)
	    (rtest-struct-literal-gov :my-field 12))))


   ;;Group: `emtm-lamba' version can be used as lambda arg to
   ;;`satisfies'.  These tests repeat the earlier data, except the
   ;;comparand (Here it's a function, not an object)
   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is given, matches.
Response: Match succeeds."
      (let
	 ;;`comparand-f' is a variable, not a function, so don't
	 ;;function-quote it.
	 ((comparand-f
	     (emtm:lambda
		(rtest-struct-literal-gov :my-field 12)
		t)))
	 (emtm
	    (rtest-struct-make-item :my-field 12)
	    (satisfies comparand-f))))

   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is given, mismatches.
Response: Match fails."
      (not
	 (let
	    ((comparand-f
		(emtm:lambda
		   (rtest-struct-literal-gov :my-field 12)
		   t)))
	    (emtm
	       (rtest-struct-make-item :my-field 13)
	       (satisfies comparand-f)))))

   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is not given.
Response: Match succeeds."
      (let
	 ((comparand-f
	     (emtm:lambda
		(rtest-struct-literal-gov)
		t)))
	 (emtm
	    (rtest-struct-make-item :my-field 12)
	    (satisfies comparand-f))))


   (  "Proves: pattern-ctor works with `emtm-f' and
`emtm:make-pattern'. 
Situation: A pattern-object is cted.
emtm-f param: That pattern-object.
emtm-f param: An object of that type.
Response: Pass/fail result is as expected."
      (let ((pat 
	       (emtm:make-pattern 
		  (rtest-struct-literal-gov :my-field 12))))
	 (assert (emtm-f 
		    (rtest-struct-make-item :my-field 12) 
		    pat))
	 (assert (not 
		    (emtm-f
		       (rtest-struct-make-item :my-field 13) 
		       pat)))
	 t))
   )



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/match/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/match/rtest.el ends here
