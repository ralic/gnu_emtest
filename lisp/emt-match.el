;;;_ emt-match.el --- Expression matcher for EMtest

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

;; 


;;;_ , Requires

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
;;`tehom-cl' needed for `constantp'
(require 'tehom-cl)
(require 'emt-accumulator)
(eval-when-compile
   (require 'cl))
;;;_. Body
;;;_ , Structures
;;;_  . Accumulator definition
(emt:accumulator:define 
   (emt:match:formdata
      (:constructor emt:match:make-formdata)
      (:conc-name emt:match:formdata->))
   "The type that a pattern governor returns"
   form
   pre-eval
   )

;;;_  . Binding-form data
(defstruct (emt:match:binding-form-data
	      (:constructor emt:match:make-binding-form-data)
	      (:conc-name emt:match:binding-form-data->))
   ""
   (uses ())
   (bind nil :type symbol)
   (rebinding-p nil :type bool)
   form)
;;;_  . Test-form data
(defstruct (emt:match:test-form-data
	      (:constructor emt:match:make-test-form-data)
	      (:conc-name emt:match:test-form-data->))
   ""
   ;;A form that, if non-nil, makes a string explaining the mismatch
   explanation
   (uses ())
   form)
;;;_  . Box
(defstruct (emt:match:box
	      (:constructor emt:match:make-box)
	      (:conc-name emt:match:box->))
   "A pattern, represented as an object"
   func
   type)

;;;_ , Utilities
;;;_  . emt-match:proper-list-p
;;Borrowed from `format'.
(defun emt-match:proper-list-p (list)
  "Return t if LIST is a proper list.
A proper list is a list ending with a nil cdr, not with an atom "
  (when (listp list)
    (while (consp list)
      (setq list (cdr list)))
    (null list)))

;;;_  . emt:match:parse-dependencies
(defsubst emt:match:parse-dependencies (binds &optional tests)
   "Parse the dependencies, for feeding the `uses' field.
BINDS must be a list of symbols.
TESTS, if given, must be a list of emt:match:test-form-data."
   (append
      binds
      (mapcar
	 #'emt:match:test-form-data->form
	 tests)))


;;;_ , Parse pattern  emt-match:parse-pattern
(defun emt-match:parse-pattern (sym pattern &optional other-deps)
   ""

   ;;Mutually recurses with most governors' functions.
   (let
      ;;Get the function.
      ((func
	  (cond
	     ((constantp pattern)
		#'emt-match:govs:literal)
	     ((symbolp pattern)
		#'emt-match:govs:symbol)
	     ((vectorp pattern)
		;;Punt for now.  Function is essentially vector
		;;function, but must destructure the pattern first.
		)
	     ((consp pattern)
		(emt-match:gov->function (car pattern)))
	     (t nil))))

      ;;Use the function we got.
      (if func
	 (funcall func sym pattern other-deps)
	 (error "Could not find a governor function"))))

;;;_ , Map governors to functions

(defun emt-match:gov->function (gov-sym)
   "Return the respective pattern function of GOV-SYM's governor"

   (get gov-sym 'emt-match:makepattern))


;;;_ , Basic governors' functions
;;;_  . emt-match:govs:literal Pseudo-governor for literals
(defun emt-match:govs:literal (sym pattern &optional other-deps)
   ""
   (emt:match:make-formdata
      :form-SINGLE 
      (emt:match:make-test-form-data
	 :uses (list* sym other-deps)
	 :form `(equal ,sym ,pattern))))

;;;_  . emt-match:govs:symbol Pseudo-governor for symbols
;;Matches anything, binds something.
(defun emt-match:govs:symbol (sym pattern &optional other-deps)
   ""
   (emt:match:make-formdata
      :form-SINGLE 
      (emt:match:make-binding-form-data
	 :bind pattern
	 :form sym)))

;;;_  . emt-match:govs:list Governor for `list'

(defun emt-match:govs:list (sym pattern &optional other-deps)
   "
PATTERN is headed by governor"

   ;;Guarantee that `pattern' is a proper list.
   (unless
      (emt-match:proper-list-p pattern)
      (error "Argument to `emt-match:govs:list' must be a list"))
   
   (let* 
      (
	 (pattern-els (cdr pattern))
	 ;;List length
	 (len (length pattern-els))

	 ;;Object must be a proper list
	 (formdata-test-listness
	    (emt:match:make-test-form-data
	       :explanation "Object is not a proper list"
	       :uses (list* sym other-deps)
	       :form `(emt-match:proper-list-p ,sym)))
	 
	 ;;Object must be the expected length
	 (formdata-test-length
	    (emt:match:make-test-form-data
	       :explanation "Object is not the right length"
	       :uses (emt:match:parse-dependencies 
			(list sym)
			(list formdata-test-listness))
	       :form  `(= (length ,sym) ,len)))

	 ;;List of gensyms which will each refer to an element.
	 (sym-list
	    (loop repeat len
	       collect (gensym)))

	 ;;Bindings: Bind a symbol to each element.
	 (binding-forms
	    (loop
	       for el-sym in sym-list
	       for i upto len
	       collect
	       (emt:match:make-binding-form-data
		  :uses 
		  (emt:match:parse-dependencies
		     (list sym)
		     (list formdata-test-length))
		  :bind el-sym
		  :form `(nth ,i ,sym))))
	 
	 ;;Do any further pattern operations on each element.
	 ;;Recurse, informed by the respective symbol and pattern.
	 ;;This makes full formdata, not element formdata, so be
	 ;;careful of the distinction.
	 (child-formdata-list
	    (mapcar*
	       #'emt-match:parse-pattern
	       sym-list
	       pattern-els)))

      ;;Result: All the forms we just made.
      (emt:accumulator:list->object
	 (list*
	    (emt:match:make-formdata
	       :form-LIST 
	       (list*
		  formdata-test-listness
		  formdata-test-length
		  binding-forms))
	    child-formdata-list)
	 'emt:match:formdata)))
(put 'list 'emt-match:makepattern
   #'emt-match:govs:list)


;;;_   , Test examples

(emt:eg:define xmp:3be9cd9e-25f7-4f9d-9304-8afc1680d17a
   ((project emtest)
      (library emt-match)
      (section emt-match:govs:list))

   ;;Structure matches & mismatches together?  Not until tester use of
   ;;docs and iterating-over is stronger.  We'd want to iterate over
   ;;combinations of 2 things: pattern-situation and subordinately,
   ;;object-situation. 
   (group ((situation one-item-t))
      (doc ()
	 "One item in the list.  It matches.")
      (item ((type pattern)(subtype list))
	 '(list 12))
      (item ((type pattern)(subtype set))
	 '(set 12))
      (item ((type object))
	 '(12))
      (item ((type matches-p)) 
	 t))

   (group ((situation one-item-f))
      (doc ()
	 "One item in the list.  It mismatches.")
      (item ((type pattern)(subtype list))
	 '(list 12))
      (item ((type pattern)(subtype set))
	 '(set 12))
      (item ((type object))
	 '(13))
      (item ((type matches-p)) 
	 nil))
   ;;Similarly for the other test data.

   
   )

;;;_   , Tests
;;
(rtest:deftest emt-match:govs:list

   ;;Object's list character
   (  "Situation: Object is not a list
Two items in the pattern list.
One item in the list object.
Forcing emt-match:govs:list.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:list 
	    '(list 12 144)
	    '13)))

   (  "Situation: List object is shorter:
Two items in the pattern list.
One item in the list object.
Forcing emt-match:govs:list.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:list 
	    '(list 12 144)
	    '(12))))

   (  "Situation:  List object is longer:
Two items in the pattern list.
Three items in the list object.
Forcing emt-match:govs:list.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:list 
	    '(list 12 144)
	    '(12 144 1728))))

   ;;Match each item in the list.
   (  "Situation: Zero items in the list.
Forcing emt-match:govs:list.
Response: Match."
      (emt:match:ts:single-gov
	 #'emt-match:govs:list 
	 '(list)
	 '()))

   (  "Situation: One item in the list.
It matches.
Forcing emt-match:govs:list.
Response: Match."
      (emt:match:ts:single-gov
	 #'emt-match:govs:list 
	 '(list 12)
	 '(12)))

   (  "Situation: One item in the list.
It mismatches.
Forcing emt-match:govs:list.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:list 
	    '(list 12)
	    '(13))))
      
   (  "Situation: Two items in the list.
They both match.
Forcing emt-match:govs:list.
Response: Match."
      (emt:match:ts:single-gov
	 #'emt-match:govs:list 
	 '(list 12 144)
	 '(12 144)))

   (  "Situation: Two items in the list.
First one mismatches.
Forcing emt-match:govs:list.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:list 
	    '(list 12 144)
	    '(13 144))))

   (  "Situation: Two items in the list.
Second one mismatches.
Forcing emt-match:govs:list.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:list 
	    '(list 12 144)
	    '(12 133)))))

;;;_  . (DORMANT) emt-match:govs:set Governor for sets (unordered lists)
'
(defun emt-match:govs:set (sym pattern &optional other-deps)
   ""
   ;;This will largely share code with emt-match:govs:list

   ;;It basically uses `child-formdata-list' as its pattern input.

   ;;Error if any element of child-formdata-list binds anything,
   ;;because we don't support that (yet?)

   ;;Set up to call a runtime comparer.  Since we don't allow set
   ;;element patterns to make bindings, we need merely call it.  We're
   ;;not interested in the permuted list that it builds.

   (let*
      ()
      
      ))
'
(defun emt-match:runtime:set (list1 list2)
   ""
   ;;Sketched, not tested.

   ;;We can assume that the object are grossly compatible: Lists of
   ;;the same length.
   (let*
      ((corresponding-indexes '()))

      ;;dolist on list1

      ;;Pick first item from list1

      ;;Try to match it to any item in list2 (via `member*'?)
      
      ;;If it matches further elements, error.

      ;;Get its index.
      
      ;;If its index can be found in corresponding-indexes, it was
      ;;duplicated, so error.

      ;;Add its index to corresponding-indexes
      ))

;;;_   , Tests
'
(rtest:deftest emt-match:govs:set
   ;;Can share data with list tests.  Just need "match in other
   ;;order", and to choose (subtype set) instead of (subtype list)


   ;;Object's list character
   (  "Situation: Object is not a list
Two items in the pattern list.
One item in the list object.
Forcing emt-match:govs:set.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:set 
	    '(set 12 144)
	    '13)))

   (  "Situation: List object is shorter:
Two items in the pattern list.
One item in the list object.
Forcing emt-match:govs:set.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:set 
	    '(set 12 144)
	    '(12))))

   (  "Situation:  List object is longer:
Two items in the pattern list.
Three items in the list object.
Forcing emt-match:govs:set.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:set 
	    '(set 12 144)
	    '(12 144 1728))))

   ;;Match each item in the list.
   (  "Situation: Zero items in the list.
Forcing emt-match:govs:list.
Response: Match."
      (emt:match:ts:single-gov
	 #'emt-match:govs:set 
	 '(set)
	 '()))

   (  "Situation: One item in the list.
It matches.
Forcing emt-match:govs:set.
Response: Match."
      (emt:match:ts:single-gov
	 #'emt-match:govs:set 
	 '(set 12)
	 '(12)))

   (  "Situation: One item in the list.
It mismatches.
Forcing emt-match:govs:set.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:set 
	    '(set 12)
	    '(13))))
      
   (  "Situation: Two items in the list.
They both match.
Forcing emt-match:govs:set.
Response: Match."
      (emt:match:ts:single-gov
	 #'emt-match:govs:set 
	 '(set 12 144)
	 '(12 144)))

   (  "Situation: Two items in the list.
First one mismatches.
Forcing emt-match:govs:set.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:set 
	    '(set 12 144)
	    '(13 144))))

   (  "Situation: Two items in the list.
Second one mismatches.
Forcing emt-match:govs:set.
Response: Mismatch."
      (not
	 (emt:match:ts:single-gov
	    #'emt-match:govs:set 
	    '(set 12 144)
	    '(12 133))))

   ;;Specific to set functionality:
   (  "Situation: Two items.  They match in other order
Response: Match."
      (emt:match:ts:single-gov
	 #'emt-match:govs:set 
	 '(set 12 144)
	 '(144 12)))
   

   (  "Situation: Two items.  One pattern element matches both, the
other matches neither.
Response: Error (Not just mismatch)."
      (emt:gives-error
	 (emt:match:ts:single-gov
	    #'emt-match:govs:set 
	    '(set 12 144)
	    '(12 12))))
   )

;;;_  . emt-match:govs:satisfies
(defun emt-match:govs:satisfies (sym pattern &optional other-deps)
   ""
   ;;Destructure pattern to get bindings, extra args.
   (destructuring-bind
      (gov pred &optional ret-patterns &rest args)
      pattern
      (let* 
	 ((ret-sym (gensym))
	    ;;Call it, passing sym and args (which are forms)
	    ;;binding the return value to `ret-sym'
	    (call-formdata
	       (emt:match:make-binding-form-data
		  :uses (list* sym other-deps)
		  :bind ret-sym
		  :form `(funcall ,pred ,sym ,@args)))
	    ;;Test the return value to determine satisfaction.
	    (testret-formadata
	       (emt:match:make-test-form-data
		  :explanation "Call to predicate failed"
		  :uses (list ret-sym)
		  ;;`identity' is used solely to distinguish this form
		  ;;from a mere binding, wrt dependencies.
		  :form `(identity ,ret-sym))))

	 (emt:accumulator:list->object
	    (append
	       (list
		  (emt:match:make-formdata
		     :form-LIST 
		     (list
			call-formdata
			testret-formadata)))
	       
	       ;;The return value is bound by RET-PATTERNS, if given.
	       (if ret-patterns
		  (list 
		     ;;We prepend to `ret-patterns' to account for the
		     ;;head, which governor functions expect to get
		     ;;too.
		     (emt-match:govs:list ret-sym 
			(cons 'list ret-patterns)))
		  ()))
	    'emt:match:formdata))))

(put 'satisfies 'emt-match:makepattern
   #'emt-match:govs:satisfies)

;;;_   , Tests
(rtest:deftest emt-match:govs:satisfies
   ;;NB, `list' that we're testing this with will return the object as
   ;;the first arg, other args as further args.

   (  "Proves: Whether it succeeds is whether pred succeeds.
Param: Pred `list'
Response: Satisfies it (because list with 1 arg always gives non-nil)."

      (and
	 (emt:match 0
	    (satisfies #'list (a)))))

   (  "Proves: Whether it succeeds is whether pred succeeds.
Param: Pred `ignore'
Response: Fails (because `ignore' always gives nil)."

      (not
	 (emt:match 0
	    (satisfies #'ignore))))
   
   (  "Proves: Uses the ret-patterns.
Object doesn't match the sub-patterns.
Response: Returns nil."
      (not
	 (emt:match 13
	    (satisfies #'list (12)))))
   
   (  "Proves: Uses the ret-patterns.
Object matches the sub-patterns.
Response: Returns non-nil."
      (and
	 (emt:match 12
	    (satisfies #'list (12)))))
   
   ;;Bindings in ret-patterns are available to other objects.
   (  "Proves: Makes bindings in the ret-patterns available to the
rest of the pattern.
Bindings in the sub-patterns are used.
Behavior: Does not error.  The correct values are seen."
      (and
	 (equal
	    (emt:match-let 12
	       (satisfies #'list (a))
	       a)
	    12)))
   

   (  "Proves: The predicate sees the other arguments.
Param: Pred `list'.
Response: Produces the expected result."
      (and
	 (equal
	    (emt:match-let '()
	       (satisfies #'list (obj a b) 12 144)
	       (list a b))
	    '(12 144))))

   (  "Proves: The extra argument are forms, not values.
Param: Pred `list'
Response: Produces the expected result."
      (and
	 (equal
	    (emt:match-let '()
	       (satisfies #'list (obj a b) (+ 6 6) (* 12 12))
	       (list a b))
	    '(12 144))))

   
   (  "Situation: The pred succeeds and returns a non-list.  Specifically, 12.
We make no bindings.
Response: Succeeds."
      (and
	 (emt:match 12
	    (satisfies #'identity))))

   
   ;;NOT provided: Given the binding pattern as something other than a
   ;;list, adapts OK.  This would require normal bindings to be headed
   ;;by `list'

   ;;Does not contemplate letting `pred' be a binding.
   )
;;;_  . emt-match:govs:eval
(defun emt-match:govs:eval (sym pattern &optional other-deps)
   ""
   (let*
      (  (form (cadr pattern))
	 (obj 
	    (if
	       (constantp form)
	       ;;This is not the "real" eval of the form, this just
	       ;;gets a layer of quoting out of the way so we can
	       ;;include it in forms as a sub-form.
	       (eval form)
	       (error "Non-ground forms are not supported")))
	 (sym-2 (gensym)))
      (emt:match:make-formdata
	 :pre-eval-SINGLE
	 (emt:match:make-binding-form-data
	    :bind sym-2
	    :form obj)
	 :form-SINGLE 
	 ;;This may become a different type, knowing sym, sym-2, and uses.
	 (emt:match:make-test-form-data
	    :uses (list* sym other-deps)
	    ;;We use explicit `backquote-unquote-symbol' because it
	    ;;will be recognized by a later `backquote' but not by
	    ;;this one.  `\,' does not work here.  We place `quote'
	    ;;around that so that the result of the pattern-compile
	    ;;eval, bound to sym-2, is not re-evalled at match time.
	    :form
	    `(emt:match-either ,sym '(,backquote-unquote-symbol ,sym-2))
	    ))))
(put 'eval 'emt-match:makepattern
   #'emt-match:govs:eval)

;;;_   , Tests
(rtest:deftest emt-match:govs:eval

   (  "Situation: Arg, when evalled, matches the object.
Response: Succeed."
      (and
	 (emt:match 12
	    (eval '(+ 6 6)))))
   
   (  "Situation: Arg, when evalled, does not match the object.
Response: Fail."
      (not 
	 (emt:match 13
	    (eval '(+ 6 6)))))

   (  "Situation: Arg, unevalled, matches the object, but evalled it
does not.
Response: Fail."
      (not 
	 (emt:match '(+ 6 6)
	    (eval '(+ 6 6)))))

   ;;For now at least.  Later such objects might be bound at match
   ;;time.
   (  "Param: Arg has a non-constant object.
Response: Error."
      (emt:gives-error
	 (let
	    ((a 0))
	    (emt:match 0
	       (eval a)))))
   
   ;;Tests dealing with expansion time

   (  "Situation: Arg, when evalled, matches the object.
Object is a form, so it can be further evalled.
Response: Succeed."
      (and
	 (emt:match '(+ 6 6)
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
		(emt:match:lambda
		   (eval 'a)
		   t))))
	 (let
	    ((a 13))
	    (and
	       (emt:match
		  12
		  (satisfies comparand-f))
	       (not
		  (emt:match
		     13
		     (satisfies comparand-f)))))))
   
   
   )

;;;_ , Build form 
;;;_  . emt:match:build-form-recurse
(defun emt:match:build-form-recurse (formdata-list core)
   ""
   (if
      formdata-list
      (let
	 ((first (car formdata-list))
	    (rest (emt:match:build-form-recurse (cdr formdata-list) core)))
	 (cond
	    ((emt:match:binding-form-data-p first)
	       (if
		  (emt:match:binding-form-data->rebinding-p first)
		  `(if
		      ;;Test equality or pattern-match.  It's the
		      ;;already-bound that's allowed to be a pattern
		      ;;(Maybe both should be)
		      (emt:match-either
			 ,(emt:match:binding-form-data->form first)
			 ,(emt:match:binding-form-data->bind first)
			 ;;The two args should be the same Lisp
			 ;;object, not just equal.
			 #'eql)
		      ,rest
		      nil)
		  `(let ((
			    ,(emt:match:binding-form-data->bind first)
			    ,(emt:match:binding-form-data->form first)))
		      ,rest)))
	    (t
	       `(if
		   ,(emt:match:test-form-data->form first)
		   ,rest
		   nil))))
      core))
;;;_  . emt:match:sort-bindings
(defun emt:match:sort-bindings (been-bound formdata-list)
   "Return a list of bindings, sorted so that it can be applied in order.
May also set `rebinding-p' flag in bindings.

FORMDATA-LIST is the list of form-data to be sorted.
BEEN-BOUND is a list of symbols that have been bound and test-forms
that have been checked."

   (let
      (
	 (pending formdata-list)
	 (held-back ())
	 ;;Reversed list
	 (rv-new-forms ()))
      (while pending
	 (let*
	    ((form (pop pending))
	       (uses-syms
		  (typecase form
		     (emt:match:binding-form-data
			(emt:match:binding-form-data->uses
			   form))
		     (emt:match:test-form-data
			(emt:match:test-form-data->uses
			   form))))
	       (unsatted
		  (set-difference uses-syms been-bound)))
	    (if unsatted
	       ;;If it's unsatisfied, hold it for later.
	       (push (list unsatted form) held-back)

	       ;;Otherwise accept it in this position.
	       (push form rv-new-forms)

	       ;;It may have satisfied some dependencies...
	       (let
		  ((sym 
		      (typecase form
			 (emt:match:binding-form-data
			    (emt:match:binding-form-data->bind
			       form))
			 (emt:match:test-form-data
			    (emt:match:test-form-data->form
			       form))))
		     (new-held-back ()))

		  (when (member sym been-bound)
		     (typecase form
			(emt:match:binding-form-data
			   ;;Flag it a rebinding
			   (setf
			      (emt:match:binding-form-data->rebinding-p form)
			      t))))

		  ;;Remember what it satisfies...
		  (push sym been-bound)

		  ;;,and reconsider everything that was held back.
		  ;;We destroy the `held-back' list and make
		  ;;another one.
		  (while held-back
		     (destructuring-bind (hb-unsatted hb-form)
			(pop held-back)
			;;The new symbol is now satisfied
			(setq hb-unsatted (delete sym hb-unsatted))
			(if hb-unsatted
			   ;;If there are still other dependencies,
			   ;;store it updated.
			   (push (list hb-unsatted hb-form) new-held-back)
			   ;;Otherwise push it onto the list of
			   ;;waiting forms.  Due to double
			   ;;reversing, it'll be seen in the
			   ;;relative order it was added, which
			   ;;tends to be good.
			   (push hb-form pending))))
		     
		  (setq held-back new-held-back)))))
      
      (when held-back
	 (error "Some dependencies could not be satisfied"))

      (nreverse rv-new-forms)))
;;;_   , Tests

(put 'emt:match:sort-bindings 'rtest:test-thru
   'emt:match:build-form)
;;Test: Error if the same form or binding occurs more than once.
;;Easy to implement.

;;;_  . emt:match:build-form
(defun emt:match:build-form (sym-list all-formdata core)
   ""
   
   (let*
      ((formdata-list
	  (emt:match:formdata->form-LIST all-formdata))
	 (sorted-list
	    (emt:match:sort-bindings sym-list formdata-list))
	 (pre-eval-list
	    (emt:match:formdata->pre-eval-LIST all-formdata))
	 ;;Pre-evals don't need sorting because all they do is bind
	 ;;gensyms that are only shared with match-time forms.
	 ;;(Get bindings in pre-eval-list)
	 (form-wo-pre-eval
	    (emt:match:build-form-recurse sorted-list core)))
      
      (if
	 pre-eval-list
	 ;;Eval because some sub-forms need to be evalled at
	 ;;pattern-compile time.  Such sub-forms are made by the
	 ;;`eval' governor.  A `backquote' protects `form-wo-pre-eval'
	 ;;from this eval, except the parts of it that are explicitly
	 ;;unquoted.
	 (eval
	    (emt:match:build-form-recurse
	       pre-eval-list
	       ;;This, the core form, is quoted. The inner, explicit
	       ;;`backquote' doesn't act until we get back to `eval',
	       ;;above.
	       `(backquote ,form-wo-pre-eval)))
	 ;;This branch is just a special case of the other.
	 form-wo-pre-eval)))

;;;_   , Tests
(rtest:deftest emt:match:build-form

   (  "Param: formdata list is an empty list.
Response: Form acts as just core."
      (equal
	 (eval
	    (emt:match:build-form 
	       '(x) 
	       (emt:match:make-formdata :form-LIST ())
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
	       (emt:match:build-form 
		  '(x) 
		  (emt:match:make-formdata 
		     :form-LIST 
		     (list
			(emt:match:make-binding-form-data
			   :bind 'z
			   :uses '(y)
			   :form 'y)
			(emt:match:make-binding-form-data
			   :bind 'y
			   :uses '(x)
			   :form 'x)
			(emt:match:make-binding-form-data
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
	       (emt:match:build-form 
		  '(x) 
		  (emt:match:make-formdata 
		     :form-LIST 
		     (list
			(emt:match:make-test-form-data
			   :uses (list form-y)
			   :form form-z)
			(emt:match:make-test-form-data
			   :uses '(x)
			   :form form-y)
			(emt:match:make-test-form-data
			   :uses (list form-z)
			   :form form-w)))
		  ''ok)))
	 'ok))

   )
;;;_  . emt:match:build-form--1 
;;These might be renamed, this to `emt:match:build-form' and the
;;original to *-x.
(defun emt:match:build-form--1 (sym pattern core-form)
   "Build a form and return it.
The factored-out part of emt:match and emt:match:lambda"
   
   (let*
      (
	 ;;Decompose the pattern.
	 (formdata (emt-match:parse-pattern sym pattern))
	 ;;Build the inner part of the form
	 (form (emt:match:build-form (list sym) formdata core-form)))
      form))
;;;_   , Tests
(put 'emt:match:build-form--1 'rtest:test-thru
   'emt:match)

;;;_ , Entry points
;;;_  . Test-supporter emt:match:th:single-gov
(defun emt:match:ts:single-gov (func pattern obj)

   "Match OBJ vs the pattern PATTERN, forcing FUNC to be the governor
function when parsing PATTERN.  PATTERN should be quoted and not
contain the symbol `testhelp-483s'.

Intended for testing governor functions in isolation."

   ;;For this, punt: Always use `testhelp-483s' as the symbol.
   (let
      ((testhelp-483s obj))
      (eval
	 (emt:match:build-form 
	    '(testhelp-483s) 
	    (funcall func 'testhelp-483s pattern) 
	    't))))

;;;_   , Tests

;;This is to validate that its results agree with known results, so
;;it's OK to use this for testing governor functions in isolation.
(rtest:deftest emt:match:ts:single-gov

   ("Situation: Function is `emt-match:govs:literal'
Object matches the literal pattern.
Response: Gives non-nil."
      (and (emt:match:ts:single-gov #'emt-match:govs:literal 12 12)))

   ("Situation: Function is `emt-match:govs:literal'
Object mismatches the literal pattern.
Response: Gives nil."
      (not (emt:match:ts:single-gov #'emt-match:govs:literal 12 13)))

   ("Situation: Function is `emt-match:govs:symbol'.
Object of course matches the symbol pattern.
Response: gives non-nil."
      (emt:match:ts:single-gov #'emt-match:govs:symbol 'a 12))    
  
   )
;;;_  . emt:match
;;Does this order of arguments make sense?  Or should it be vv?
(defmacro emt:match (object-form pattern)
   ""
   (let
      ((sym (gensym)))
      `(let
	 ((,sym ,object-form))
	  ,(emt:match:build-form--1 sym pattern 't))))

;;;_   , Tests
(rtest:deftest emt:match

   ("Situation: A literal pattern matches.
Response: Gives non-nil."

      (emt:match 12 12))

   ("Situation: A literal pattern matches.
That pattern is a quoted list.
Response: Gives non-nil."

      (and (emt:match '(12) '(12))))

   ("Situation: A literal pattern mismatches.
Response: Gives nil."

      (not (emt:match 13 12)))

   ("Situation: A symbol
Response: Gives non-nil."

      (and (emt:match 12 a)))

   ;;Governed lists.  Data should be from eg examples.
   
   (  "Situation: Pattern is a governed list
Response: Works."
      (and
	 (emt:match
	    '(12)
	    (list 12)))
      )

   (  "Situation: Pattern is a governed list
Response: Works."
      (not
	 (emt:match
	    '(13)
	    (list 12)))
      )


   '
   ("Situation: Governed by `is-a'.
Response: Gives non-nil."

      (and (emt:match 12 (is-a 'integer))))
   '
   ("Situation: Governed by `equal'.  
Both pattern arguments match the object.
Response: Gives non-nil."

      (and (emt:match 12 (equal a 12))))
      
   '
   ("Situation: Governed by `equal'.  
Only one of the pattern arguments matches the object.
Response: Gives non-nil."

      (and (emt:match 12 (equal a 13))))

   ;;Maybe re-order this to test the builder instead.
   ;;Binding multiple things
   (  "Situation: Pattern has the same variable occuring twice
Response: Works."
      (and
	 (emt:match
	    '(12 12)
	    (list a a)))
      )
   (  "Situation: Pattern has the same variable occuring twice
Response: Works."
      (and
	 (emt:match
	    '(144 144)
	    (list a a)))
      )
   (  "Situation: Pattern has the same variable occuring twice
Response: Works."
      (not
	 (emt:match
	    '(12 13)
	    (list a a))))

   (  "Situation: Pattern is made by a general lambda, 1 extra arg.
Then it's instantiated, with the extra binding being an object.
Behavior: `a' must be equal to that object.
Response: Works."
      (let* 
	 ((pat-f
	     (emt:match:make-general-lambda 
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
	     (emt:match:make-general-lambda 
		;;Pattern
		(list a)
		;;Bindings
		(a)
		;;Body
		t
		))
	    ;;Boxed pattern 1
	    (boxed-pat-1 (emt:match:make-pattern 12))
	    ;;Boxed pattern 2
	    (boxed-pat-2 (emt:match:make-pattern 144)))
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

      (not (emt:match '(12 12) '(13 13))))
   )

'
(emt:deftest-2 emt:match
   ;;Literals
   ("Situation: A literal pattern matches.
Response: Gives non-nil."
      (should 
	 (emt:match 12 12)))

   ("Situation: A literal pattern mismatches.
Response: Gives nil."
      (should
	 (not (emt:match 13 12))))

   )
;;;_  . emt:match-f
(defun emt:match-f (obj pattern)
   "Non-nil if OBJ matches PATTERN.

PATTERN must be a boxed pattern object (Use `emt:match:make-pattern')."
   (unless (emt:match:box-p pattern)
      (error "PATTERN needs to be an emt-match pattern object"))

   (let*
      ((func (emt:match:box->func pattern)))
      (funcall func obj)))

;;;_   , Tests
(put 'emt:match-f 'rtest:test-thru
   'emt:match:make-pattern)
;;;_  . emt:match-either
;;This must use `equal' for when it's used by the `eval' governor.
(defun* emt:match-either (obj1 obj2 &optional (cmp-f #'equal))
   "Non-nil if either:
 * OBJ1 and OBJ2 are equal, or
 * OBJ2 is a pattern-box and OBJ1 matches it."
   (if
      (emt:match:box-p obj2)
      (emt:match-f obj1 obj2 )
      (funcall cmp-f obj1 obj2)))

;;;_   , Tests
(put 'emt:match-either 'rtest:test-thru
   'emt:match:make-pattern)
;;;_  . emt:match:make-pattern
(defmacro emt:match:make-pattern (pattern)
   "Makes a boxed pattern object according to PATTERN."
   `(emt:match:make-box
       :func (emt:match:lambda ,pattern t)
       ;;For now, we know nothing about type.  Later, we'll query the
       ;;head, if any.
       :type t))
;;;_   , Tests
(rtest:deftest emt:match:make-pattern

   (  "Proves: `emt:match-f' and `emt:match:make-pattern' work together.
Situation: A pattern-object is cted.
emt:match-f param: That pattern-object.
Response: Pass/fail result is as expected."
      (let ((pat (emt:match:make-pattern 12)))
	 (assert (emt:match-f 12 pat))
	 (assert (not (emt:match-f 13 pat)))
	 t))

   (  "Proves: The boxed pattern can be used in other patterns.
Situation: A pattern-object is cted.
It is used as part of another pattern.
Response: Pass/fail result is as expected."
      (let* ((pat (emt:match:make-pattern 12))
	       (func
		  (emt:match:lambda
		     (list (eval 'pat) 144)
		     t)))
	 (assert (funcall func '(12 144)))
	 (assert (not (funcall func '(13 144))))
	 t)))

;;;_  . emt:match:make-general-lambda
;;Rewrite the others in terms of this.
(defmacro emt:match:make-general-lambda (pattern bindings &rest body)
   "Make a pattern-match function that can take extra args.
First argument is the object to be matched by PATTERN.
BINDINGS is a list of symbols.  They will be available inside
PATTERN and BODY, bound to whatever the extra args are bound to.
BODY is a form body."
   (let
      ((sym (gensym)))
      `
      (lambda (,sym ,@bindings)
	 ,(emt:match:build-form 
	     (list* sym bindings)
	     (emt-match:parse-pattern sym pattern) 
	      `(progn ,@body)))))

;;;_  . emt:match:lambda
(defmacro emt:match:lambda (pattern &rest body)
   "BODY is a form body."
   (let
      ((sym (gensym)))
      `(lambda (,sym)
	  ,(emt:match:build-form--1 sym pattern `(progn ,@body)))))

;;;_   , Tests
(rtest:deftest emt:match:lambda

   (  "Situation: Body returns `a'
Behavior: Returns the expected success.
When successful, returns the expected value.
When unsuccessful, returns `nil'"
      (let
	 ((func 
	     (emt:match:lambda
		(list 12 a)
		(list a 1728))))
	 (and
	    (funcall func '(12 144))
	    ;;The returned value should be as expected.
	    (equal
	       (funcall func '(12 144))
	       '(144 1728))
	    (not (funcall func '(13 13)))))))


;;;_  . emt:match:lambda-binds

(defmacro emt:match:lambda-binds (pattern &rest bindings)
   ""
   
   ;;Check `bindings' as a list of symbols.
   `(emt:match:lambda 
       ,pattern
       ,(if bindings
	   `(list ,@bindings)
	   't)))

;;;_   , Tests
(rtest:deftest emt:match:lambda-binds

   (  "Situation: No bindings
Behavior: Returns the expected success."
      (let
	 ((func 
	     (emt:match:lambda-binds 12)))
	 (and
	    (funcall func 12)
	    (not (funcall func 13)))))
   
   
   (  "Situation: Some bindings.
Specifically, one binding.
Behavior: Returns the expected success.
When successful, returns the expected list of values."
      (let
	 ((func 
	     (emt:match:lambda-binds
		(list 12 a)
		a)))
	 (and
	    (funcall func '(12 144))
	    ;;The returned value should be as expected.
	    (equal
	       (funcall func '(12 144))
	       '(144))
	    (not (funcall func '(13 13)))))))

;;;_  . emt:match-let
(defmacro emt:match-let (object-form pattern &rest body)
   ""
   (let*
      ((sym (gensym))
	 ;;Decompose the pattern.
	 (formdata (emt-match:parse-pattern sym pattern))
	 ;;Build the inner part of the form
	 (form (emt:match:build-form 
		  (list sym) 
		  formdata 
		  `(progn ,@body))))
      `(let
	 ((,sym ,object-form))
	  ,form)))
;;;_   , Tests
(rtest:deftest emt:match-let

   (  "Proves: The body's return value is returned.
Situation: Trivial PATTERN matches OBJECT-FORM
Response: The body's return value is returned."
      (equal
	 (emt:match-let 0 0
	    12)
	 12))

   (  "Proves: The bindings are available in BODY.
Situation: Trivial pattern matches object.
Response: The body's return value is returned."
      (equal
	 (emt:match-let '(12 144) (list a b)
	    (list b a))
	 (list 144 12)))
   
   ;;Behavior on mis-match ... this is TBD, see docs.
   )

;;;_  . emt:match-case
;;Not implemented.
;;;_ , Type support
(deftype emt:match:pattern (x)
   'emt:match:box)
;;;_  . Tests
(rtest:deftest emt:match:pattern

   (  "Proves: We can test for type.
Response: typep's pass/fail is as expected."
      (let ((pat (emt:match:make-pattern 12)))
	 (assert (typep pat '(emt:match:pattern *)))
	 (assert (not (typep 13 '(emt:match:pattern *))))
	 t))
   ;;We punt more exact type checks for now.
   ;;For `repeat' 
   ;;For `list'.
   ;;For where we have defined the underlying type and a pattern-ctor.

   )

;;;_ , Functions to help others build

;;;_  . Making structure governors
;;;_   , emt-match:make-struct-governor-x

(defun emt-match:make-struct-governor-x (sym depth accessor)
   "Return value is a list of two forms:
 * First form is a form.
 * Second form is a formdata.
DEPTH is a pattern.
ACCESSOR is a structure accessor."
   (let*
      (		
	 (depth-sym (gensym))
	 ;;Get the accessor.
	 ;;Make a binding that accesses field
	 (bind-field-depth
	    (emt:match:make-binding-form-data
	       :uses 
	       (emt:match:parse-dependencies
		  (list sym) (list check-type))
	       :bind depth-sym
	       :form `(,accessor ,sym)))
	 ;;Match the given `depth' pattern to that object.
	 (child-formdata
	    (emt-match:parse-pattern
	       depth-sym
	       depth)))

      (list bind-field-depth child-formdata)))
;;;_   , emt-match:make-typecheck-form
(defun emt-match:make-typecheck-form (sym pred)
   "Make a form to check the type of SYM.
SYM is the symbol to be checked.
PRED is a function-quoted predicate to apply to it."
   (emt:match:make-test-form-data
      :explanation "Object is the wrong type"
      :uses (list sym)
      :form `(,pred ,sym)))
;;;_   , Helpers

(defsubst emt-match:util:keysym->sv-keysym (keysym)
   ""
   (intern (concat "sv-" (symbol-name keysym))))

(defsubst emt-match:util:keysym->accessor-sym (conc-name keysym)
   ""
   (intern (concat (symbol-name conc-name) (symbol-name keysym))))

;;;_   , emt-match:make-struct-governor

;;We make a lambda, which functions can call internally, or which we'd
;;set the symbol function to.  So don't take name here.

;;It is a pattern governor function, not a ctor.  If included in a
;;pattern, requires that the object match the explicit fields of the
;;would-be object.  It ignores the implicit fields (they are not even
;;`nil', they are just unspecified).  To make an instance, include it
;;in emt:match:lambda.

;;Allowed args are the same as a default ctor.  However, they are
;;patterns, so they can be literals, bindings, and anything else
;;emt-match supports.


;;In emacs 21 at least, nested backquotes do not work.  Unquote
;;operators operate in the smallest enclosing backquoted scope, not
;;the largest, and after seeing a nested backquote, parser no longer
;;recognizes ,@.  So as a workaround I've moved all the nested
;;backquote work out to helper functions.
(defmacro emt-match:make-struct-governor (pred conc-name keys)
   "Build a lambda that is a structure governor function.
PRED is a symbol naming a predicate that tests whether an object is
this type.
CONC-NAME is a symbol naming the conc-name defined for the structure.
KEYS is a list of all field-names."
   (let
      (
	 (key-keylist 
	    (mapcar
	       #'(lambda (key)
		    ;;A single keyword arg definition, like (x () sv-x)
		    `(,key () ,(emt-match:util:keysym->sv-keysym key)))
	       keys))
	 (key-datalist
 	    (mapcar
 	       #'(lambda (key)
		    ;;A form making data that applies to a single
		    ;;field, like (if sv-x (list x #'foo->x) ())
 		    `(if ,(emt-match:util:keysym->sv-keysym key)
 			(list
 			   ,key
			   #',(emt-match:util:keysym->accessor-sym
				 conc-name key))
 			()))
 	       keys)))
      

      `(lambda (sym pattern &optional other-deps)
	  ""
   
	  (destructuring-bind (&key ,@key-keylist)
	     (cdr pattern)

	     (let* 
		(
		   ;;Object must be of the given type.
		   (check-type
		      (emt-match:make-typecheck-form sym #',pred))

		   ;;Each element of this list is the data for one
		   ;;given field: Its respective pattern and its
		   ;;accessor.
		   (data-list
		      (remq nil
			 (list ,@key-datalist)))
		   
		   ;;data-list, transformed into bindings.  These are
		   ;;returned in pairs because they share symbols etc
		   ;;separate from the other fields.  first is a
		   ;;bind-form, second is formdata
		   (formpair-list
		      (mapcar
			 #'(lambda (cell)
			      (emt-match:make-struct-governor-x 
				 sym 
				 (first cell)
				 (second cell)))
			 data-list)))
	     
		(emt:accumulator:list->object
		   (append
		      (list 
			 (emt:match:make-formdata
			    :form-LIST 
			    (append
			       (list check-type)
			       (mapcar #'first formpair-list))))
		      (mapcar #'second formpair-list))
		   'emt:match:formdata))))))

;;;_   , emt-match:define-struct-governor-oldstyle
(defmacro emt-match:define-struct-governor-oldstyle (gov-name pred-name
					      conc-name fields)
   "Define a pattern-ctor as match-governor GOV-NAME.
PRED-NAME must be the name of the predicate that tests it.
CONC-NAME must be the structure's conc-name."
   ;;No type info yet.
   `(put ',gov-name 'emt-match:makepattern
      (emt-match:make-struct-governor 
	 ,pred-name
	 ,conc-name
	 ,fields)))
;;;_   , emt-match:define-struct-governor-x
;;$$FIXME Should take `name' as parameter
;;The defaults might be moved out to caller, since all clients should
;;have the same defaults. 
(defun* emt-match:define-struct-governor-x 
   (
      (&key
	 (predicate (intern (concat (symbol-name name) "-p"))) 
	 (constructor (intern (concat "make-" (symbol-name name))))
	 (conc-name (intern (concat (symbol-name name) "-"))) 
	 &allow-other-keys)
      fields)
   "Make the form."
   `(put ',constructor 'emt-match:makepattern
       (emt-match:make-struct-governor 
	  ,predicate
	  ,conc-name
	  ,fields)))
;;;_   , emt-match:define-struct-governor 
(defmacro emt-match:define-struct-governor (name+args &rest fields)
   "Define a structure governor function.  
emt-match will see the definition and use it appropriately.
Takes essentially a defstruct definition, but don't provide a docstring."
   ;;Change defstruct-like syntax into an easier-to-handle form.
   (let*
      ((name (if (listp name+args) (car name+args) name+args))
	 (struct-options 
	    (if (listp name+args)
	       (apply #'append 
		  ;;For cases where options take other than 2 args.
		  (mapcar
		     #'(lambda (x)
			  (list (car x) (second x)))
		     (cdr name+args))) 
	       '())))

      ;;Call a lower worker.
      (emt-match:define-struct-governor-x struct-options fields)))


;;;_    . Tests
(put 'emt-match:define-struct-governor* 'rtest:test-thru
   'rtest-struct:gov-high-level)

;;;_   , Test strategy

;;Test this on the usual test structure type `rtest-struct'.  In
;;rtest-tools.

;;We want to use the same tests (and more) on the general function
;;that worked for the particular one.

;;Could rebuild them with the same data.  But without emtest yet, then
;;it's tough to see what parts failed.  For now, I am just
;;rebuilding the function both ways, to test it.


;;;_    . Test data

;;For now, no examples are defined, we just repeat all the data for
;;every test.

;;The structure definitions, and some aliases to be conformant.
(require 'rtest-tools)
(defalias 'rtest-struct->my-field 'rtest-struct-my-field)
(defalias 'rtest-struct->my-second-field 'rtest-struct-my-second-field)
(defalias 'rtest-struct-make-item 'make-rtest-struct)

;;;_     , Setup

'  ;;The old way, which was incomplete.
(setf (symbol-function 'rtest-struct:gov-high-level)
   (emt-match:make-struct-governor 
      rtest-struct-p 
      rtest-struct-> 
      (my-field my-second-field)))

;;Assign it to a governor.
'  ;;The medium-old way
(put 'rtest-struct-high-gov 'emt-match:makepattern
   (emt-match:make-struct-governor 
      rtest-struct-p 
      rtest-struct-> 
      (my-field my-second-field)))

;;The slightly old way
'
(emt-match:define-struct-governor-oldstyle
   rtest-struct-high-gov
   rtest-struct-p 
   rtest-struct-> 
   (my-field my-second-field))


(emt-match:define-struct-governor
    (rtest-struct
       (:constructor rtest-struct-high-gov)
       (:predicate rtest-struct-p)
       (:conc-name rtest-struct->))
    
    my-field 
    my-second-field)

;;;_   , Tests

(put 'emt-match:make-struct-governor 'rtest:test-thru
   'rtest-struct:gov-high-level)

(rtest:deftest rtest-struct:gov-high-level

   ;;Proves: Object type is checked
   (  "Situation: Wrong type of object is given.
Response: Match fails."
      (not
	 (emt:match
	    (list 13)
	    (rtest-struct-high-gov :my-field 12))))

   ;;Proves: A field is matched on, unless not given.
   (  "Situation: My-Field field is given, matches.
Response: Match succeeds."
      (emt:match
	 (rtest-struct-make-item :my-field 12)
	 (rtest-struct-high-gov :my-field 12)))
   
   (  "Situation: My-Field field is given, mismatches.
Response: Match fails."
      (not
	 (emt:match
	    (rtest-struct-make-item :my-field 13)
	    (rtest-struct-high-gov :my-field 12))))

   (  "Situation: My-Field field is not given.
Response: Match succeeds."
      (emt:match
	 (rtest-struct-make-item :my-field 12)
	 (rtest-struct-high-gov)))



   ;;Group: `emt:match-lamba' version can be used as lambda arg to
   ;;`satisfies'.  These tests repeat the earlier data, except the
   ;;comparand (Here it's a function, not an object)
   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is given, matches.
Response: Match succeeds."
      (let
	 ;;`comparand-f' is a variable, not a function, so don't
	 ;;function-quote it.
	 ((comparand-f
	     (emt:match:lambda
		(rtest-struct-high-gov :my-field 12)
		t)))
	 (emt:match
	    (rtest-struct-make-item :my-field 12)
	    (satisfies comparand-f))))

   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is given, mismatches.
Response: Match fails."
      (not
	 (let
	    ((comparand-f
		(emt:match:lambda
		   (rtest-struct-high-gov :my-field 12)
		   t)))
	    (emt:match
	       (rtest-struct-make-item :my-field 13)
	       (satisfies comparand-f)))))

   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is not given.
Response: Match succeeds."
      (let
	 ((comparand-f
	     (emt:match:lambda
		(rtest-struct-high-gov)
		t)))
	 (emt:match
	    (rtest-struct-make-item :my-field 12)
	    (satisfies comparand-f))))

   ;;Proves: Works with other fields

   (  "Situation: my-second-field is given, mismatches.
Response: Match fails."
      (not
	 (emt:match
	    (rtest-struct-make-item :my-second-field 13)
	    (rtest-struct-high-gov :my-second-field 12))))

   ;;Proves: Works with multiple fields

   (  "Situation: Both fields are given, match.
Response: Match succeeds."
      (emt:match
	 (rtest-struct-make-item :my-field 12 :my-second-field 144)
	 (rtest-struct-high-gov :my-field 12 :my-second-field 144)))

   (  "Proves: pattern-ctor works with `emt:match-f' and
`emt:match:make-pattern'. 
Situation: A pattern-object is cted.
emt:match-f param: That pattern-object.
emt:match-f param: An object of that type.
Response: Pass/fail result is as expected."
      (let ((pat 
	       (emt:match:make-pattern 
		  (rtest-struct-high-gov :my-field 12))))
	 (assert (emt:match-f 
		    (rtest-struct-make-item :my-field 12) 
		    pat))
	 (assert (not 
		    (emt:match-f
		       (rtest-struct-make-item :my-field 13) 
		       pat)))
	 t))

   )

;;;_   , Literal version, to validate against

(defun rtest-struct:gov-literal (sym pattern &optional other-deps)
   ""

   (destructuring-bind (&key (my-field () sv-my-field))
      (cdr pattern)

      (let* 
	 (
	    ;;Object must be of type rtest-struct.
	    (check-type
	       (emt:match:make-test-form-data
		  :explanation "Object is the wrong type"
		  :uses (list sym)
		  :form `(rtest-struct-p ,sym)))

	    (my-field-sym (gensym))
	    ;;Get the my-field accessor `rtest-struct->my-field'
	    ;;Make a binding that accesses field
	    (bind-field-my-field
	       (emt:match:make-binding-form-data
		  :uses 
		  (emt:match:parse-dependencies
		     (list sym) (list check-type))
		  :bind my-field-sym
		  :form `(rtest-struct->my-field ,sym)))
	    ;;Match the given `my-field' pattern to that object.
	    (child-formdata
	       (emt-match:parse-pattern
		  my-field-sym
		  my-field)))
	 
	 (emt:accumulator:list->object
	    (append
	       (list 
		  (emt:match:make-formdata
		     :form-LIST 
		     (list check-type)))
	       
	       (if sv-my-field
		  (list
		     (emt:match:make-formdata
			:form-LIST 
			(list bind-field-my-field))
		     child-formdata)
		  ()))
	    'emt:match:formdata))))
(put 'rtest-struct-literal-gov 'emt-match:makepattern
   #'rtest-struct:gov-literal)

;;;_    . Tests (of literal version)

(rtest:deftest rtest-struct:gov-literal
   ;;Proves: My-Field fields are matched on, unless not given.
   (  "Situation: My-Field field is given, matches.
Response: Match succeeds."
      (emt:match
	 (rtest-struct-make-item :my-field 12)
	 (rtest-struct-literal-gov :my-field 12)))
   
   (  "Situation: My-Field field is given, mismatches.
Response: Match fails."
      (not
	 (emt:match
	    (rtest-struct-make-item :my-field 13)
	    (rtest-struct-literal-gov :my-field 12))))

   (  "Situation: My-Field field is not given.
Response: Match succeeds."
      (emt:match
	 (rtest-struct-make-item :my-field 12)
	 (rtest-struct-literal-gov)))
   
   (  "Situation: Wrong type of object is given.
Response: Match fails."
      (not
	 (emt:match
	    (list 13)
	    (rtest-struct-literal-gov :my-field 12))))


   ;;Group: `emt:match-lamba' version can be used as lambda arg to
   ;;`satisfies'.  These tests repeat the earlier data, except the
   ;;comparand (Here it's a function, not an object)
   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is given, matches.
Response: Match succeeds."
      (let
	 ;;`comparand-f' is a variable, not a function, so don't
	 ;;function-quote it.
	 ((comparand-f
	     (emt:match:lambda
		(rtest-struct-literal-gov :my-field 12)
		t)))
	 (emt:match
	    (rtest-struct-make-item :my-field 12)
	    (satisfies comparand-f))))

   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is given, mismatches.
Response: Match fails."
      (not
	 (let
	    ((comparand-f
		(emt:match:lambda
		   (rtest-struct-literal-gov :my-field 12)
		   t)))
	    (emt:match
	       (rtest-struct-make-item :my-field 13)
	       (satisfies comparand-f)))))

   (  "Situation: lambda-ized and used as lambda arg to `satisfies'.
My-Field field is not given.
Response: Match succeeds."
      (let
	 ((comparand-f
	     (emt:match:lambda
		(rtest-struct-literal-gov)
		t)))
	 (emt:match
	    (rtest-struct-make-item :my-field 12)
	    (satisfies comparand-f))))


   (  "Proves: pattern-ctor works with `emt:match-f' and
`emt:match:make-pattern'. 
Situation: A pattern-object is cted.
emt:match-f param: That pattern-object.
emt:match-f param: An object of that type.
Response: Pass/fail result is as expected."
      (let ((pat 
	       (emt:match:make-pattern 
		  (rtest-struct-literal-gov :my-field 12))))
	 (assert (emt:match-f 
		    (rtest-struct-make-item :my-field 12) 
		    pat))
	 (assert (not 
		    (emt:match-f
		       (rtest-struct-make-item :my-field 13) 
		       pat)))
	 t))
   )




;;;_. Footers
;;;_ , Provides

(provide 'emt-match)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emt-match.el ends here
