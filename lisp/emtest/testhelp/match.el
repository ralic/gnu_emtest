;;;_ emtest/testhelp/match.el --- Expression matcher for EMtest

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
(require 'emtest/testhelp/tagnames)
(eval-when-compile
   (require 'cl))

;;`tehom-cl' needed for `constantp'
;;$$MOVE ME into utilities
(require 'tehom-cl)
(require 'emtest/testhelp/standard)
(require 'utility/accumulator)

;;;_. Body
;;;_ , Structures
;;;_  . Accumulator definition
(utiacc:define 
   (emtm:formdata
      (:constructor emtm:make-formdata)
      (:conc-name emtm:formdata->))
   "The type that a pattern governor returns"
   form
   pre-eval
   )

;;;_  . Binding-form data
(defstruct (emtm:binding-form-data
	      (:constructor emtm:make-binding-form-data)
	      (:conc-name emtm:binding-form-data->))
   ""
   (uses ())
   (bind nil :type symbol)
   (rebinding-p nil :type bool)
   form)
;;;_  . Test-form data
(defstruct (emtm:test-form-data
	      (:constructor emtm:make-test-form-data)
	      (:conc-name emtm:test-form-data->))
   ""
   ;;A form that, if non-nil, makes a string explaining the mismatch
   explanation
   (uses ())
   form)
;;;_  . Box
(defstruct (emtm:box
	      (:constructor emtm:make-box)
	      (:conc-name emtm:box->))
   "A pattern, represented as an object"
   func
   type)

;;;_ , Utilities
;;;_  . emtm:proper-list-p
;;Borrowed from `format'.
(defun emtm:proper-list-p (list)
  "Return t if LIST is a proper list.
A proper list is a list ending with a nil cdr, not with an atom "
  (when (listp list)
    (while (consp list)
      (setq list (cdr list)))
    (null list)))

;;;_  . emtm:parse-dependencies
(defsubst emtm:parse-dependencies (binds &optional tests)
   "Parse the dependencies, for feeding the `uses' field.
BINDS must be a list of symbols.
TESTS, if given, must be a list of emtm:test-form-data."
   (append
      binds
      (mapcar
	 #'emtm:test-form-data->form
	 tests)))


;;;_ , Parse pattern  emtm:parse-pattern
(defun emtm:parse-pattern (sym pattern &optional other-deps)
   ""

   ;;Mutually recurses with most governors' functions.
   (let
      ;;Get the function.
      ((func
	  (cond
	     ((constantp pattern)
		#'emtm:govs:literal)
	     ((symbolp pattern)
		#'emtm:govs:symbol)
	     ((vectorp pattern)
		;;Punt for now.  Function is essentially vector
		;;function, but must destructure the pattern first.
		)
	     ((consp pattern)
		(emtm:gov->function (car pattern)))
	     (t nil))))

      ;;Use the function we got.
      (if func
	 (funcall func sym pattern other-deps)
	 (error "Could not find a governor function"))))

;;;_ , Map governors to functions

(defun emtm:gov->function (gov-sym)
   "Return the respective pattern function of GOV-SYM's governor"

   (get gov-sym 'emtm:makepattern))


;;;_ , Basic governors' functions
;;;_  . emtm:govs:literal Pseudo-governor for literals
(defun emtm:govs:literal (sym pattern &optional other-deps)
   ""
   (emtm:make-formdata
      :form-SINGLE 
      (emtm:make-test-form-data
	 :uses (list* sym other-deps)
	 :form `(equal ,sym ,pattern))))

;;;_  . emtm:govs:symbol Pseudo-governor for symbols
;;Matches anything, binds something.
(defun emtm:govs:symbol (sym pattern &optional other-deps)
   ""
   (emtm:make-formdata
      :form-SINGLE 
      (emtm:make-binding-form-data
	 :bind pattern
	 :form sym)))

;;;_  . emtm:govs:list Governor for `list'

(defun emtm:govs:list (sym pattern &optional other-deps)
   "
PATTERN is headed by governor"

   ;;Guarantee that `pattern' is a proper list.
   (unless
      (emtm:proper-list-p pattern)
      (error "Argument to `emtm:govs:list' must be a list"))
   
   (let* 
      (
	 (pattern-els (cdr pattern))
	 ;;List length
	 (len (length pattern-els))

	 ;;Object must be a proper list
	 (formdata-test-listness
	    (emtm:make-test-form-data
	       :explanation "Object is not a proper list"
	       :uses (list* sym other-deps)
	       :form `(emtm:proper-list-p ,sym)))
	 
	 ;;Object must be the expected length
	 (formdata-test-length
	    (emtm:make-test-form-data
	       :explanation "Object is not the right length"
	       :uses (emtm:parse-dependencies 
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
	       (emtm:make-binding-form-data
		  :uses 
		  (emtm:parse-dependencies
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
	       #'emtm:parse-pattern
	       sym-list
	       pattern-els)))

      ;;Result: All the forms we just made.
      (utiacc:list->object
	 (list*
	    (emtm:make-formdata
	       :form-LIST 
	       (list*
		  formdata-test-listness
		  formdata-test-length
		  binding-forms))
	    child-formdata-list)
	 'emtm:formdata)))
(put 'list 'emtm:makepattern
   #'emtm:govs:list)


;;;_   , Test examples

(defconst emtm:govs:thd:examples
   (emtg:define+ ;;xmp:3be9cd9e-25f7-4f9d-9304-8afc1680d17a
      ((project emtest)
	 (library emtm)
	 (section emtm:govs:list))

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

   
      ))

;;;_  . (DORMANT) emtm:govs:set Governor for sets (unordered lists)
'
(defun emtm:govs:set (sym pattern &optional other-deps)
   ""
   ;;This will largely share code with emtm:govs:list

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
(defun emtm:runtime:set (list1 list2)
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

;;;_  . emtm:govs:satisfies
(defun emtm:govs:satisfies (sym pattern &optional other-deps)
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
	       (emtm:make-binding-form-data
		  :uses (list* sym other-deps)
		  :bind ret-sym
		  :form `(funcall ,pred ,sym ,@args)))
	    ;;Test the return value to determine satisfaction.
	    (testret-formadata
	       (emtm:make-test-form-data
		  :explanation "Call to predicate failed"
		  :uses (list ret-sym)
		  ;;`identity' is used solely to distinguish this form
		  ;;from a mere binding, wrt dependencies.
		  :form `(identity ,ret-sym))))

	 (utiacc:list->object
	    (append
	       (list
		  (emtm:make-formdata
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
		     (emtm:govs:list ret-sym 
			(cons 'list ret-patterns)))
		  ()))
	    'emtm:formdata))))

(put 'satisfies 'emtm:makepattern
   #'emtm:govs:satisfies)

;;;_  . emtm:govs:eval
(defun emtm:govs:eval (sym pattern &optional other-deps)
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
      (emtm:make-formdata
	 :pre-eval-SINGLE
	 (emtm:make-binding-form-data
	    :bind sym-2
	    :form obj)
	 :form-SINGLE 
	 ;;This may become a different type, knowing sym, sym-2, and uses.
	 (emtm:make-test-form-data
	    :uses (list* sym other-deps)
	    ;;We use explicit `backquote-unquote-symbol' because it
	    ;;will be recognized by a later `backquote' but not by
	    ;;this one.  `\,' does not work here.  We place `quote'
	    ;;around that so that the result of the pattern-compile
	    ;;eval, bound to sym-2, is not re-evalled at match time.
	    :form
	    `(emtm-either ,sym '(,backquote-unquote-symbol ,sym-2))
	    ))))
(put 'eval 'emtm:makepattern
   #'emtm:govs:eval)

;;;_ , Build form 
;;;_  . emtm:build-form-recurse
(defun emtm:build-form-recurse (formdata-list core)
   ""
   (if
      formdata-list
      (let
	 ((first (car formdata-list))
	    (rest (emtm:build-form-recurse (cdr formdata-list) core)))
	 (cond
	    ((emtm:binding-form-data-p first)
	       (if
		  (emtm:binding-form-data->rebinding-p first)
		  `(if
		      ;;Test equality or pattern-match.  It's the
		      ;;already-bound that's allowed to be a pattern
		      ;;(Maybe both should be)
		      (emtm-either
			 ,(emtm:binding-form-data->form first)
			 ,(emtm:binding-form-data->bind first)
			 ;;The two args should be the same Lisp
			 ;;object, not just equal.
			 #'eql)
		      ,rest
		      nil)
		  `(let ((
			    ,(emtm:binding-form-data->bind first)
			    ,(emtm:binding-form-data->form first)))
		      ,rest)))
	    (t
	       `(if
		   ,(emtm:test-form-data->form first)
		   ,rest
		   nil))))
      core))
;;;_  . emtm:sort-bindings
(defun emtm:sort-bindings (been-bound formdata-list)
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
		     (emtm:binding-form-data
			(emtm:binding-form-data->uses
			   form))
		     (emtm:test-form-data
			(emtm:test-form-data->uses
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
			 (emtm:binding-form-data
			    (emtm:binding-form-data->bind
			       form))
			 (emtm:test-form-data
			    (emtm:test-form-data->form
			       form))))
		     (new-held-back ()))

		  (when (member sym been-bound)
		     (typecase form
			(emtm:binding-form-data
			   ;;Flag it a rebinding
			   (setf
			      (emtm:binding-form-data->rebinding-p form)
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

;;;_  . emtm:build-form
(defun emtm:build-form (sym-list all-formdata core)
   ""
   
   (let*
      ((formdata-list
	  (emtm:formdata->form-LIST all-formdata))
	 (sorted-list
	    (emtm:sort-bindings sym-list formdata-list))
	 (pre-eval-list
	    (emtm:formdata->pre-eval-LIST all-formdata))
	 ;;Pre-evals don't need sorting because all they do is bind
	 ;;gensyms that are only shared with match-time forms.
	 ;;(Get bindings in pre-eval-list)
	 (form-wo-pre-eval
	    (emtm:build-form-recurse sorted-list core)))
      
      (if
	 pre-eval-list
	 ;;Eval because some sub-forms need to be evalled at
	 ;;pattern-compile time.  Such sub-forms are made by the
	 ;;`eval' governor.  A `backquote' protects `form-wo-pre-eval'
	 ;;from this eval, except the parts of it that are explicitly
	 ;;unquoted.
	 (eval
	    (emtm:build-form-recurse
	       pre-eval-list
	       ;;This, the core form, is quoted. The inner, explicit
	       ;;`backquote' doesn't act until we get back to `eval',
	       ;;above.
	       `(backquote ,form-wo-pre-eval)))
	 ;;This branch is just a special case of the other.
	 form-wo-pre-eval)))

;;;_  . emtm:build-form--1 
;;These might be renamed, this to `emtm:build-form' and the
;;original to *-x.
(defun emtm:build-form--1 (sym pattern core-form)
   "Build a form and return it.
The factored-out part of emtm and emtm:lambda"
   
   (let*
      (
	 ;;Decompose the pattern.
	 (formdata (emtm:parse-pattern sym pattern))
	 ;;Build the inner part of the form
	 (form (emtm:build-form (list sym) formdata core-form)))
      form))

;;;_ , Entry points
;;;_  . Test-supporter emtm:th:single-gov
(defun emtm:ts:single-gov (func pattern obj)

   "Match OBJ vs the pattern PATTERN, forcing FUNC to be the governor
function when parsing PATTERN.  PATTERN should be quoted and not
contain the symbol `testhelp-483s'.

Intended for testing governor functions in isolation."

   ;;For this, punt: Always use `testhelp-483s' as the symbol.
   (let
      ((testhelp-483s obj))
      (eval
	 (emtm:build-form 
	    '(testhelp-483s) 
	    (funcall func 'testhelp-483s pattern) 
	    't))))

;;;_  . emtm
;;Does this order of arguments make sense?  Or should it be vv?
(defmacro emtm (object-form pattern)
   ""
   (let
      ((sym (gensym)))
      `(let
	 ((,sym ,object-form))
	  ,(emtm:build-form--1 sym pattern 't))))

;;;_  . emtm-f
(defun emtm-f (obj pattern)
   "Non-nil if OBJ matches PATTERN.

PATTERN must be a boxed pattern object (Use `emtm:make-pattern')."
   (unless (emtm:box-p pattern)
      (error "PATTERN needs to be an emtm pattern object"))

   (let*
      ((func (emtm:box->func pattern)))
      (funcall func obj)))

;;;_  . emtm-either
;;This must use `equal' for when it's used by the `eval' governor.
(defun* emtm-either (obj1 obj2 &optional (cmp-f #'equal))
   "Non-nil if either:
 * OBJ1 and OBJ2 are equal, or
 * OBJ2 is a pattern-box and OBJ1 matches it."
   (if
      (emtm:box-p obj2)
      (emtm-f obj1 obj2 )
      (funcall cmp-f obj1 obj2)))

;;;_  . emtm:make-pattern
(defmacro emtm:make-pattern (pattern)
   "Makes a boxed pattern object according to PATTERN."
   `(emtm:make-box
       :func (emtm:lambda ,pattern t)
       ;;For now, we know nothing about type.  Later, we'll query the
       ;;head, if any.
       :type t))

;;;_  . emtm:make-general-lambda
;;Rewrite the others in terms of this.
(defmacro emtm:make-general-lambda (pattern bindings &rest body)
   "Make a pattern-match function that can take extra args.
First argument is the object to be matched by PATTERN.
BINDINGS is a list of symbols.  They will be available inside
PATTERN and BODY, bound to whatever the extra args are bound to.
BODY is a form body."
   (let
      ((sym (gensym)))
      `
      (lambda (,sym ,@bindings)
	 ,(emtm:build-form 
	     (list* sym bindings)
	     (emtm:parse-pattern sym pattern) 
	      `(progn ,@body)))))

;;;_  . emtm:lambda
(defmacro emtm:lambda (pattern &rest body)
   "BODY is a form body."
   (let
      ((sym (gensym)))
      `(lambda (,sym)
	  ,(emtm:build-form--1 sym pattern `(progn ,@body)))))


;;;_  . emtm:lambda-binds

(defmacro emtm:lambda-binds (pattern &rest bindings)
   ""
   
   ;;Check `bindings' as a list of symbols.
   `(emtm:lambda 
       ,pattern
       ,(if bindings
	   `(list ,@bindings)
	   't)))

;;;_  . emtm-let
(defmacro emtm-let (object-form pattern &rest body)
   ""
   (let*
      ((sym (gensym))
	 ;;Decompose the pattern.
	 (formdata (emtm:parse-pattern sym pattern))
	 ;;Build the inner part of the form
	 (form (emtm:build-form 
		  (list sym) 
		  formdata 
		  `(progn ,@body))))
      `(let
	 ((,sym ,object-form))
	  ,form)))

;;;_  . emtm-case
;;Not implemented.
;;;_ , Type support
(deftype emtm:pattern (x)
   'emtm:box)

;;;_ , Functions to help others build

;;;_  . Making structure governors
;;;_   , emtm:make-struct-governor-x
(defun emtm:make-struct-governor-x (sym check-type depth accessor)
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
	    (emtm:make-binding-form-data
	       :uses 
	       (emtm:parse-dependencies
		  (list sym) (list check-type))
	       :bind depth-sym
	       :form `(,accessor ,sym)))
	 ;;Match the given `depth' pattern to that object.
	 (child-formdata
	    (emtm:parse-pattern
	       depth-sym
	       depth)))

      (list bind-field-depth child-formdata)))
;;;_   , emtm:make-typecheck-form
(defun emtm:make-typecheck-form (sym pred)
   "Make a form to check the type of SYM.
SYM is the symbol to be checked.
PRED is a function-quoted predicate to apply to it."
   (emtm:make-test-form-data
      :explanation "Object is the wrong type"
      :uses (list sym)
      :form `(,pred ,sym)))
;;;_   , Helpers

(defsubst emtm:util:keysym->sv-keysym (keysym)
   ""
   (intern (concat "sv-" (symbol-name keysym))))

(defsubst emtm:util:keysym->accessor-sym (conc-name keysym)
   ""
   (intern (concat (symbol-name conc-name) (symbol-name keysym))))

;;;_   , emtm:make-struct-governor

;;We make a lambda, which functions can call internally, or which we'd
;;set the symbol function to.  So don't take name here.

;;It is a pattern governor function, not a ctor.  If included in a
;;pattern, requires that the object match the explicit fields of the
;;would-be object.  It ignores the implicit fields (they are not even
;;`nil', they are just unspecified).  To make an instance, include it
;;in emtm:lambda.

;;Allowed args are the same as a default ctor.  However, they are
;;patterns, so they can be literals, bindings, and anything else
;;emtm supports.


;;In emacs 21 at least, nested backquotes do not work.  Unquote
;;operators operate in the smallest enclosing backquoted scope, not
;;the largest, and after seeing a nested backquote, parser no longer
;;recognizes ,@.  So as a workaround I've moved all the nested
;;backquote work out to helper functions.
(defmacro emtm:make-struct-governor (pred conc-name keys)
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
		    `(,key () ,(emtm:util:keysym->sv-keysym key)))
	       keys))
	 (key-datalist
 	    (mapcar
 	       #'(lambda (key)
		    ;;A form making data that applies to a single
		    ;;field, like (if sv-x (list x #'foo->x) ())
 		    `(if ,(emtm:util:keysym->sv-keysym key)
 			(list
 			   ,key
			   #',(emtm:util:keysym->accessor-sym
				 conc-name key))
 			()))
 	       keys)))
      

      `(lambda (sym pattern &optional other-deps)
	  ""
   
	  (destructuring-bind (&key ,@key-keylist)
	     (cdr pattern)

	     (let* 
		(
		   ;;Make form to check that Object is of the given
		   ;;type.
		   (check-type
		      (emtm:make-typecheck-form sym #',pred))

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
			      (emtm:make-struct-governor-x 
				 sym 
				 check-type
				 (first cell)
				 (second cell)))
			 data-list)))
	     
		(utiacc:list->object
		   (append
		      (list 
			 (emtm:make-formdata
			    :form-LIST 
			    (append
			       (list check-type)
			       (mapcar #'first formpair-list))))
		      (mapcar #'second formpair-list))
		   'emtm:formdata))))))

;;;_   , emtm:define-struct-governor-oldstyle
(defmacro emtm:define-struct-governor-oldstyle (gov-name pred-name
					      conc-name fields)
   "Define a pattern-ctor as match-governor GOV-NAME.
PRED-NAME must be the name of the predicate that tests it.
CONC-NAME must be the structure's conc-name."
   ;;No type info yet.
   `(put ',gov-name 'emtm:makepattern
      (emtm:make-struct-governor 
	 ,pred-name
	 ,conc-name
	 ,fields)))
;;;_   , emtm:define-struct-governor-x
;;$$FIXME Should take `name' as parameter
;;The defaults might be moved out to caller, since all clients should
;;have the same defaults. 
(defun* emtm:define-struct-governor-x 
   (
      (&key
	 (predicate (intern (concat (symbol-name name) "-p"))) 
	 (constructor (intern (concat "make-" (symbol-name name))))
	 (conc-name (intern (concat (symbol-name name) "-"))) 
	 &allow-other-keys)
      fields)
   "Make the form."
   `(put ',constructor 'emtm:makepattern
       (emtm:make-struct-governor 
	  ,predicate
	  ,conc-name
	  ,fields)))
;;;_   , emtm:define-struct-governor 
(defmacro emtm:define-struct-governor (name+args &rest fields)
   "Define a structure governor function.  
emtm will see the definition and use it appropriately.
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
      (emtm:define-struct-governor-x struct-options fields)))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/match)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/match.el ends here
