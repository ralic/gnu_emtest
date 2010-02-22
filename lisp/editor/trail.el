;;;_ editor/trail.el --- Trailed eval - Utility for WYSIWYG destructurer construction

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp, maint

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
(require 'tester/testhelp/misc) ;;For `emt/util/collect-in-tree'
(eval-and-compile
   (when (not (fboundp 'rtest:deftest))
      (defmacro emtp (id args &rest rest)
	 `(progn ,@rest))))
;;;_. Body
;;;_ , Structures
(defstruct (emt/trail:item
	      (:copier nil)
	      (:conc-name emt/trail:item->)
	      (:constructor emt/trail:make-item))
   "Info for one item on the eval trail"
   expression
   value
   (index ()      :type integer)
   (selected-p () :type bool)
   should  ;;Form is undecided.  Punted as `t' in tests
   marks   ;;Form is undecided
   ;;Alist of data, all settable by user.  Type is not so easily checked.
   generator-data
   (var-name ()   :type symbol))


;;;_ , Config
(defconst emt/trail:buf-name "*Trail buffer*" 
   "Buffer name for emt/trail" )
;;;_ , Variables
;;;_  . emt/trail:value-list
(defvar emt/trail:value-list () 
   "List of values for emt/trail" )
;;;_  . emt/trail/eg-narrowing
;;Just for emt:eg generator
(defvar emt/trail/eg-narrowing () "" )
;;;_ , emt/trail:record-item
(defun emt/trail:record-item (expression value)
   ""

   (let*
      (  (index (length emt/trail:value-list))
	 (item
	    (emt/trail:make-item
	       :expression expression
	       :value value
	       :index index)))
      ;;It must go at the end, to preserve correct indexing.
      (setq emt/trail:value-list
	 (append emt/trail:value-list (list item)))

      (emtp tp:8efac1de-09d4-4865-bea2-d3df2b211ad3 ()
	 (let
	    ((buf
		(get-buffer-create emt/trail:buf-name)))
	    (with-current-buffer buf
	       ;;Not (point-max).  Even if buffer were narrowed for
	       ;;some reason, we still must print at the end.
	       (goto-char (1+ (buffer-size)))

	       (unless (bolp) (insert "\n"))
	       (insert (number-to-string index) ":  ")
	       (pp expression (current-buffer))
	       (insert "  => ")
	       (pp value (current-buffer))
	       (insert "\n"))
	    (pop-to-buffer buf)))))


;;;_ , emt/trail:eval-expression
(defun emt/trail:eval-expression (eval-expression-arg)
   "Evaluate EVAL-EXPRESSION-ARG and store it on the trail"
   (interactive
      (list (read-from-minibuffer "Eval: "
	       nil read-expression-map t
	       'read-expression-history)))
   (let
      ((val (eval eval-expression-arg)))
      (emt/trail:record-item eval-expression-arg val)))

;;;_  . Tests

;;Usability tests only.

;;;_ , Building code
;;;_  . Structures
(defstruct (emt/trail:superitem 
	      (:type list)
	      (:copier nil)
	      (:conc-name emt/trail:superitem->)
	      (:constructor emt/trail:make-superitem))
   ""
   item
   src-info)

(defstruct (emt/trail:code-source
	      (:copier nil)
	      (:conc-name emt/trail:code-source->)
	      (:constructor emt/trail:make-code-source))
   "Data about a piece of code source, either value or expression."

   ;;(which () :type (member expression value))
   (count 0 :type integer)
   (binding-p () :type bool)
   surface-form
   real-form)
(defstruct (emt/trail:src-info
	      (:copier nil)
	      (:conc-name emt/trail:src-info->)
	      (:constructor emt/trail:make-src-info))
   "Data about an item relative to  a single generation run."
   (item            () :type emt/trail:item)
   (visited-p       () :type bool)
   (refs            () :type (repeat *))
   (value-info      () :type emt/trail:code-source)
   (expression-info () :type emt/trail:code-source))

;;;_  . emt/trail:build-expression-code 
(defun emt/trail:build-expression-code (source indexing)
   "Builds a form corresponding to an expression.
Will do substitutions.
Handles making `should' or `assert' as well."
   (emt/trail:destructure-superitem (item src-info) source
      (let*
	 ((raw-form
	     (emt/trail:item->expression item))
	    (refs
	       (emt/trail:src-info->refs src-info))
	    ;;Alist mapping each ref to the surface form it
	    ;;should be replaced by.
	    (ref-alist
	       (mapcar
		  #'(lambda (ref)
		       (cons 
			  ref
			  (emt/trail:code-source->surface-form
			     (emt/trail:src-info->expression-info
				(emt/trail:superitem->src-info
				   ;;$$Encap this lookup.
				   (nth (second ref) indexing))))))
		  refs))
	    (form-1
	       ;;$$Could :test #'eq
	       (sublis ref-alist raw-form))
	    (form-2
	       (if
		  (emt/trail:item->should item)
		  ;;This might be parameterized - make "assert" or
		  ;;"should" depending on something.
		  `(assert 
		      ,form-1
		      ;;Must only show-args for forms that
		      ;;are lists.
		      ,(if (listp form-1) t nil))
		  form-1)))
	 form-2)))

;;;_  . emt/trail:code-src-setup-local-form
(defun emt/trail:code-src-setup-local-form (code-src build-core-f)
   "Build the local forms for this.
Assumes everything it references has already been built for.
This looks at ref count and does nothing if = 0."
   
   (when
      (> (emt/trail:code-source->count code-src) 0)
      (let
	 (
	    ;;Maybe make a name.  `nil' or a symbol.  Punt for now.
	    (name nil))

	 ;;NAME could take item's per-generator data into account.
	 ;;Let a hook build it via `run-hook-with-args-until-success'.
	 ;;Each is passed item and code-source.
	 '(> (emt/trail:code-source->count code-src) 1)
		  
	 ;;Callback to get deep form.
	 (setf
	    (emt/trail:code-source->real-form code-src)
	    (funcall build-core-f))
		  
	 ;;Set up surface stuff.
	 (if name
	    (progn 
	       (setf
		  (emt/trail:code-source->surface-form code-src)
		  name)
	       (setf
		  (emt/trail:code-source->binding-p code-src)
		  t))
	    (setf
	       (emt/trail:code-source->surface-form code-src)
	       (emt/trail:code-source->real-form code-src))))))

;;;_  . emt/trail:code-src-add-to-form
(defun emt/trail:code-src-add-to-form (form code-src)
   ""
   (if (> (emt/trail:code-source->count code-src) 0)
      (progn
	 ;;For bindings, add a `let*' if there is none
	 ;;(transform a progn), otherwise add to its
	 ;;bindings.  Add new ones to the front.

	 ;;For needed-items, always add them in a progn,
	 ;;creating one if needed.

	 ;;For now, incoming form is assumed to be a `progn'
	 `(progn 
	     ,(emt/trail:code-source->surface-form code-src)
	     ,@(cdr form)))
      form))

;;;_  . emt/trail:src-info-incf-count
(defun emt/trail:code-source-incf-count (src-info which)
   ""

   (let
      ((code-source
	  (ecase which 
	     (value
		(emt/trail:src-info->value-info 
		   src-info))
	     (expression
		(emt/trail:src-info->expression-info 
		   src-info)))))
   (incf (emt/trail:code-source->count code-source))))

;;;_  . emt/trail:destructure-superitem
(defmacro emt/trail:destructure-superitem (fields superitem &rest body)
   ""
   
   `(destructuring-bind ,fields ,superitem ,@body))
(def-edebug-spec emt/trail:destructure-superitem
   (sexp form body))

;;;_  . emt/trail:build-code 
(defun emt/trail:build-code ()
   ""

   (let*
      (
	 ;;An indexing list, must have same indexing that trail has.
	 ;;Of type (repeat emt/trail:superitem)
	 (superitem-list
	    (mapcar 
	       #'(lambda (x)
		    (emt/trail:make-superitem
		       :item x 
		       :src-info
		       (emt/trail:make-src-info
			  :item x
			  :expression-info
			  (emt/trail:make-code-source)
			  :value-info
			  (emt/trail:make-code-source))))
	       
	       emt/trail:value-list))

	 ;;List of the items marked "should".  
	 ;;Of type (repeat emt/trail:item)
	 (should-list
	    ;;Non-destructive.
	    (remove* nil emt/trail:value-list
	       :test-not
	       #'(lambda (dummy item)
		    ;;Punt, because this field is still just a bool.
		    (emt/trail:item->should item))))

	 ;;Initial set of needed-items.  
	 ;;Of type (repeat emt/trail:superitem)
	 (pending
	    (mapcar
	       #'(lambda (item)
		    (let*
		       (  (superitem
			     (nth (emt/trail:item->index item)
				superitem-list))
			  (src-info (emt/trail:superitem->src-info superitem)))
		       
		       ;;Count is 1 because we will generate a should
		       ;;that refers to it.
		       (emt/trail:code-source-incf-count 
			  src-info
			  'expression)
		       superitem))
	       should-list))
	 
	 ;;Items that will contribute to the final source.
	 ;;Of type (repeat emt/trail:superitem)
	 (needed-items ()))

      ;;Tests are interested in which elements are initial.  (This
      ;;wasn't used)
      (emtp tp:9fd5ba68-6ce6-45ae-b79d-311eebbbfa56
	 (pending))

      ;;Find all items that are needed - find ref counts at the same
      ;;time.
      (while pending
	 (let
	    ((superitem (pop pending)))
	    (emt/trail:destructure-superitem (item src-info) superitem
	       ;;Don't revisit items we already visited.  Since the
	       ;;flag means "explored" we can get it twice.
	       (unless (emt/trail:src-info->visited-p src-info)
		  (setf (emt/trail:src-info->visited-p src-info) t)

		  (push superitem needed-items)

		  ;;Find all refs anywhere in its item's expression tree.
		  ;;Ie, collect elements that pass a test.
		  (let 
		     ((refs
			 (emt/util/collect-in-tree 
			    #'(lambda (x)
				 (and
				    (listp x)
				    (memq
				       (car x)
				       '(emt/trail:value
					   emt/trail:value-always))))
			    (emt/trail:item->expression item))))
		     
		     ;;Store refs so that
		     ;;`emt/trail:build-expression-code' needn't look for
		     ;;them again.
		     (setf
			(emt/trail:src-info->refs src-info)
			refs)
	       
		     ;;For each ref...
		     (dolist (ref refs)
			;;Tests are interested in which items each
			;;item is seen to depend on.

			;;Increment the ref count of the thing it
			;;references
			(case (car ref)
			   (emt/trail:value
			      (let*
				 ((index (second ref))
				    (its-src-info
				       (emt/trail:superitem->src-info 
					  (nth index superitem-list))))
				 (emt/trail:code-source-incf-count 
				    its-src-info
				    'expression)
				 (emtp 
				    tp:ea524bc6-1e65-4c90-8772-ab94c916a375
				    (
				       (emt/trail:item->index item)
				       index 
				       (car ref)))


				 ;;If an expression is referenced and
				 ;;hasn't been explored yet, put it
				 ;;onto PENDING so we will explore it.
				 (unless nil
				    (emt/trail:src-info->visited-p 
				       its-src-info)
				    (push 
				       (nth index superitem-list) 
				       pending))))
		     
			   (emt/trail:value-always
			      (let
				 ((index (second ref)))
				 (emtp 
				    tp:ea524bc6-1e65-4c90-8772-ab94c916a375
				    (
				       (emt/trail:item->index item)
				       index 
				       (car ref)))
				 (emt/trail:code-source-incf-count 
				    (emt/trail:superitem->src-info 
				       (nth index superitem-list))
				    'value))))))))))
      

      ;;Sort `needed-items' by index of the underlying piece, lowest first.
      (setq needed-items
	 (sort* needed-items #'<
	    :key
	    #'(lambda (info)
		 (emt/trail:item->index (emt/trail:superitem->item info)))))

      ;;Root first, create local forms for each source
      (dolist (source needed-items)
	 (emt/trail:destructure-superitem (item src-info) source

	    (emt/trail:code-src-setup-local-form
	       (emt/trail:src-info->expression-info src-info)
	       #'(lambda ()
		    (emt/trail:build-expression-code source superitem-list)))
	    
	    (emt/trail:code-src-setup-local-form
	       (emt/trail:src-info->value-info src-info)
	       ;;Will be a hook to allow making eg definitions.
	       #'(lambda ()
		    (emt/trail:item->value item)))))
      
      ;;Build whole code.
      (let
	 ((form (progn)))
	 ;;Traverse `needed-items' leaves first (ie, opposite of
	 ;;before).  Really, we're reducing an initial value across
	 ;;the list.
	 (dolist (superitem (reverse needed-items))
	    (emt/trail:destructure-superitem (item src-info) superitem
	       (setq form
		  (emt/trail:code-src-add-to-form 
		     form 
		     (emt/trail:src-info->expression-info src-info)))
	       (setq form
		  (emt/trail:code-src-add-to-form 
		     form 
		     (emt/trail:src-info->value-info src-info)))))
	 
	 ;;Add any whole-scope management (by generators)

	 ;;Return form.  In the future, may produce, reduce, and
	 ;;return an alist of which one part is this form, so it's
	 ;;extensible
	 form)))


;;;_   , Test helpers 
;;;_    . emt/trail:build-code:th
(defun emt/trail:build-code:th (input-list)
   ""
   
   (emtp:eval
      (dolist (input input-list)
	 ;;Build item.
	 (destructuring-bind (should &rest expression) input
	    (emt/trail:record-item expression (eval expression))
	    ;;SHOULD tells us to set up a should.
	    (when should
	       (setf
		  (emt/trail:item->should
		     (car (last emt/trail:value-list)))
		  t))))
      ;;Testpoint to skip displaying the item.
      (tp* (:id tp:8efac1de-09d4-4865-bea2-d3df2b211ad3 :count nil)())))

;;;_    . emt/trail:build-code:thm
(defmacro emt/trail:build-code:thm (input-list bindings &rest body)
   "Run BODY with `emt/trail:value-list' in a particular state.
INPUT-LIST elements are in the form (SHOULD . EXPRESSION).
BINDINGS are as for a `let'"
   
   `(let ((emt/trail:value-list ())
	    ,@bindings)
       (emt/trail:build-code:th ,input-list)
       ,@body))
;;;_    . emt/trail:build-code:th2
(defun emt/trail:build-code:th2 (precedence-pairs)
   ""
   (emtp:eval
      (emt/trail:build-code)
      ;;Testpoint per dependency
      (tp*
	 (:id tp:ea524bc6-1e65-4c90-8772-ab94c916a375
	    :count 
	    (length precedence-pairs))
	 (got-index want-index type)
	 (emt:tp:collect (list got-index want-index type)))
      (finally (:bindings ((precedence-pairs precedence-pairs))) (depend-set)
	 (assert
	    (emt:sets= depend-set 
	       precedence-pairs)
	    t))))
;;;_    . emt/trail:build-code:thm2
(defmacro* emt/trail:build-code:thm2 
   ((&key inputs bindings precedence-pairs sym) &rest body)
   ""
   
   `(emt/trail:build-code:thm
       ,inputs
       ,bindings
       (let
	  ((,sym
	      (emt/trail:build-code:th2 ,precedence-pairs)))
	  ,@body)))


;;;_    . emt/trail:build-code:th:w-bindings-passes-p
(defun emt/trail:build-code:th:w-bindings-passes-p 
   (bindings should-pass-p code)
   ""

   (let*
      ((form
	  ;;Bind the values CODE sees.
	  `(let 
	      (,@bindings)
	      ;;Code is a single form, not a list.
	      ,code))
	 ;;Eval it.  Does it err?
	 (errs-p
	    (emt:gives-error
	       (eval form))))
      
      ;;Test that it errored just if it should.
      (if should-pass-p
	 (assert (not errs-p)
	    nil "Form %s failed" form)
	 (assert errs-p
	    nil "Form %s wrongly succeeded" form))))



;;;_   , Tests
(rtest:deftest emt/trail:build-code

   (  "Situation: Consists of a single direct should-item.
Simple: The arg to that is always t - can't fail.
Operation: Build form
Operation: Eval the form.
When that var is non-nil, form has no error.
When that var is nil, form has error."
      (emt/trail:build-code:thm2
	 (:inputs
	    '((t . t))
	    :bindings
	    () ;;No bindings
	    :precedence-pairs
	    '()
	    :sym
	    code)
	 
	 (emt/trail:build-code:th:w-bindings-passes-p
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
      (emt/trail:build-code:thm2
	 (:inputs
	    ;;Inputs are (SHOULD . EXPRESSION)
	    '(  (t . my-var))
	    :bindings
	    ((my-var t))
	    :precedence-pairs
	    '()
	    :sym
	    code)
	 
	    
	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((my-var t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
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
      (emt/trail:build-code:thm2
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
	 
	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))
   
   (  "Shows: Works in the presence of previous items
Situation: An `equal' should and two dummy elements.
Response: The sub-situations behave as expected."
      (emt/trail:build-code:thm2
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

	 
	    
	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))


   (  "Shows: Use of previous trail items.
Situation: An `equal' fed from two items, which directly are variables
Response: The sub-situations behave as expected."
      (emt/trail:build-code:thm2
	 (:inputs
	    '(  (nil . var1)
		(nil . var2)
		(t . (equal (emt/trail:value 0)(emt/trail:value 1))))
	    :bindings
	    ((var1 t)
	       (var2 t))
	    :precedence-pairs
	    '((2 1 emt/trail:value)
		(2 0 emt/trail:value))
	    :sym
	    code)
	 
	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)
	  
	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))

   (  "Shows: Can use chains of input: 
Situation: An `equal' fed from two items, which directly are variables
Response: The sub-situations behave as expected."
      (emt/trail:build-code:thm2
	 (:inputs
	    ;;Inputs are (SHOULD . EXPRESSION)
	    '(  (nil . var1)
		(nil . (emt/trail:value 0))
		(nil . var2)
		(t . (equal (emt/trail:value 1)(emt/trail:value 2))))
	    :bindings
	    ((var1 t)
	       (var2 t))
	    :precedence-pairs
	    '((3 1 emt/trail:value)
		(3 2 emt/trail:value)
		(1 0 emt/trail:value))
	    :sym
	    code)
	 
	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))

   (  "Shows: Multiple uses of the same item don't cause problems. 
Situation: An `equal' fed from twice each from two items, which
directly are variables.
Response: The sub-situations behave as expected."
      (emt/trail:build-code:thm2
	 (:inputs
	    ;;Inputs are (SHOULD . EXPRESSION)
	    '(  (nil . var1)
		(nil . var2)
		(t . (equal 
			(list
			   (emt/trail:value 0)
			   (emt/trail:value 0))
			(list
			   (emt/trail:value 1)
			   (emt/trail:value 1)))))
	    :bindings
	    ((var1 t)
	       (var2 t))
	    :precedence-pairs
	    ;;Would rather compare them with duplicates removed.
	    '((2 1 emt/trail:value)
		(2 1 emt/trail:value)
		(2 0 emt/trail:value)
		(2 0 emt/trail:value))
	    :sym
	    code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 t) (var2 t)) t code)

	 (emt/trail:build-code:th:w-bindings-passes-p
	    '((var1 13) (var2 14)) nil code)	    

	 t))


   ;;Shows: values are usable as well.
   ;;An `equal' is fed from an expression and a value.  The expression
   ;;is just a variable.  Bind the variable to the same as the value,
   ;;it passes.  Bind to different, it fails.

   ;;Shows: It supports multiple shoulds.


   ;;Indirect should items, for equal-to-value, equal sets, and others.
   
   )

;;;_ , emt/trail:export-code
(defun emt/trail:export-code ()
   "Build and export code"
   ;;For now, just use one exporter
   (let*
      ()
      
      ))

;;;_  . Tests
;;It should be direct

;;;_ , Accessors
;;;_  . emt/trail:value
(defsubst emt/trail:value (index)
   ""
   (emt/trail:item->value
      (nth index emt/trail:value-list)))

;;;_  . emt/trail:value-always
(defalias 'emt/trail:value-always 'emt/trail:value)
;;;_ , Exporters
;;;_  . emt/trail:export-to-kill-ring
(defun emt/trail:export-to-kill-ring (code)
   ""
   
   (let*
      ()
      
      ))


;;;_. Footers
;;;_ , Provides

(provide 'editor/trail)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; editor/trail.el ends here
