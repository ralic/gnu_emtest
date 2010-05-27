;;;_ emtest/editing/trail.el --- Trailed eval - Utility for WYSIWYG destructurer construction

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

;; This code is still under construction.


;;;_ , Requires

(require 'emtest/testhelp/misc) ;;For `emth:collect-in-tree'
(eval-when-compile
   (require 'emtest/testhelp/testpoint/requirer)
   (emtp:require))
(eval-when-compile
   (require 'cl))

;;;_. Body
;;;_ , Structures
(defstruct (emter:item
	      (:copier nil)
	      (:conc-name emter:item->)
	      (:constructor emter:make-item))
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
(defconst emter:buf-name "*Trail buffer*" 
   "Buffer name for emter" )
;;;_ , Variables
;;;_  . emter:value-list
(defvar emter:value-list () 
   "List of values for emter" )
;;;_  . emter/eg-narrowing
;;Just for emtg generator
(defvar emter/eg-narrowing () "" )
;;;_ , emter:record-item
(defun emter:record-item (expression value)
   ""

   (let*
      (  (index (length emter:value-list))
	 (item
	    (emter:make-item
	       :expression expression
	       :value value
	       :index index)))
      ;;It must go at the end, to preserve correct indexing.
      (setq emter:value-list
	 (append emter:value-list (list item)))

      (emtp tp:8efac1de-09d4-4865-bea2-d3df2b211ad3 ()
	 (let
	    ((buf
		(get-buffer-create emter:buf-name)))
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


;;;_ , emter:eval-expression
(defun emter:eval-expression (eval-expression-arg)
   "Evaluate EVAL-EXPRESSION-ARG and store it on the trail"
   (interactive
      (list (read-from-minibuffer "Eval: "
	       nil read-expression-map t
	       'read-expression-history)))
   (let
      ((val (eval eval-expression-arg)))
      (emter:record-item eval-expression-arg val)))

;;;_  . Tests

;;Usability tests only.

;;;_ , Building code
;;;_  . Structures
(defstruct (emter:superitem 
	      (:type list)
	      (:copier nil)
	      (:conc-name emter:superitem->)
	      (:constructor emter:make-superitem))
   ""
   item
   src-info)

(defstruct (emter:code-source
	      (:copier nil)
	      (:conc-name emter:code-source->)
	      (:constructor emter:make-code-source))
   "Data about a piece of code source, either value or expression."

   ;;(which () :type (member expression value))
   (count 0 :type integer)
   (binding-p () :type bool)
   surface-form
   real-form)
(defstruct (emter:src-info
	      (:copier nil)
	      (:conc-name emter:src-info->)
	      (:constructor emter:make-src-info))
   "Data about an item relative to  a single generation run."
   (item            () :type emter:item)
   (visited-p       () :type bool)
   (refs            () :type (repeat *))
   (value-info      () :type emter:code-source)
   (expression-info () :type emter:code-source))

;;;_  . emter:destructure-superitem
(defmacro emter:destructure-superitem (fields superitem &rest body)
   ""
   ;;$$ANNOTATE ME - move edebug spec into here
   `(destructuring-bind ,fields ,superitem ,@body))
(def-edebug-spec emter:destructure-superitem
   (sexp form body))

;;;_  . emter:build-expression-code 
(defun emter:build-expression-code (source indexing)
   "Builds a form corresponding to an expression.
Will do substitutions.
Handles making `should' or `assert' as well."
   (emter:destructure-superitem (item src-info) source
      (let*
	 ((raw-form
	     (emter:item->expression item))
	    (refs
	       (emter:src-info->refs src-info))
	    ;;Alist mapping each ref to the surface form it
	    ;;should be replaced by.
	    (ref-alist
	       (mapcar
		  #'(lambda (ref)
		       (cons 
			  ref
			  (emter:code-source->surface-form
			     (emter:src-info->expression-info
				(emter:superitem->src-info
				   ;;$$Encap this lookup.
				   (nth (second ref) indexing))))))
		  refs))
	    (form-1
	       ;;$$Could :test #'eq
	       (sublis ref-alist raw-form))
	    (form-2
	       (if
		  (emter:item->should item)
		  ;;This might be parameterized - make "assert" or
		  ;;"should" depending on something.
		  `(assert 
		      ,form-1
		      ;;Must only show-args for forms that
		      ;;are lists.
		      ,(if (listp form-1) t nil))
		  form-1)))
	 form-2)))

;;;_  . emter:code-src-setup-local-form
(defun emter:code-src-setup-local-form (code-src build-core-f)
   "Build the local forms for this.
Assumes everything it references has already been built for.
This looks at ref count and does nothing if = 0."
   
   (when
      (> (emter:code-source->count code-src) 0)
      (let
	 (
	    ;;Maybe make a name.  `nil' or a symbol.  Punt for now.
	    (name nil))

	 ;;NAME could take item's per-generator data into account.
	 ;;Let a hook build it via `run-hook-with-args-until-success'.
	 ;;Each is passed item and code-source.
	 '(> (emter:code-source->count code-src) 1)
		  
	 ;;Callback to get deep form.
	 (setf
	    (emter:code-source->real-form code-src)
	    (funcall build-core-f))
		  
	 ;;Set up surface stuff.
	 (if name
	    (progn 
	       (setf
		  (emter:code-source->surface-form code-src)
		  name)
	       (setf
		  (emter:code-source->binding-p code-src)
		  t))
	    (setf
	       (emter:code-source->surface-form code-src)
	       (emter:code-source->real-form code-src))))))

;;;_  . emter:code-src-add-to-form
(defun emter:code-src-add-to-form (form code-src)
   ""
   (if (> (emter:code-source->count code-src) 0)
      (progn
	 ;;For bindings, add a `let*' if there is none
	 ;;(transform a progn), otherwise add to its
	 ;;bindings.  Add new ones to the front.

	 ;;For needed-items, always add them in a progn,
	 ;;creating one if needed.

	 ;;For now, incoming form is assumed to be a `progn'
	 `(progn 
	     ,(emter:code-source->surface-form code-src)
	     ,@(cdr form)))
      form))

;;;_  . emter:src-info-incf-count
(defun emter:code-source-incf-count (src-info which)
   ""

   (let
      ((code-source
	  (ecase which 
	     (value
		(emter:src-info->value-info 
		   src-info))
	     (expression
		(emter:src-info->expression-info 
		   src-info)))))
   (incf (emter:code-source->count code-source))))

;;;_  . emter:build-code 
(defun emter:build-code ()
   ""

   (let*
      (
	 ;;An indexing list, must have same indexing that trail has.
	 ;;Of type (repeat emter:superitem)
	 (superitem-list
	    (mapcar 
	       #'(lambda (x)
		    (emter:make-superitem
		       :item x 
		       :src-info
		       (emter:make-src-info
			  :item x
			  :expression-info
			  (emter:make-code-source)
			  :value-info
			  (emter:make-code-source))))
	       
	       emter:value-list))

	 ;;List of the items marked "should" (ie, should be non-nil in a test)
	 ;;Of type (repeat emter:item)
	 (should-list
	    ;;Non-destructive.
	    (remove* nil emter:value-list
	       :test-not
	       #'(lambda (dummy item)
		    ;;Punt, because this field is still just a bool.
		    (emter:item->should item))))

	 ;;Initial set of needed-items.  
	 ;;Of type (repeat emter:superitem)
	 (pending
	    (mapcar
	       #'(lambda (item)
		    (let*
		       (  (superitem
			     (nth (emter:item->index item)
				superitem-list))
			  (src-info (emter:superitem->src-info superitem)))
		       
		       ;;Count is 1 because we will generate a should
		       ;;that refers to it.
		       (emter:code-source-incf-count 
			  src-info
			  'expression)
		       superitem))
	       should-list))
	 
	 ;;Items that will contribute to the final source.
	 ;;Of type (repeat emter:superitem)
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
	    (emter:destructure-superitem (item src-info) superitem
	       ;;Don't revisit items we already visited.  Since the
	       ;;flag means "explored" we can get it twice.
	       (unless (emter:src-info->visited-p src-info)
		  (setf (emter:src-info->visited-p src-info) t)

		  (push superitem needed-items)

		  ;;Find all refs anywhere in its item's expression tree.
		  ;;Ie, collect elements that pass a test.
		  (let 
		     ((refs
			 (emth:collect-in-tree 
			    #'(lambda (x)
				 (and
				    (listp x)
				    (memq
				       (car x)
				       '(emter:value
					   emter:value-always))))
			    (emter:item->expression item))))
		     
		     ;;Store refs so that
		     ;;`emter:build-expression-code' needn't look for
		     ;;them again.
		     (setf
			(emter:src-info->refs src-info)
			refs)
	       
		     ;;For each ref...
		     (dolist (ref refs)
			;;Tests are interested in which items each
			;;item is seen to depend on.

			;;Increment the ref count of the thing it
			;;references
			(case (car ref)
			   (emter:value
			      (let*
				 ((index (second ref))
				    (its-src-info
				       (emter:superitem->src-info 
					  (nth index superitem-list))))
				 (emter:code-source-incf-count 
				    its-src-info
				    'expression)
				 (emtp 
				    tp:ea524bc6-1e65-4c90-8772-ab94c916a375
				    (
				       (emter:item->index item)
				       index 
				       (car ref)))


				 ;;If an expression is referenced and
				 ;;hasn't been explored yet, put it
				 ;;onto PENDING so we will explore it.
				 (unless nil
				    (setf 
				       (emter:src-info->visited-p 
					  its-src-info)
				       t)
				    (push 
				       (nth index superitem-list) 
				       pending))))
		     
			   (emter:value-always
			      (let
				 ((index (second ref)))
				 (emtp 
				    tp:ea524bc6-1e65-4c90-8772-ab94c916a375
				    (
				       (emter:item->index item)
				       index 
				       (car ref)))
				 (emter:code-source-incf-count 
				    (emter:superitem->src-info 
				       (nth index superitem-list))
				    'value))))))))))
      

      ;;Sort `needed-items' by index of the underlying piece, lowest first.
      (setq needed-items
	 (sort* needed-items #'<
	    :key
	    #'(lambda (info)
		 (emter:item->index (emter:superitem->item info)))))

      ;;Root first, create local forms for each source
      (dolist (source needed-items)
	 (emter:destructure-superitem (item src-info) source

	    (emter:code-src-setup-local-form
	       (emter:src-info->expression-info src-info)
	       #'(lambda ()
		    (emter:build-expression-code source superitem-list)))
	    
	    (emter:code-src-setup-local-form
	       (emter:src-info->value-info src-info)
	       ;;Will be a hook to allow making tagnames definitions.
	       #'(lambda ()
		    (emter:item->value item)))))
      
      ;;Build whole code.
      (let
	 ((form (progn)))
	 ;;Traverse `needed-items' leaves first (ie, opposite of
	 ;;before).  Really, we're reducing an initial value across
	 ;;the list.
	 (dolist (superitem (reverse needed-items))
	    (emter:destructure-superitem (item src-info) superitem
	       (setq form
		  (emter:code-src-add-to-form 
		     form 
		     (emter:src-info->expression-info src-info)))
	       (setq form
		  (emter:code-src-add-to-form 
		     form 
		     (emter:src-info->value-info src-info)))))
	 
	 ;;Add any whole-scope management (by generators)

	 ;;Return form.  In the future, may produce, reduce, and
	 ;;return an alist of which one part is this form, so it's
	 ;;extensible
	 form)))


;;;_   , Test helpers 
;;$$MOVE ME
;;;_    . emter:build-code:th
(defun emter:build-code:th (input-list)
   ""
   
   (emtp:eval
      (dolist (input input-list)
	 ;;Build item.
	 (destructuring-bind (should &rest expression) input
	    (emter:record-item expression (eval expression))
	    ;;SHOULD tells us to set up a should.
	    (when should
	       (setf
		  (emter:item->should
		     (car (last emter:value-list)))
		  t))))
      ;;Testpoint to skip displaying the item.
      (tp* (:id tp:8efac1de-09d4-4865-bea2-d3df2b211ad3 :count nil)())))

;;;_    . emter:build-code:thm
(defmacro emter:build-code:thm (input-list bindings &rest body)
   "Run BODY with `emter:value-list' in a particular state.
INPUT-LIST elements are in the form (SHOULD . EXPRESSION).
BINDINGS are as for a `let'"
   
   `(let ((emter:value-list ())
	    ,@bindings)
       (emter:build-code:th ,input-list)
       ,@body))
;;;_    . emter:build-code:th2
(defun emter:build-code:th2 (precedence-pairs)
   ""
   (emtp:eval
      (emter:build-code)
      ;;Testpoint per dependency
      (tp*
	 (:id tp:ea524bc6-1e65-4c90-8772-ab94c916a375
	    :count 
	    (length precedence-pairs))
	 (got-index want-index type)
	 (emt:tp:collect (list got-index want-index type)))
      (finally (:bindings ((precedence-pairs precedence-pairs))) (depend-set)
	 (assert
	    (emth:sets= depend-set 
	       precedence-pairs)
	    t))))
;;;_    . emter:build-code:thm2
(defmacro* emter:build-code:thm2 
   ((&key inputs bindings precedence-pairs sym) &rest body)
   ""
   
   `(emter:build-code:thm
       ,inputs
       ,bindings
       (let
	  ((,sym
	      (emter:build-code:th2 ,precedence-pairs)))
	  ,@body)))


;;;_    . emter:build-code:th:w-bindings-passes-p
(defun emter:build-code:th:w-bindings-passes-p 
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
	    (emth:gives-error
	       (eval form))))
      
      ;;Test that it errored just if it should.
      (if should-pass-p
	 (assert (not errs-p)
	    nil "Form %s failed" form)
	 (assert errs-p
	    nil "Form %s wrongly succeeded" form))))



;;;_ , emter:export-code
(defun emter:export-code ()
   "Build and export code"
   ;;For now, just use one exporter
   (let*
      ()
      
      ))

;;;_ , Accessors
;;;_  . emter:value
(defsubst emter:value (index)
   ""
   (emter:item->value
      (nth index emter:value-list)))

;;;_  . emter:value-always
(defalias 'emter:value-always 'emter:value)
;;;_ , Exporters
;;;_  . emter:export-to-kill-ring
(defun emter:export-to-kill-ring (code)
   ""
   
   (let*
      ()
      
      ))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/trail)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/trail.el ends here
