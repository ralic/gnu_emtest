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

;; This code is still under construction and always will be.


;;;_ , Requires

(require 'emtest/testhelp/misc) ;;For `emth:collect-in-tree'
(progn
   (eval-when-compile
      (require 'emtest/testhelp/testpoint/requirer))
   (emtp:require))
(eval-when-compile
   (require 'cl))

;;;_. Body
;;;_ , Structures
(defstruct (emt:ed:trl:item
	      (:copier nil)
	      (:conc-name emt:ed:trl:item->)
	      (:constructor emt:ed:trl:make-item))
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
(defconst emt:ed:trl:buf-name "*Trail buffer*" 
   "Buffer name for emter" )
;;;_ , Variables
;;;_  . emt:ed:trl:value-list
(defvar emt:ed:trl:value-list () 
   "List of values for emter" )
;;;_  . emter/eg-narrowing
;;Just for emtg generator
(defvar emter/eg-narrowing () "" )
;;;_ , emt:ed:trl:record-item
(defun emt:ed:trl:record-item (expression value)
   ""

   (let*
      (  (index (length emt:ed:trl:value-list))
	 (item
	    (emt:ed:trl:make-item
	       :expression expression
	       :value value
	       :index index)))
      ;;It must go at the end, to preserve correct indexing.
      (setq emt:ed:trl:value-list
	 (append emt:ed:trl:value-list (list item)))

      (emtp tp:8efac1de-09d4-4865-bea2-d3df2b211ad3 ()
	 (let
	    ((buf
		(get-buffer-create emt:ed:trl:buf-name)))
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


;;;_ , emt:ed:trl:eval-expression
(defun emt:ed:trl:eval-expression (eval-expression-arg)
   "Evaluate EVAL-EXPRESSION-ARG and store it on the trail"
   (interactive
      (list (read-from-minibuffer "Eval: "
	       nil read-expression-map t
	       'read-expression-history)))
   (let
      ((val (eval eval-expression-arg)))
      (emt:ed:trl:record-item eval-expression-arg val)))

;;;_  . Tests

;;Usability tests only.

;;;_ , Building code
;;;_  . Structures
(defstruct (emt:ed:trl:superitem 
	      (:type list)
	      (:copier nil)
	      (:conc-name emt:ed:trl:superitem->)
	      (:constructor emt:ed:trl:make-superitem))
   ""
   item
   src-info)

(defstruct (emt:ed:trl:code-source
	      (:copier nil)
	      (:conc-name emt:ed:trl:code-source->)
	      (:constructor emt:ed:trl:make-code-source))
   "Data about a piece of code source, either value or expression."

   ;;(which () :type (member expression value))
   (count 0 :type integer)
   (binding-p () :type bool)
   surface-form
   real-form)
(defstruct (emt:ed:trl:src-info
	      (:copier nil)
	      (:conc-name emt:ed:trl:src-info->)
	      (:constructor emt:ed:trl:make-src-info))
   "Data about an item relative to  a single generation run."
   (item            () :type emt:ed:trl:item)
   (visited-p       () :type bool)
   (refs            () :type (repeat *))
   (value-info      () :type emt:ed:trl:code-source)
   (expression-info () :type emt:ed:trl:code-source))

;;;_  . emt:ed:trl:destructure-superitem
(defmacro emt:ed:trl:destructure-superitem (fields superitem &rest body)
   ""
   ;;$$ANNOTATE ME - move edebug spec into here
   `(destructuring-bind ,fields ,superitem ,@body))
(def-edebug-spec emt:ed:trl:destructure-superitem
   (sexp form body))

;;;_  . emt:ed:trl:build-expression-code 
(defun emt:ed:trl:build-expression-code (source indexing)
   "Builds a form corresponding to an expression.
Will do substitutions.
Handles making `should' or `assert' as well."
   (emt:ed:trl:destructure-superitem (item src-info) source
      (let*
	 ((raw-form
	     (emt:ed:trl:item->expression item))
	    (refs
	       (emt:ed:trl:src-info->refs src-info))
	    ;;Alist mapping each ref to the surface form it
	    ;;should be replaced by.
	    (ref-alist
	       (mapcar
		  #'(lambda (ref)
		       (cons 
			  ref
			  (emt:ed:trl:code-source->surface-form
			     (emt:ed:trl:src-info->expression-info
				(emt:ed:trl:superitem->src-info
				   ;;$$Encap this lookup.
				   (nth (second ref) indexing))))))
		  refs))
	    (form-1
	       ;;$$Could :test #'eq
	       (sublis ref-alist raw-form))
	    (form-2
	       (if
		  (emt:ed:trl:item->should item)
		  ;;This might be parameterized - make "assert" or
		  ;;"should" depending on something.
		  `(assert 
		      ,form-1
		      ;;Must only show-args for forms that
		      ;;are lists.
		      ,(if (listp form-1) t nil))
		  form-1)))
	 form-2)))

;;;_  . emt:ed:trl:code-src-setup-local-form
(defun emt:ed:trl:code-src-setup-local-form (code-src build-core-f)
   "Build the local forms for this.
Assumes everything it references has already been built for.
This looks at ref count and does nothing if = 0."
   
   (when
      (> (emt:ed:trl:code-source->count code-src) 0)
      (let
	 (
	    ;;Maybe make a name.  `nil' or a symbol.  Punt for now.
	    (name nil))

	 ;;NAME could take item's per-generator data into account.
	 ;;Let a hook build it via `run-hook-with-args-until-success'.
	 ;;Each is passed item and code-source.
	 '(> (emt:ed:trl:code-source->count code-src) 1)
		  
	 ;;Callback to get deep form.
	 (setf
	    (emt:ed:trl:code-source->real-form code-src)
	    (funcall build-core-f))
		  
	 ;;Set up surface stuff.
	 (if name
	    (progn 
	       (setf
		  (emt:ed:trl:code-source->surface-form code-src)
		  name)
	       (setf
		  (emt:ed:trl:code-source->binding-p code-src)
		  t))
	    (setf
	       (emt:ed:trl:code-source->surface-form code-src)
	       (emt:ed:trl:code-source->real-form code-src))))))

;;;_  . emt:ed:trl:code-src-add-to-form
(defun emt:ed:trl:code-src-add-to-form (form code-src)
   ""
   (if (> (emt:ed:trl:code-source->count code-src) 0)
      (progn
	 ;;For bindings, add a `let*' if there is none
	 ;;(transform a progn), otherwise add to its
	 ;;bindings.  Add new ones to the front.

	 ;;For needed-items, always add them in a progn,
	 ;;creating one if needed.

	 ;;For now, incoming form is assumed to be a `progn'
	 `(progn 
	     ,(emt:ed:trl:code-source->surface-form code-src)
	     ,@(cdr form)))
      form))

;;;_  . emt:ed:trl:src-info-incf-count
(defun emt:ed:trl:code-source-incf-count (src-info which)
   ""

   (let
      ((code-source
	  (ecase which 
	     (value
		(emt:ed:trl:src-info->value-info 
		   src-info))
	     (expression
		(emt:ed:trl:src-info->expression-info 
		   src-info)))))
   (incf (emt:ed:trl:code-source->count code-source))))

;;;_  . emt:ed:trl:build-code 
(defun emt:ed:trl:build-code ()
   ""

   (let*
      (
	 ;;An indexing list, must have same indexing that trail has.
	 ;;Of type (repeat emt:ed:trl:superitem)
	 (superitem-list
	    (mapcar 
	       #'(lambda (x)
		    (emt:ed:trl:make-superitem
		       :item x 
		       :src-info
		       (emt:ed:trl:make-src-info
			  :item x
			  :expression-info
			  (emt:ed:trl:make-code-source)
			  :value-info
			  (emt:ed:trl:make-code-source))))
	       
	       emt:ed:trl:value-list))

	 ;;List of the items marked "should" (ie, should be non-nil in a test)
	 ;;Of type (repeat emt:ed:trl:item)
	 (should-list
	    ;;Non-destructive.
	    (remove* nil emt:ed:trl:value-list
	       :test-not
	       #'(lambda (dummy item)
		    ;;Punt, because this field is still just a bool.
		    (emt:ed:trl:item->should item))))

	 ;;Initial set of needed-items.  
	 ;;Of type (repeat emt:ed:trl:superitem)
	 (pending
	    (mapcar
	       #'(lambda (item)
		    (let*
		       (  (superitem
			     (nth (emt:ed:trl:item->index item)
				superitem-list))
			  (src-info (emt:ed:trl:superitem->src-info superitem)))
		       
		       ;;Count is 1 because we will generate a should
		       ;;that refers to it.
		       (emt:ed:trl:code-source-incf-count 
			  src-info
			  'expression)
		       superitem))
	       should-list))
	 
	 ;;Items that will contribute to the final source.
	 ;;Of type (repeat emt:ed:trl:superitem)
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
	    (emt:ed:trl:destructure-superitem (item src-info) superitem
	       ;;Don't revisit items we already visited.  Since the
	       ;;flag means "explored" we can get it twice.
	       (unless (emt:ed:trl:src-info->visited-p src-info)
		  (setf (emt:ed:trl:src-info->visited-p src-info) t)

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
				       '(emt:ed:trl:value
					   emt:ed:trl:value-always))))
			    (emt:ed:trl:item->expression item))))
		     
		     ;;Store refs so that
		     ;;`emt:ed:trl:build-expression-code' needn't look for
		     ;;them again.
		     (setf
			(emt:ed:trl:src-info->refs src-info)
			refs)
	       
		     ;;For each ref...
		     (dolist (ref refs)
			;;Tests are interested in which items each
			;;item is seen to depend on.

			;;Increment the ref count of the thing it
			;;references
			(case (car ref)
			   (emt:ed:trl:value
			      (let*
				 ((index (second ref))
				    (its-src-info
				       (emt:ed:trl:superitem->src-info 
					  (nth index superitem-list))))
				 (emt:ed:trl:code-source-incf-count 
				    its-src-info
				    'expression)
				 (emtp 
				    tp:ea524bc6-1e65-4c90-8772-ab94c916a375
				    (
				       (emt:ed:trl:item->index item)
				       index 
				       (car ref)))


				 ;;If an expression is referenced and
				 ;;hasn't been explored yet, put it
				 ;;onto PENDING so we will explore it.
				 (unless nil
				    (setf 
				       (emt:ed:trl:src-info->visited-p 
					  its-src-info)
				       t)
				    (push 
				       (nth index superitem-list) 
				       pending))))
		     
			   (emt:ed:trl:value-always
			      (let
				 ((index (second ref)))
				 (emtp 
				    tp:ea524bc6-1e65-4c90-8772-ab94c916a375
				    (
				       (emt:ed:trl:item->index item)
				       index 
				       (car ref)))
				 (emt:ed:trl:code-source-incf-count 
				    (emt:ed:trl:superitem->src-info 
				       (nth index superitem-list))
				    'value))))))))))
      

      ;;Sort `needed-items' by index of the underlying piece, lowest first.
      (setq needed-items
	 (sort* needed-items #'<
	    :key
	    #'(lambda (info)
		 (emt:ed:trl:item->index (emt:ed:trl:superitem->item info)))))

      ;;Root first, create local forms for each source
      (dolist (source needed-items)
	 (emt:ed:trl:destructure-superitem (item src-info) source

	    (emt:ed:trl:code-src-setup-local-form
	       (emt:ed:trl:src-info->expression-info src-info)
	       #'(lambda ()
		    (emt:ed:trl:build-expression-code source superitem-list)))
	    
	    (emt:ed:trl:code-src-setup-local-form
	       (emt:ed:trl:src-info->value-info src-info)
	       ;;Will be a hook to allow making tagnames definitions.
	       #'(lambda ()
		    (emt:ed:trl:item->value item)))))
      
      ;;Build whole code.
      (let
	 ((form (progn)))
	 ;;Traverse `needed-items' leaves first (ie, opposite of
	 ;;before).  Really, we're reducing an initial value across
	 ;;the list.
	 (dolist (superitem (reverse needed-items))
	    (emt:ed:trl:destructure-superitem (item src-info) superitem
	       (setq form
		  (emt:ed:trl:code-src-add-to-form 
		     form 
		     (emt:ed:trl:src-info->expression-info src-info)))
	       (setq form
		  (emt:ed:trl:code-src-add-to-form 
		     form 
		     (emt:ed:trl:src-info->value-info src-info)))))
	 
	 ;;Add any whole-scope management (by generators)

	 ;;Return form.  In the future, may produce, reduce, and
	 ;;return an alist of which one part is this form, so it's
	 ;;extensible
	 form)))



;;;_ , emt:ed:trl:export-code
(defun emt:ed:trl:export-code ()
   "Build and export code"
   ;;For now, just use one exporter
   (let*
      ()
      
      ))

;;;_ , Accessors
;;;_  . emt:ed:trl:value
(defsubst emt:ed:trl:value (index)
   ""
   (emt:ed:trl:item->value
      (nth index emt:ed:trl:value-list)))

;;;_  . emt:ed:trl:value-always
(defalias 'emt:ed:trl:value-always 'emt:ed:trl:value)
;;;_ , Exporters
;;;_  . emt:ed:trl:export-to-kill-ring
(defun emt:ed:trl:export-to-kill-ring (code)
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
