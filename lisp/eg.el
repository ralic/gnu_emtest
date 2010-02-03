;;;_ eg.el --- Examples managements utility for tester

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp

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
;;;###autoload (require 'cl)
(require 'cl)
(require 'emt-accumulator)
;;For testing
(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest    (&rest dummy))
    (defmacro rtest:if-avail   (&rest dummy)))
(rtest:if-avail
   (require 'el-mock))

;;;_. Body

;;;_ , Variables
;;;_  . emt:eg:all-examples
(defvar emt:eg:all-examples
   ()
   "List of all examples.
Don't change this except thru `emt:eg:narrow' and its worker functions" )
(defvar emt:eg:all-prpty-makers
   ()
   "List of property-makers, which will apply properties to any new element.
Each element is of the type `emt:eg:example'" )
;;;_  . emt:eg:tagset
(defconst emt:eg:tagset nil 
   "Tagset, to make general constraints known to `emt:eg'.
This variable is `let' in appropriate scopes" )

;;;_ , Types
;;;_  . Example structure as it occurs on `emt:eg:all-examples'
(defstruct (emt:example. (:type list))
   "The structure of an example on the list"
   definer-id
   tagset
   value
   property-list
   )

;;Properties' values are a list (Prop-sym value)

;;;_  . Helper return-type
(defstruct (emt:eg:helper-rettype. (:copier nil))
   ""
   (value-info () :type (repeat emt:eg:valuedef-type.))
   (property-info () :type (repeat emt:eg:valuedef-type.)))

(defstruct (emt:eg:valuedef-type. (:copier nil))
   "Data for defining an example's value or a property"
   (tagset () :type (repeat *))
   value-form)


;;;_ , Example definer
;;;_  . Test data

;;For bootstrap reasons, these examples can't be defined with
;;`emt:eg:define'

(defconst emt:eg:define:td:typical-helper-retval
   (make-emt:eg:helper-rettype.
      :value-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset '((dummy-tag 1))
	    :value-form 12)))
   
   "" )

(defconst emt:eg:define:td:docstring-1
   "A dummy docstring"
   "" )

;;$$Add the other test data here.

;;;_  .  Test helper emt:eg:th:validate-helper-retval
;;$$Could just use deep-type-check
(defun emt:eg:th:validate-helper-retval (retval)
   "Validate a helpers' return value as the right type"
   (and
      (emt:eg:helper-rettype.-p retval)
      (let
	 ((value-info (emt:eg:helper-rettype.-value-info retval)))
	 (and
	    (listp value-info)
	    (every
	       #'emt:eg:valuedef-type.-p
	       value-info)))))


;;;_   , Tests
(rtest:deftest emt:eg:th:validate-helper-retval

   (  "Validate this against `emt:eg:define:td:typical-helper-retval'."
      (emt:eg:th:validate-helper-retval
	 emt:eg:define:td:typical-helper-retval))

   (  "Situation: We get a retval that's correct
Response: Return non-nil."
      (emt:eg:th:validate-helper-retval
	 (make-emt:eg:helper-rettype.
	    :value-info
	    (list
	       (make-emt:eg:valuedef-type.
		  :tagset
		  ()
		  :value-form
		  'the-value-form)))))
   

   (  "Situation: We get a retval that's displaced one place too high
Response: Return nil."
      (not
	 (emt:eg:th:validate-helper-retval
	    (make-emt:eg:helper-rettype.
	       :value-info
	       (make-emt:eg:valuedef-type.
		  :tagset
		  ()
		  :value-form
		  'the-value-form)))))

   (  "Situation: We get a retval whose contents are messed up
Response: Return nil."
      (not
	 (emt:eg:th:validate-helper-retval
	    (make-emt:eg:helper-rettype.
	       :value-info
	       (list
		  (list
		     'value 
		     ()
		     13))))))

   

   ;;YAGNI: Could also validate that an expected tagset appears
   ;;everywhere.

   )

;;;_  . emt:eg:combine-tagsets
(defsubst emt:eg:combine-tagsets (tagset +tagset)
   ""
   (append +tagset tagset))
;;;_   , Tests
;;It's direct
;;;_  . Handle individual markings
;;;_   , emt:see-item
(defun emt:see-item (tagset +tagset value-form &rest others)
   ""
   (make-emt:eg:helper-rettype.
      :value-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset
	    (emt:eg:combine-tagsets tagset +tagset)
	    :value-form
	    value-form))))

;;;_    . Tests
(rtest:deftest emt:see-item

   (  "Behavior: The return value validates OK."
      (emt:eg:th:validate-helper-retval
	 (emt:see-item () () 'the-value-form)))
   
   ;;Punt `others' for now.  Later, test that it can return also a doc
   ;;with the same tagset.

   )
;;;_   , emt:see-doc
(defun emt:see-doc (tagset +tagset doc-form)
   ""
   (make-emt:eg:helper-rettype.
      :property-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset
	    (emt:eg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'documentation ,doc-form)))))

;;;_    . Tests
(rtest:deftest emt:see-doc

   (  "Behavior: The return value validates OK."
      (emt:eg:th:validate-helper-retval
	 (emt:see-doc () () "A doc string")))

   ;;For now, punt but be structurally correct
   
   )
;;;_   , emt:see-transparent-tags
(defun emt:see-transparent-tags (tagset +tagset form)
   ""
   (make-emt:eg:helper-rettype.
      :property-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset
	    (emt:eg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'transparent-tags ',form)))))
;;;_   , emt:see-type-must-be
(defun emt:see-type-must-be (tagset +tagset type-spec)
   ""
   (make-emt:eg:helper-rettype.
      :property-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset
	    (emt:eg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'type-must-be ',type-spec)))))
;;;_    . Tests
(rtest:deftest emt:see-type-must-be

   (  "Situation: In definer, item mismatches type.
Type spec is before item spec.
Response: error."
      (emt:eg:define:th:with-empty-tagset ()
	 (assert
	    (emt:gives-error
	       (emt:eg:define
		  dummy-id ()
		  (type-must-be ((dummy-type-tag a)) integer)
		  (item
		     ((dummy-type-tag a))
		     'dummy-1-type-a))
	       wrong-type-argument))
	 t))

   (  "Situation: In definer, item mismatches type.
Item spec is before type spec.
Response: error."
      (emt:eg:define:th:with-empty-tagset ()
	 (assert
	    (emt:gives-error
	       (emt:eg:define
		  dummy-id ()
		  (item
		     ((dummy-type-tag a))
		     'dummy-1-type-a)
		  (type-must-be ((dummy-type-tag a)) integer))
	       wrong-type-argument))
	 t))
   
   (  "Situation: In definer, item matches type
Response: item is usable as normal."
      (emt:eg:define:th:with-empty-tagset ()

	 (emt:eg:define
	    dummy-id ()
	    (type-must-be ((dummy-type-tag a)) symbol)
	    (item
	       ((dummy-type-tag a))
	       'dummy-1-type-a))
	 (assert
	    (not
	       (emt:gives-error
		  (emt:eg (dummy-type-tag a)))))
	 t))
   

   )
;;;_   , emt:see-group

(defun emt:see-group (tagset +tagset &rest definers)
   ""

   (let
      (  
	 (full-tagset
	    (emt:eg:combine-tagsets tagset +tagset)))
      
      (emt:accumulator:collect
	 #'(lambda (governor &rest d)
	      (let
		 ((fun
		     (case governor
			(item  #'emt:see-item)
			(doc   #'emt:see-doc)
			(group #'emt:see-group)
			(transparent-tags
			   #'emt:see-transparent-tags)
			(type-must-be 
			   #'emt:see-type-must-be))))
		 (apply fun full-tagset d)))
	 definers
	 'emt:eg:helper-rettype.)))


;;;_    . Tests
(rtest:deftest emt:see-group
   
   (  "Behavior: The return value validates OK."
      (emt:eg:th:validate-helper-retval
	 (emt:see-group () () '(item () the-value-form))))
   

   )

;;;_  . Handle adding definitions
;;;_   , emt:eg:remove-earlier-defs
(defun emt:eg:remove-earlier-defs (id)
   ""
   (setq emt:eg:all-examples
      (remove* id
	 emt:eg:all-examples
	 :key #'emt:example.-definer-id))
   (setq emt:eg:all-prpty-makers
      (remove* id
	 emt:eg:all-prpty-makers
	 :key #'emt:example.-definer-id)))

;;;_    . Tests
(put 'emt:eg:remove-earlier-defs 'rtest:test-thru
   'emt:eg:define)
;;;_   , emt:eg:propty-match-ctxt-p
(defun emt:eg:propty-match-ctxt-p (tagset prpty-maker)
   ""

   (every
      #'(lambda (one-kv)
	   (emt:eg:some-kv-matches
	      one-kv
	      tagset))
      (emt:example\.-tagset prpty-maker)))

;;;_    . Tests
(put 'emt:eg:propty-match-ctxt-p 'rtest:test-thru
   'emt:eg:find-properties)
;;;_   , emt:eg:find-properties
(defun emt:eg:find-properties (target-tagset prpty-makers)
   "Given a tagset, return a list of the properties that apply for it"
   
   (let*
      ;;Filter the property-makers by whether their tagset applies to
      ;;example's tagset (Ie, the reverse of finding example by
      ;;tagset)
      ((relevant-props
	  (remove* target-tagset prpty-makers
	     :test-not
	     #'emt:eg:propty-match-ctxt-p)))
      ;;Return a list of all their values in any order.
      (mapcar
	 #'emt:example.-value
	 relevant-props)))

;;;_    . Tests
(rtest:deftest emt:eg:find-properties

   (  "Situation: target-tagset exactly matches the only property.
Response: Return list has just the value of that property."
      (equal
	 (emt:eg:find-properties
	    '((dummy-tag 1))
	    (list
	       (make-emt:example.
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value '(prop-sym1 val1))))
	 
	 '((prop-sym1 val1))))
   
   )
;;;_   , emt:eg:describe-item-context
(defun emt:eg:describe-item-context (v)
   ""
   (format "Item %s tagged %s in eg definition %S"
      (emt:example.-value v)
      (emt:example.-tagset v)
      (emt:example.-definer-id v)))


;;;_   , emt:eg:apply-proplist-specials-to-example
(defun emt:eg:apply-proplist-specials-to-example (props example)
   ""
   (dolist (p props)
      (when
	 (eq (first p) 'type-must-be)
	 (require 'deep-type-checker)
	 (condition-case err
	    (emt:deep-type:check-f
	       (emt:example.-value example)
	       (second p))
	    (wrong-type-argument
	       (signal
		  (car err)
		  (list
		     (concat
			(emt:eg:describe-item-context example)
			(format " is not a %S" (second p))))))))))


;;;_    . Tests
(put 'emt:eg:apply-proplist-specials-to-example 'rtest:test-thru
   'emt:see-type-must-be)
;;;_   , emt:eg:apply-prpty-makers-to-examples
(defun emt:eg:apply-prpty-makers-to-examples (prpty-makers examples)
   "Apply the property-makers to each example whose tagset matches.
Return the new list of examples."

   (mapcar
      ;;Adds appropriate properties to one example.
      #'(lambda (x)
	   (let
	      ((props
		  (emt:eg:find-properties 
		     (emt:example\.-tagset x)
		     prpty-makers)))

	      (emt:eg:apply-proplist-specials-to-example props x)
	      
	      (callf append 
		 (emt:example.-property-list x)
		 props)
	      x))
      examples))
;;;_    . Test helper emt:eg:apply-prpty-makers-to-examples:th
(defun emt:eg:apply-prpty-makers-to-examples:th (prpty-maker example)
   "Like `emt:eg:apply-prpty-makers-to-examples' but deals only with
singletons.  Returns a singleton"
   (car
      (emt:eg:apply-prpty-makers-to-examples
	 (list prpty-maker)
	 (list example))))

;;;_    . Tests
(rtest:deftest emt:eg:apply-prpty-makers-to-examples

   (  "Situation: X's tagset matches PROP's.
Response: That property is added to X's property-list."
      (emt:eg:define:th:with-empty-tagset ()
	 (let*
	    (
	       (docstring emt:eg:define:td:docstring-1)
	       (new-x
		  (emt:eg:apply-prpty-makers-to-examples:th
		     (make-emt:example.
			:definer-id 'dont-care
			:value `(doc ,docstring)
			:tagset '((dummy-tag 1)))
		     (make-emt:example.
			:definer-id 'dont-care
			:value 'dont-care
			:tagset '((dummy-tag 1))
			:property-list ()))))
	    
	    (equal
	       (emt:example.-property-list new-x)
	       (list
		  `(doc ,docstring))))))

   (  "Situation: X's tagset matches PROP's.
X already has a different property.
Response: The new property is added to X's property-list."
      (emt:eg:define:th:with-empty-tagset ()
	 (let*
	    (
	       (docstring emt:eg:define:td:docstring-1)
	       (new-x

		  (emt:eg:apply-prpty-makers-to-examples:th

		     (make-emt:example.
			:definer-id 'dont-care
			:value `(doc ,docstring)
			:tagset '((dummy-tag 1)))

		     (make-emt:example.
			:definer-id 'dont-care
			:value 'dont-care
			:tagset '((dummy-tag 1))
			:property-list (list '(other-prop other-value))))))
	    
	    (rtest:sets=
	       (emt:example.-property-list new-x)
	       (list
		  `(doc ,docstring)
		  '(other-prop other-value))))))
   
   (  "Situation: X's tagset does not match PROP's.
Response: X is unchanged."
      (emt:eg:define:th:with-empty-tagset ()
	 (let*
	    (
	       (docstring emt:eg:define:td:docstring-1)
	       (new-x

		  (emt:eg:apply-prpty-makers-to-examples:th

		     (make-emt:example.
			:definer-id 'dont-care
			:value `(doc ,docstring)
			:tagset '((dummy-tag 1)))

		     (make-emt:example.
			:definer-id 'dont-care
			:value 'dont-care
			:tagset '((dummy-tag 2))
			:property-list ()))))
	    
	    (equal
	       (emt:example.-property-list new-x)
	       ()))))
   
   )
;;;_   , emt:eg:see-new-prpty-makers
(defun emt:eg:see-new-prpty-makers (prpty-makers)
   ""
   ;;Apply the property-makers to existing examples. 
   (setq emt:eg:all-examples
      (emt:eg:apply-prpty-makers-to-examples
	     prpty-makers
	     emt:eg:all-examples))

   ;;Record the property-makers.
   (setq emt:eg:all-prpty-makers
      (append prpty-makers emt:eg:all-prpty-makers)))

;;;_   , emt:eg:see-a-new-example
(defun emt:eg:see-a-new-example (x)
   ""
   (push x emt:eg:all-examples))
;;;_    . Tests
;;It's direct

;;;_   , emt:eg:see-new-examples (Obsolete?)
'
(defun emt:eg:see-new-examples (examples)
   ""
   ;;Assume examples have already been given properties.
   (setq emt:eg:all-examples
      (append
	 examples
	 emt:eg:all-examples)))

;;;_    . Tests
'
(put 'emt:eg:see-new-examples 'rtest:test-thru
   'emt:eg:define)

;;;_   , emt:eg:valuedef->example

(defun emt:eg:valuedef->example (valuedef id all-prpty-makers)
   ""
   (let* 
      (  (item-tagset 
	    (emt:eg:valuedef-type.-tagset valuedef))
	 (props
	    (emt:eg:find-properties item-tagset
	       all-prpty-makers))
	 (tagset
	    (emt:eg:tagset-strip-transparents 
	       item-tagset
	       props)) 
	 (value-form
	    (emt:eg:narrow-f
	       `',tagset
	       ;;body - the form, inside an emt:eg:narrow that
	       ;;constrains the tagset it uses to find other
	       ;;examples.
	       (list 
		  (emt:eg:valuedef-type.-value-form valuedef))))
	 (value
	    (eval value-form))
	 (x
	    (make-emt:example.
	       :definer-id 
	       id
	       :tagset    
	       item-tagset
	       :value
	       value 
	       :property-list 
	       props)))

      (emt:eg:apply-proplist-specials-to-example props x)
      x))
;;;_    . Tests

(put 'emt:eg:valuedef->example 'rtest:test-thru
   'emt:eg:define)

;;;_  . Error symbols

;;;_   , emt:eg:err:not-found
(put 'emt:eg:err:not-found 'error-conditions
   '(error emt:eg:err emt:eg:err:not-found))
(put 'emt:eg:err:not-found 'error-message
   "Could not find an example")

;;;_   , emt:eg:err:too-many
(put 'emt:eg:err:too-many 'error-conditions
   '(error emt:eg:err emt:eg:err:too-many))
(put 'emt:eg:err:too-many 'error-message
   "More than one example found")

;;;_  . emt:eg:try-valuedef->example
(defun emt:eg:try-valuedef->example (v id all-prpty-makers)
   ""
   (condition-case err
      (emt:eg:see-a-new-example
	 (emt:eg:valuedef->example v id all-prpty-makers))
      ('emt:eg:err:not-found
	 (push v emt:eg:delayed-examples))))


;;;_   , Test helper
(defmacro emt:eg:try-valuedef->example:th (&rest body)
   "
Can't use `emt:eg' in body."
   
   `(let
       (  (emt:eg:all-examples ())
	  (emt:eg:delayed-examples ()))
       ,@body))


;;;_   , Tests
(rtest:deftest emt:eg:try-valuedef->example

   (  "Situation: Form has no error.
Response: Put it on the examples list `emt:eg:all-examples'."
      (emt:eg:try-valuedef->example:th
	 (emt:eg:try-valuedef->example
	    (make-emt:eg:valuedef-type.
	       :value-form
	       12)
	    'dummy-id
	    ())
	 
	 (equal (length emt:eg:all-examples) 1)))
   

   (  "Situation: Form throws an error of type `emt:eg:err-not-available'
Response: Put it on the delayed list `emt:eg:delayed-examples'.
The error doesn't escape."
      (emt:eg:try-valuedef->example:th
	 (emt:eg:try-valuedef->example
	    (make-emt:eg:valuedef-type.
	       :value-form
	       '(signal 'emt:eg:err:not-found nil))
	    'dummy-id
	    ())
	 (equal (length emt:eg:delayed-examples) 1)))
   


   )
;;;_  . emt:eg:valuedef->property
(defun emt:eg:valuedef->property (valuedef id)
   ""

   (let
      (  (tagset 
	    (emt:eg:valuedef-type.-tagset    valuedef))
	 (value-form
	    (emt:eg:valuedef-type.-value-form valuedef)))
	      
      (make-emt:example.
	 :definer-id 
	 id
	 :tagset    
	 tagset
	 :value
	 (eval value-form) 
	 :property-list 
	 ())))

;;;_   , Tests
(put 'emt:eg:valuedef->property 'rtest:test-thru
   'emt:eg:define)
;;;_  . emt:eg:tagset-strip

(defun emt:eg:tagset-strip (tagset transparent-tags)
   ""
   (remove* transparent-tags tagset 
      :test
      #'(lambda
	   (tts 1-tagset)
	   (memq
	      (if
		 (consp 1-tagset)
		 (car 1-tagset)
		 1-tagset)
	      tts))))
;;;_   , Tests
(put 'emt:eg:tagset-strip 'rtest:test-thru
   'emt:eg:tagset-strip-transparents)

;;;_  . emt:eg:tagset-strip-transparents
(defun emt:eg:tagset-strip-transparents (tagset props)
   "Return TAGSET with any transparent tags removed.

TAGSET must be a kv-list suitable for `emt:eg:narrow'.
Transparent tags are exactly those tags named in the
`transparent-tags' property (if any) in PROPS."

   '
   (let
      ((cell (assoc 'transparent-tags props)))
      (if cell (second cell) ()))
   
   (let*
      (  ;;Get them even if they are in several applying properties.
	 (transparent-tags
	       (apply #'append
		  (mapcar 
		     #'(lambda (cell)
			  (if (eq (first cell) 'transparent-tags)
			     (second cell)
			     ()))
		     props)))
	 ;;Subtract the tags named in transparent-tags.
	 (new-tagset
	    (emt:eg:tagset-strip tagset transparent-tags)))
      
      new-tagset))

;;;_   , Tests
(rtest:deftest emt:eg:tagset-strip-transparents
   ;;
   (  "Situation: No transparent-tags among props
Response: Returns exactly the tagset."
      (let
	 ((tagset '((dummy-tag 1))))
	 (equal
	    (emt:eg:tagset-strip-transparents tagset ())
	    tagset)))


   (  "Situation: No transparent-tags among props
Response: Returns tagset with matching tags part removed."
      (let
	 ((tagset '((dummy-tag 1)(dummy-tag-2 2))))
	 (equal
	    (emt:eg:tagset-strip-transparents 
	       tagset
	       '((transparent-tags (dummy-tag))))
	    
	    '((dummy-tag-2 2)))))

   (  "Situation: tagset contains a singleton item,
Behavior: Still works."
      (let
	 ((tagset '(dummy-tag)))
	 (equal
	    (emt:eg:tagset-strip-transparents tagset ())
	    tagset))))

;;;_  . emt:eg:define-f
(defun emt:eg:define-f (id group-args)
   ""
   (unless (symbolp id)
      (error "emt:eg:define ID must be a symbol"))
   (let
      ((item-maker-list (apply #'emt:see-group () group-args)))

      ;;Remove existing things that had this definer-id.  They're
      ;;from an earlier run of this definer.
      (emt:eg:remove-earlier-defs id)

      ;;We do this on a list, for historical reasons.
      ;;Put any new property-makers in place
      (emt:eg:see-new-prpty-makers
	 (mapcar
	    #'(lambda (v)
		 (emt:eg:valuedef->property v id))
	    (emt:eg:helper-rettype.-property-info item-maker-list)))

      (let*
	 (  (emt:eg:delayed-examples ())
	    (pending-valuedefs 
	       (emt:eg:helper-rettype.-value-info item-maker-list))
	    (last-num-delayed (length pending-valuedefs)))

	 (while pending-valuedefs
	    ;;Put any new examples in place.  This uses the
	    ;;property-makers including any we just added.
	    (dolist (v pending-valuedefs)
	       (emt:eg:try-valuedef->example v id
		  emt:eg:all-prpty-makers))

	    ;;How many are still left to do?
	    (let
	       ((num-delayed (length emt:eg:delayed-examples)))
	       (cond
		  ;;All done.
		  ((= num-delayed 0)
		     (setq pending-valuedefs ()))
		  ;;Not done but making progress.  Set up to try
		  ;;again.
		  ((< num-delayed last-num-delayed)
		     (setq last-num-delayed num-delayed)
		     (setq pending-valuedefs emt:eg:delayed-examples)
		     (setq emt:eg:delayed-examples ()))
		  ;;Not even making progress.
		  (t
		     (error 
			"In eg definition %S, some examples' values could not be resolved: %s"
			id
			(mapconcat
			   #'(lambda (v)
				(format "%s within %s"
				   (emt:eg:valuedef-type.-value-form v)
				   (emt:eg:valuedef-type.-tagset v)))
			   emt:eg:delayed-examples
			   "\n"))))
	       )))))

;;;_   , Tests
(put 'emt:eg:define-f 'rtest:test-thru
   'emt:eg:define)
;;;_  . emt:eg:define

(defmacro emt:eg:define (id &rest group-args)
   "Define examples.
DEFINERS are implicitly in a `group' structure.
Should document the structure here."

   `(emt:eg:define-f ',id ',group-args))




;;;_   , Test helper
(defmacro* emt:eg:define:th:with-empty-tagset
   ((&key examples) &rest body)
   ""
   
   `(let
       (  (emt:eg:all-examples        ,(or examples ()))
	  (emt:eg:all-prpty-makers     ()))
       ,@body))


;;;_   , Tests

(rtest:deftest emt:eg:define

   ;;Validation:
   (  "Validation: `emt:eg:all-examples' does not have anything defined
by the dummy definers (which deliberately re-use the same ID)."
      (not
	 (member* 'dummy-id emt:eg:all-examples
	    :key #'emt:example.-definer-id)))
   
   (  "Validation: `emt:eg:all-prpty-makers' does not have anything
defined by the dummy definers (which deliberately re-use the same ID)."
      (not
	 (member* 'dummy-id emt:eg:all-prpty-makers
	    :key #'emt:example.-definer-id)))
   
   ;;These tests let `emt:eg:all-examples' to known values so that
   ;;tagset is known.  We can't use emt:eg-style examples inside them.

   ;;Mock `emt:see-group', so we can just test the inserting
   ;;functionality.  
   (  "Situation: There are no examples in `emt:eg:all-examples'.
Afterwards: There is an example in `emt:eg:all-examples'."
      (with-mock
	 (stub emt:see-group =>
	    emt:eg:define:td:typical-helper-retval)
	 (emt:eg:define:th:with-empty-tagset ()
	    (emt:eg:define
	       dummy-id 
	       ;;Punt the other args
	       ()
	       ())

	    (and
	       (equal 
		  (length 
		     emt:eg:all-examples)
		  1)

	       (member
		  (make-emt:example.
		     :definer-id 'dummy-id
		     :tagset '((dummy-tag 1))
		     :value 12)
		  emt:eg:all-examples)
	       t))))
   
   
   (  "Situation: One example, different definer ID.
Afterwards: There are two examples in `emt:eg:all-examples'.  One is
the new one"
      (with-mock
	 (stub emt:see-group => emt:eg:define:td:typical-helper-retval)
	 (emt:eg:define:th:with-empty-tagset
	    (:examples
	       (list
		  (make-emt:example.
		     :definer-id 'dummy-id-2
		     :tagset '((dummy-tag 1))
		     :value 13)))
	    (emt:eg:define
	       dummy-id 
	       ;;Punt the other args
	       ()
	       ())

	    (and
	       (equal 
		  (length 
		     emt:eg:all-examples)
		  2)

	       (member
		  (make-emt:example.
		     :definer-id 'dummy-id
		     :tagset '((dummy-tag 1))
		     :value 12)
		  emt:eg:all-examples)
	       t))))
   
   

   (  "Situation: One example exists, same definer ID
Afterwards: There is only the new example."
      (with-mock
	 (stub emt:see-group => emt:eg:define:td:typical-helper-retval)
	 (emt:eg:define:th:with-empty-tagset
	    (:examples
	       (list
		  (make-emt:example.
		     :definer-id 'dummy-id ;;Same ID
		     :tagset '((dummy-tag 1))
		     :value 13)))
	    (emt:eg:define
	       dummy-id 
	       ;;Punt the other args
	       ()
	       ())

	    (and
	       (equal 
		  (length 
		     emt:eg:all-examples)
		  1)

	       (member
		  (make-emt:example.
		     :definer-id 'dummy-id
		     :tagset '((dummy-tag 1))
		     :value 12)
		  emt:eg:all-examples)
	       t))))



   ;;These tests test the same without mocks.  They rely on the
   ;;helpers working.

   (  "Situation: There are no examples in `emt:eg:all-examples'.
Afterwards: There is an example in `emt:eg:all-examples'."
      (emt:eg:define:th:with-empty-tagset ()
	 (emt:eg:define
	    dummy-id 
	    ;;The tagset.  Everything inside is tagged with this.
	    ((dummy-tag 1))
	    (item
	       ;;No further tagset given
	       ()
	       ;;Value
	       12))

	 (and
	    (equal 
	       (length 
		  emt:eg:all-examples)
	       1)

	    (member
	       (make-emt:example.
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value 12)
	       emt:eg:all-examples)
	    t)))

   
   (  "Situation: One example, different definer ID.
Afterwards: There are two examples in `emt:eg:all-examples'.  One is
the new one"
      (emt:eg:define:th:with-empty-tagset
	 (:examples
	    (list
	       (make-emt:example.
		  :definer-id 'dummy-id-2
		  :tagset '((dummy-tag 1))
		  :value 13)))
	 (emt:eg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (item
	       ;;No further tagset given
	       ()
	       ;;Value
	       12))

	 (and
	    (equal 
	       (length 
		  emt:eg:all-examples)
	       2)

	    (member
	       (make-emt:example.
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value 12)
	       emt:eg:all-examples)
	    t)))
   
   

   (  "Situation: One example exists, same definer ID
Afterwards: There is only the new example."
      (emt:eg:define:th:with-empty-tagset
	 (:examples
	    (list
	       (make-emt:example.
		  :definer-id 'dummy-id	;;Same ID
		  :tagset '((dummy-tag 1))
		  :value 13)))
	 (emt:eg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (item
	       ;;No further tagset given
	       ()
	       ;;Value
	       12))

	 (and
	    (equal 
	       (length 
		  emt:eg:all-examples)
	       1)

	    (member
	       (make-emt:example.
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value 12)
	       emt:eg:all-examples)
	    ;;Needed for the moment
	    t)))

   (  "Situation: One example exists, same definer ID
Params:  The value of an item is given as a form.
Response:: The new example have the value of the form, not the form itself."
      (emt:eg:define:th:with-empty-tagset ()
	 (emt:eg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (item
	       ()
	       ;;Value
	       (+ 6 6)))

	 (and
	    (equal 
	       (length 
		  emt:eg:all-examples)
	       1)

	    (member
	       (make-emt:example.
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value 12)
	       emt:eg:all-examples)
	    t)))

   ;;Fixing problems
   ("Situation: A group is nested in the definition.
Response: No error.  Behaves normally."
      (emt:eg:define:th:with-empty-tagset ()
	 (not
	    (rtest:gives-error
	       (emt:eg:define dummy-id 
		  ()
		  (group ()))))))

   ("Param: ID is something other than a symbol
Response: Error"
      (emt:eg:define:th:with-empty-tagset ()
	 (rtest:gives-error
	    (emt:eg:define
	       "String instead of symbol"
	       ()))))

   (  "Situation: There are no examples in `emt:eg:all-examples'.
Params: Only a doc is defined.
Behavior: Doesn't create an example.
Afterwards: There is still no example in `emt:eg:all-examples'."
      (emt:eg:define:th:with-empty-tagset ()
	 (emt:eg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (doc
	       ()
	       "A dummy docstring"))
	 (null emt:eg:all-examples)))


   ;;Examples get properties that are defined in their scopes.  
   ;;Treat their respective timing carefully

   (  "Params: A doc and an example are defined.  Doc is defined
first. 
The example's tagset is underneath the example's.
Behavior: Example gets that doc as property `documentation'.
Afterwards: There is one example in `emt:eg:all-examples'.
It has that documentation."
      (emt:eg:define:th:with-empty-tagset ()
	 (let
	    (
	       (docstring emt:eg:define:td:docstring-1))
	    (emt:eg:define
	       dummy-id 
	       ((dummy-tag 1))
	       (doc
		  ()
		  docstring)
	       (item () 12))
	 
	    (equal
	       (emt:example.-property-list
		  (car emt:eg:all-examples))
	       (list
		  (list 'documentation docstring))))))
   

   (  "Params: A doc and an example are defined.  
Doc is defined second. 
The example's tagset is underneath the example's.
Behavior: example gets that doc as property `documentation'.
Afterwards: There is one example in `emt:eg:all-examples'.
It has that documentation."
      (emt:eg:define:th:with-empty-tagset ()
	 (let
	    (
	       (docstring emt:eg:define:td:docstring-1))
	    (emt:eg:define
	       dummy-id 
	       ((dummy-tag 1))
	       (item () 12)
	       (doc
		  ()
		  docstring))
	 
	    (equal
	       (emt:example.-property-list
		  (car emt:eg:all-examples))
	       (list
		  (list 'documentation docstring))))))
   


   (  "Params: A doc and an example are defined.  
Doc is defined first, in a separate `emt:eg:define'. 
The example's tagset is underneath the example's.
Behavior: example gets that doc as property `documentation'.
Afterwards: There is one example in `emt:eg:all-examples'.
It has that documentation."
      (emt:eg:define:th:with-empty-tagset ()
	 (let
	    (
	       (docstring emt:eg:define:td:docstring-1))
	    (emt:eg:define
	       dummy-id 
	       ((dummy-tag 1))
	       (doc
		  ()
		  docstring))

	    (emt:eg:define
	       dummy-id-2
	       ((dummy-tag 1))
	       (item () 12))
	 	 
	    (equal
	       (emt:example.-property-list
		  (car emt:eg:all-examples))
	       (list
		  (list 'documentation docstring))))))
   
   ;;Internally using emt:eg
   (  "Situation: A definition form uses `emt:eg' to get the value of
another example. 
A `transparent-tags' has been defined that relates to this item and
allows it to see the value.
The `emt:eg' doesn't mention the tag that's made transparent
\(otherwise we'd only be proving that emt:eg manages transparency)
The `emt:eg' is still narrow enough to naturally get just one item.
Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'."
      (emt:eg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emt:eg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((dummy-type-tag b)(other-tag b))
	       'dummy-1-type-b)
	    (transparent-tags () (dummy-type-tag)))

	 (emt:eg:define dummy-id ((dummy-tag 1))
	    (item
	       ((dummy-type-tag a))
	       (list 
		  ;;Doesn't mention the tag that's made transparent
		  (emt:eg (other-tag b)) 
		  "Includes another item's value")))
	 (equal
	    (emt:eg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))

   ( "Situation: A definition form uses `emt:eg' to get the value of
another example.
There's no `transparent-tags' property.
And the narrowing tags `emt:eg' uses are distinct from the differentiating tags
between the examples.
Response: Error."
      (emt:eg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emt:eg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b))
	 (rtest:gives-error
	    (emt:eg:define dummy-id ((dummy-tag 1))
	       (item
		  ((another-dummy-type-tag a))
		  (list 
		     (emt:eg (dummy-type-tag b)) 
		     "Includes another item's value"))))))
   ;;This kinda tests emt:eg instead.
   ( "Situation: A definition form uses `emt:eg' to get the value of
another example.
There's no `transparent-tags' property.
But the narrowing tags `emt:eg' uses subsume the differentiating tags
between the examples.
Response: Succeeds."
      (emt:eg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emt:eg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((same-dummy-type-tag b))
	       'dummy-1-type-b))
	 (not
	    (rtest:gives-error
	       (emt:eg:define dummy-id ((dummy-tag 1))
		  (item
		     ((same-dummy-type-tag a))
		     (list 
			(emt:eg (same-dummy-type-tag b)) 
			"Includes another item's value")))))))

   ( "Situation: A definition form uses `emt:eg' to get the value of
another example.

There is a `transparent-tags' property but it doesn't let that
call of `emt:eg' see the example it's trying to use.
Response: Error."
      (emt:eg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emt:eg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b)
	    (transparent-tags () (a-different-tag)))
	 

	 (rtest:gives-error
	    (emt:eg:define dummy-id ((dummy-tag 1))
	       (item
		  ((another-dummy-type-tag a))
		  (list 
		     (emt:eg (dummy-type-tag b)) 
		     "Includes another item's value"))))))

   ( "Situation: A definition form uses `emt:eg' to get the value of
another example.
There is a suitable `transparent-tags' property.
The example it's trying to use doesn't exist.
Response: Error."
      (emt:eg:define:th:with-empty-tagset ()
	 (emt:eg:define dummy-id-different ((dummy-tag 1))
	    (transparent-tags () (a-different-tag)))
	 (rtest:gives-error
	    (emt:eg:define dummy-id ((dummy-tag 1))
	       (item
		  ((dummy-type-tag a))
		  (list 
		     (emt:eg (dummy-type-tag b)) 
		     "Includes another item's value"))))))


   (  "Situation: A definition form uses `emt:eg' to get the value of
another example.
That example existed before but now doesn't.
Response: Error."
      (emt:eg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emt:eg:define dummy-id-different ((dummy-tag 1))
	    (transparent-tags () (dummy-type-tag)))
	 
	 ;;Previous call to same definer.
	 (emt:eg:define dummy-id ((dummy-tag 1))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b))

	 (rtest:gives-error
	    (emt:eg:define dummy-id ((dummy-tag 1))
		  
	       (item
		  ((dummy-type-tag a))
		  (list 
		     (emt:eg (dummy-type-tag b)) 
		     "Includes another item's value"))))))

   ;;Nested transparent-tags 
   ;;Inside groups
   (  "Situation: A definition form uses `emt:eg' to get the value of
another example. 
A `transparent-tags' has been defined inside a group.
It relates to this item and allows it to see the value.
The `emt:eg' doesn't mention the tag that's made transparent
\(otherwise we'd only be proving that emt:eg manages transparency)
The `emt:eg' is still narrow enough to naturally get just one item.
Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'."
      (emt:eg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emt:eg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((all-in-this-tag 1)(dummy-type-tag b)(other-tag b))
	       'dummy-1-type-b)
	    ;;Inside a group.
	    (group ((all-in-this-tag 1))
	       (transparent-tags () (dummy-type-tag))))

	 (emt:eg:define dummy-id ((dummy-tag 1))
	    (item
	       ((all-in-this-tag 1)(dummy-type-tag a))
	       (list 
		  ;;Doesn't mention the tag that's made transparent
		  (emt:eg (other-tag b)) 
		  "Includes another item's value")))
	 (equal
	    (emt:eg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))

   ;;Multiple transparent-tags' that should apply.
   (  "Situation: A definition form uses `emt:eg' to get the value of
another example. 
Two `transparent-tags' have been defined.  Both apply to this item.
Collectively, they remove all the blocking tags to allow it to see the
value.

The `emt:eg' doesn't mention the tag that's made transparent
\(otherwise we'd only be proving that emt:eg manages transparency)
The `emt:eg' is still narrow enough to naturally get just one item.
Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'."
      (emt:eg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emt:eg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((all-in-this-tag 1)(dummy-type-tag b)
		  (dummy-type-tag-2 b)
		  (other-tag b))
	       'dummy-1-type-b)
	    (transparent-tags () (dummy-type-tag))
	    (transparent-tags ((all-in-this-tag 1)) (dummy-type-tag-2)))

	 (emt:eg:define dummy-id ((dummy-tag 1))
	    (item
	       ((all-in-this-tag 1)
		  (dummy-type-tag a)
		  (dummy-type-tag-2 a))
	       (list 
		  ;;Doesn't mention the tag that's made transparent
		  (emt:eg (other-tag b)) 
		  "Includes another item's value")))
	 (equal
	    (emt:eg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))

   
   ;;Dependency handling.  If what's needed is present, no error
   ;;regardless the order they appear in.

   (  "Situation: A definition form uses the value of another example.
The other example and a suitable `transparent-tags' are defined
earlier in the same definition form.
Response: It already sees and uses the `transparent-tags' mark and the
other example."
      (emt:eg:define:th:with-empty-tagset ()
	 (emt:eg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (transparent-tags () (dummy-type-tag))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b)
	    (item
	       ((dummy-type-tag a))
	       (list 
		  (emt:eg (dummy-type-tag b)) 
		  "Includes another item's value")))
	 (equal
	    (emt:eg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))


   (  "Situation: A definition form uses the value of another example.
The other example and a suitable `transparent-tags' are defined later in the
same definition form.
Response: It already sees and uses the `transparent-tags' mark and the
other example."
      (emt:eg:define:th:with-empty-tagset ()
	 (emt:eg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (item
	       ((dummy-type-tag a))
	       (list 
		  (emt:eg (dummy-type-tag b)) 
		  "Includes another item's value"))
	    (transparent-tags () (dummy-type-tag))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b))
	 
	 (equal
	    (emt:eg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))
   

   (  "Situation: Two definitions mutually uses each others values.
Response: Error."
      (emt:eg:define:th:with-empty-tagset ()
	 (rtest:gives-error
	    (emt:eg:define
	       dummy-id 
	       ((dummy-tag 1))
	       (transparent-tags () (dummy-type-tag))
	       (item
		  ((dummy-type-tag b))
		  (list 
		     (emt:eg (dummy-type-tag a)) 
		     "First of two mutually recursive items"))
	       (item
		  ((dummy-type-tag a))
		  (list 
		     (emt:eg (dummy-type-tag b)) 
		     "Second of two mutually recursive items"))))))

   ;;YAGNI yet:

   ;;parallel-items (test in its own thing?)


   ;;Test that tag values are computed just once (use `incf' on some variable).

   ;;doc
   
   )


;;;_ , Use examples
;;;_  . Test helpers
;;;_   , emt:eg:thd:example-examples
(defconst emt:eg:thd:example-examples
   (list
      ;;0 examples in (dummy-tag 0)

      ;;1 example in (dummy-tag 1)
      ;;Also tagged (a t)
      (make-emt:example.
	 :definer-id 'dummy-id
	 :tagset    '((dummy-tag 1) (a t))
	 :value      'tag-1-a)

      ;;2 examples in (dummy-tag 2)
      ;;Also tagged (a t) and (b t) (one each) 
      (make-emt:example.
	 :definer-id 'dummy-id
	 :tagset    '((dummy-tag 2) (a t))
	 :value      'tag-2-a)

      (make-emt:example.
	 :definer-id 'dummy-id
	 :tagset    '((dummy-tag 2) (b t))
	 :value      'tag-2-b))
   
   "Example of a list of examples in their full storage format" )

;;;_   , emt:eg:thd:example-examples-2
(defconst emt:eg:thd:example-examples-2
   (cons
      (make-emt:example.
	 :definer-id 'dummy-id
	 :tagset    '(c)
	 :value      'c)
      emt:eg:thd:example-examples)
   "" )

;;;_   , emt:eg:th:with-example-examples
(defmacro emt:eg:th:with-example-examples (&rest body)
   ""
   ;;Values all name the example they occur in
   `(let
       ;;The larger tagset constrainer is empty.
       (  (emt:eg:tagset ())
	  (emt:eg:all-examples emt:eg:thd:example-examples))
       ,@body))

;;;_   , emt:eg:th:with-example-examples-2
(defmacro emt:eg:th:with-example-examples-2 (&rest body)
   ""
   ;;Values all name the example they occur in
   `(let
       ;;The larger tagset constrainer is empty.
       (  (emt:eg:tagset ())
	  (emt:eg:all-examples emt:eg:thd:example-examples-2))
       ,@body))


;;;_  . emt:eg:kv-matches-p
(defun emt:eg:kv-matches-p (kv filter)
   ""

   (cond
      ((symbolp kv)
	 (if
	    (symbolp filter)
	    (eq kv filter)
	    (error 
	       "Filter %s needs to be a symbol.  There's no value
for it to match" filter)))
		      
      ((consp kv)
	 (cond
	    ((symbolp filter)
	       (eq (car kv)  filter))
	    (t
	       (equal kv filter))))
      (t
	 (error "Key %s is neither symbol nor list" kv))))

;;;_   , Tests
(rtest:deftest emt:eg:kv-matches-p

   (  "Params: Kv is the wrong type (a string)
Response: Non-nil just if kv's key matches filter."
      (rtest:gives-error
	 (emt:eg:kv-matches-p
	    "Wrong type"
	    'dummy-tag)))

   (  "Params: Filter is a bare symbol, kv is a key/value pair
Response: Non-nil just if kv's key matches filter."
      (emt:eg:kv-matches-p
	 '(dummy-tag 2)
	 'dummy-tag))
   
   (  "Params: Filter is a bare symbol, kv is a key/value pair
Response: Non-nil just if kv's key matches filter."
      (not
	 (emt:eg:kv-matches-p
	    '(a t)
	    'dummy-tag)))

   (  "Params: Filter and kv are both bare symbols
Response: Non-nil just if they match."
      (emt:eg:kv-matches-p
	 'a
	 'a))

   (  "Params: Filter and kv are both bare symbols
Response: Non-nil just if they match."
      (not
	 (emt:eg:kv-matches-p
	    'a
	    'b)))

   (  "Params: Filter is a key/value pair, kv is a bare symbol
Response: Error."
      (rtest:gives-error
	 (emt:eg:kv-matches-p
	    'dummy-tag
	    '(dummy-tag 2))))
   
   (  "Params: Filter and kv are both key/value pairs
Response: Non-nil just if they match."
      (emt:eg:kv-matches-p
	 '(dummy-tag 2)
	 '(dummy-tag 2)))
   
   (  "Params: Filter and kv are both key/value pairs
Response: Non-nil just if they match."
      (not 
	 (emt:eg:kv-matches-p
	    '(dummy-tag 2)
	    '(dummy-tag 1))))

   (  "Params: Filter and kv are both key/value pairs
Response: Non-nil just if they match."
      (not 
	 (emt:eg:kv-matches-p
	    '(dummy-tag 2)
	    '(a t))))
   
   )
;;;_  . emt:eg:some-kv-matches

(defun emt:eg:some-kv-matches (one-kv tagset)
   ""
   (some
      #'(lambda (kv)
	   (emt:eg:kv-matches-p kv one-kv))
      tagset))
;;;_   , Tests
(put 'emt:eg:some-kv-matches 'rtest:test-thru
   'emt:eg:filter-one)

;;;_  . emt:eg:filter-one
(defun emt:eg:filter-one (list one-kv)
   ""
   (remove* one-kv list
      :test-not
      #'(lambda (one-kv item)
	   (emt:eg:some-kv-matches 
	      one-kv 
	      (emt:example\.-tagset item)))))

;;;_   , Test helper
(defun emt:eg:filter-one:th (list expected-values)
   ""
   (rtest:sets=
      (mapcar
	 #'emt:example.-value
	 list)
      expected-values))

;;;_   , Tests
(rtest:deftest emt:eg:filter-one
   
   ;;Validate emt:eg:filter-one
   (  "Situation: Passed the entire list and a list of the values on it
Response: Gives non-nil."
      (emt:eg:th:with-example-examples
	 (emt:eg:filter-one:th
	    emt:eg:all-examples
	    '(tag-1-a tag-2-a tag-2-b))))

   (  "Situation: Passed the entire list and a different list of values
Response: Gives nil."
      (emt:eg:th:with-example-examples
	 (not
	    (emt:eg:filter-one:th
	       emt:eg:all-examples
	       '(tag-1-a tag-2-b)))))

   ;;Test emt:eg:filter-one itself
   (  "Param: A tag/value pair
Response: Just examples matching that tag/value pair are kept."
      (emt:eg:th:with-example-examples
	 (emt:eg:filter-one:th
	    (emt:eg:filter-one 
	       emt:eg:all-examples
	       '(dummy-tag 1))
	    '(tag-1-a))))
   

   (  "Param: A tag/value pair
Response: Just examples matching that tag/value pair are kept."
      (emt:eg:th:with-example-examples
	 (emt:eg:filter-one:th
	    (emt:eg:filter-one 
	       emt:eg:all-examples
	       '(dummy-tag 2))
	    '(tag-2-a tag-2-b))))
   

   (  "Param: A bare tag.
Response: Every example matching that tag is kept."
      (emt:eg:th:with-example-examples
	 (emt:eg:filter-one:th
	    (emt:eg:filter-one 
	       emt:eg:all-examples
	       'a)
	    '(tag-1-a tag-2-a))))
   
   (  "Situation: At least one example has a tagset has a bare tag.
Param: A bare tag.
Response: Filters normally, of course matching that tag."
      (emt:eg:th:with-example-examples-2
	 (emt:eg:filter-one:th
	    (emt:eg:filter-one 
	       emt:eg:all-examples
	       'c)
	    '(c))))
   

   (  "Situation: At least one example has a tagset has a bare tag (c).
Param: A tag/value pair matching that tag (c)
Response: Error."
      (emt:eg:th:with-example-examples-2
	 (rtest:gives-error
	    (emt:eg:filter-one:th
	       (emt:eg:filter-one 
		  emt:eg:all-examples
		  '(c t))
	       '(c)))))
   

   )

;;;_  . emt:eg:filter
(defun emt:eg:filter (list kv-list)
   ""

   (reduce
      #'emt:eg:filter-one
      kv-list
      ;;In `reduce', this occurs as arg 1 to FUNC.  List elements are
      ;;arg 2. 
      :initial-value list))


;;;_   , Tests
;;It's just about direct.
(rtest:deftest emt:eg:filter

   ;;Essentially the same as one `emt:eg' test
   ( "Params: A tag/value pair that matches just one example.
Response: Return that example's value."
      (emt:eg:filter-one:th
	 (emt:eg:filter
	    emt:eg:thd:example-examples
	    '((dummy-tag 1)))
	 '(tag-1-a)))
   
   )

;;;_  . emt:eg:get-value
(defun emt:eg:get-value (list kv-list)
   ""
   
   (let
      ((vals (emt:eg:filter list kv-list)))
      
      (case (length vals)
	 (0 
	    (signal 'emt:eg:err:not-found kv-list))
	 (1 
	    (emt:example.-value (car vals)))
	 (t 
	    (signal 'emt:eg:err:too-many kv-list)))))

;;;_   , Tests
(put 'emt:eg:get-value 'rtest:test-thru
   'emt:eg)
;;;_  . emt:eg
;;;###autoload
(defmacro emt:eg (&rest kv-list)
   ""
   ;;Call `emt:eg:value' instead.
   `(emt:eg:get-value emt:eg:all-examples 
       (append 
	  ;;New
	  (emt:eg:tagset-strip emt:eg:tagset 
	     ',(mapcar
		#'(lambda (kv)
		     (if (consp kv) (car kv) kv))
		kv-list))
	  ;;Original
	  ;;emt:eg:tagset 

	  ',kv-list)))
;;;_   , Tests
(rtest:deftest emt:eg

   ;;All these clauses are situated in the example tagset
   ;;`emt:eg:th:with-example-examples'.

   (  "Params: A tag/value pair that matches just one example.
Response: Return that example's value."
      (emt:eg:th:with-example-examples
	 (equal
	    (emt:eg (dummy-tag 1))
	    'tag-1-a)))
   
   ;;Gotta improve `rtest:gives-error' for this.
   ;;Error type is TBD
   (  "Params: A tag/value pair that overconstrains; matches no values.
Response: Throw an error of predetermined type."
      (emt:eg:th:with-example-examples
	 (rtest:gives-error
	    (emt:eg (dummy-tag 2)))))
   
   ;;Error type is TBD.  A resumable error, if that was available.
   (  "Params: A tag/value pair that underconstrains; matches two values.
Response: Throw an error of predetermined type."
      (emt:eg:th:with-example-examples
	 (rtest:gives-error
	    (emt:eg (dummy-tag 0)))))
   
   
   (  "Params: A bare symbol
Behavior: Interpret the symbol as tag whose value we don't care about.
Response: Return the value."
      (emt:eg:th:with-example-examples
	 (equal
	    (emt:eg b)
	    'tag-2-b)))
   

   (  "Params: Several (two) tag/value pairs.
Behavior: Constrain on both of them.
Response: Return the value."
      (emt:eg:th:with-example-examples
	 (equal
	    (emt:eg (dummy-tag 2)(a t))
	    'tag-2-a)))
   
   ;;Dealing with bare tags in the tagset.
   (  "Situation: Some examples' tagsets includes a bare tag.
Params: That tag, bare.
Response: Return the value."
      (emt:eg:th:with-example-examples-2
	 (equal
	    (emt:eg c)
	    'c)))
   

   (  "Situation: Some examples' tagsets includes a bare tag.
Params: That tag, with a value.
Response: Throw an error of predetermined type."
      (emt:eg:th:with-example-examples-2
	 (rtest:gives-error
	    (emt:eg (c 1)))))
   

   ;;Co-operation with emt:eg:narrow.  Specifically, it uses its
   ;;tagset in restricting what is seen.  
   ;;This also tests `emt:eg:tagset-strip'
   (  "Situation: Inside `emt:eg:th:with-example-examples',
`emt:eg:narrow' has narrowed the tagset to only those with tag `a'.
Behavior: See only the examples allowed in that narrowed tagset.
Params: (dummy-tag invalid-2), which would ordinarily fail by being
underconstrained.
Response: Return the value."
      (emt:eg:th:with-example-examples
	 (emt:eg:narrow (a) 
	    (equal
	       (emt:eg (dummy-tag 2))
	       'tag-2-a))))

   )
;;;_  . emt:eg:value
;;;###autoload
(defmacro* emt:eg:value (&key narrow ignore-tags)
   "
Takes keywords
  NARROW - A tagset to narrow by.
  IGNORE-TAGS - tags to be ignored."
   `(emt:eg:get-value emt:eg:all-examples
       (append
	  (emt:eg:tagset-strip emt:eg:tagset ',ignore-tags)
	  ',narrow)))

;;;_   , Tests
(rtest:deftest emt:eg:value

   (  "Params: A tag/value pair that matches just one example.
Response: Return that example's value."
      (emt:eg:th:with-example-examples
	 (equal
	    (emt:eg:value :narrow ((dummy-tag 1)))
	    'tag-1-a)))
   
   ;;This also tests `emt:eg:tagset-strip'
   (  "Same as an emt:eg test, but with `emt:eg:value' in place of
`emt:eg'.

Situation: Inside `emt:eg:th:with-example-examples',
`emt:eg:narrow' has narrowed the tagset to only those with tag `a'.
Behavior: See only the examples allowed in that narrowed tagset.
Params: (dummy-tag invalid-2), which would ordinarily fail by being
underconstrained.
Response: Return the value."

	 (emt:eg:th:with-example-examples
	    (emt:eg:narrow (a) 
	       (equal
		  (emt:eg:value :narrow ((dummy-tag 2)))
		  'tag-2-a))))
   )
;;;_ , Narrowing the tagset
;;;_  . emt:eg:narrow-f
(defun emt:eg:narrow-f (qt-kv-filter body)
   "Execute BODY in a particular examples tagset.
Purpose: For programmability.
Takes it arguments literally."
   
   `(let
       ((emt:eg:tagset 
	   (append ,qt-kv-filter emt:eg:tagset)))
       ,@body))
;;;_   , Tests
(put 'emt:eg:narrow-f 'rtest:test-thru
   'emt:eg)
;;;_  . emt:eg:ignore-tags
(defmacro emt:eg:ignore-tags (transparent-tags &rest body)
   ""
   '  ;;Didn't work right.
   (emt:eg:narrow-f 
      `',(emt:eg:tagset-strip emt:eg:tagset transparent-tags)
      body)

   `(let
      ((emt:eg:tagset 
	  (emt:eg:tagset-strip emt:eg:tagset ',transparent-tags)))
      ,@body))

;;;_   , Tests
(rtest:deftest emt:eg:ignore-tags
   '  ;;Not written.  It was tested thru one use in
   ;;`org-for-code.el'
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   ;;Doesn't handle non-literal transparent-tags 
   )
;;;_  . emt:eg:narrow
;;;###autoload
(defmacro* emt:eg:narrow (kv-filter &rest body)
   "Execute BODY in a particular examples tagset.
Purpose: For consise use inside test code."
   (emt:eg:narrow-f `',kv-filter body))



;;;_   , Test

(rtest:deftest emt:eg:narrow

   ;;Non-looping operation
   ;;Can constrain what emt:eg sees
   '
   (  "Situation: There are two examples, different values for a given
tag.
Params: That tag, with one value
Response: Only the matching example is seen."
      (progn)
      )

   ;;Can co-operate with `emt:eg'
   

   ;;Looping operation, only after emt can accept results:
   ;;Test the looper.
   ;;Normally
   ;;Informs
   ;;With skipping over when some piece can't be found.


   ;;Is informed by emt tester.
   )
;;;_ , Looping
;;;_  . emt:eg:map
;;;###autoload
(defmacro emt:eg:map (tag name &rest body)
   ""
   (let
      ((name (or name (gensym))))
      `(let
	  ((arg-values (emt:eg:all-tag-args ',tag)))
	  (mapcar
	     #'(lambda (,name)
		  ,(emt:eg:narrow-f
		      `(list (list ',tag ,name))
		      body))
	  
	     arg-values))))

;;;_   , Test data
(emt:eg:define xmp:1a424ae8-1c28-4875-bdac-6ba6ad9d1a5e
   ((project emtest)(library emt:eg)(topic map))
   (group
      ((discriminator small)(not-medium))
      (item
	 ((part number)) 2)
      (item
	 ((part string)) "wee"))
   
   (group
      ((discriminator medium))
      (item
	 ((part number)) 14)
      (item
	 ((part string)) "medium string"))

   
   )

;;;_   , Tests
(rtest:deftest emt:eg:map

   (  "Situation: The defined examples
Operation: Iterate over values of `discriminator'.
Response: 
 * Each part resolves without error
 * Each part passes a type test
 * Returning a list of the parts, we get the expected set."

      (emt:eg:narrow
	 ((project emtest)(library emt:eg)(topic map))
	 (assert
	    (rtest:sets= 
	       (emt:eg:map discriminator x x)
	       '(small medium))
	    t)
	 (let
	    ((results
		(emt:eg:map discriminator x
		   (check-type (emt:eg (part number)) integer)
		   (check-type (emt:eg (part string)) string)
		   (list
		      (emt:eg (part number))
		      (emt:eg (part string))))))

	    (assert (rtest:sets= results 
		       '(  (2 "wee")
			   (14 "medium string"))))
	    t)))

   (  "Param: NAME is nil
Response: No errors."

      (emt:eg:narrow
	 ((project emtest)(library emt:eg)(topic map))
	 (emt:eg:map discriminator nil t)
	 t))
   
   ( "Situation: The tag has associated values (`medium') that should
already have been narrowed out of our scope.
Response: Those values are not visited."
      (emt:eg:narrow
	 ((project emtest)(library emt:eg)(topic map)(not-medium))
	 (assert
	    (rtest:sets= 
	       (emt:eg:map discriminator x x)
	       '(small))
	    t)
	 (let
	    ((results
		(emt:eg:map discriminator x
		   (check-type (emt:eg (part number)) integer)
		   (check-type (emt:eg (part string)) string)
		   (list
		      (emt:eg (part number))
		      (emt:eg (part string))))))

	    (assert
	       (rtest:sets= results 
		  '(  (2 "wee"))))
	    t)))
   
   )

;;;_ , Utilities
;;;_  . emt:eg:all-tags
(defun emt:eg:all-tags (&optional example-list)
   "Return a list of all tags used in the existing examples"
   
   (remove-duplicates
      (apply #'append
	 (mapcar
	    #'(lambda (x)
		 (mapcar
		    #'(lambda (kv)
			 (if (consp kv)
			    (car kv)
			    kv))
		    (emt:example.-tagset x)))
	    (or example-list emt:eg:all-examples)))))

;;;_   , Tests
(rtest:deftest emt:eg:all-tags

   (  "Situation: Using the usual examples.
Response: Returns the expected value."
      (emt:eg:th:with-example-examples
	 (rtest:sets=
	    (emt:eg:all-tags)
	    '(dummy-tag a b))))
   
   (  "Situation: Using the second examples.
Response: Returns the expected value."
      (emt:eg:th:with-example-examples-2
	 (rtest:sets=
	    (emt:eg:all-tags)
	    '(dummy-tag a b c))))
      
   )
;;;_  . emt:eg:all-tag-args
(defun emt:eg:all-tag-args (tag)
   "Return a list of all values for a given tag"
   (catch 'emt:eg:tag-no-arg
      (remove-duplicates
	 (apply #'append
	    (mapcar
	       #'(lambda (x)
		    (apply #'append
		       (mapcar
			  #'(lambda (kv)
			       (if (consp kv)
				  (if (eq tag (car kv))
				     (list (second kv))
				     ())
				  (if (eq tag kv)
				     (throw 'emt:eg:tag-no-arg 'no-arg)
				     ())))
			  (emt:example.-tagset x))))
	       (emt:eg:filter 
		  emt:eg:all-examples
		  emt:eg:tagset))))))


;;;_   , Tests
(rtest:deftest emt:eg:all-tag-args

   (  "Situation: Using the usual examples.
Param: dummy-tag.
Response: Returns a list of the values that dummy-tag takes."
      (emt:eg:th:with-example-examples
	 (equal 
	    (emt:eg:all-tag-args 'dummy-tag)
	    '(1 2))))

   (  "Situation: Using the second examples, which have a bare tag.
Response: Returns the symbol `no-arg'."
      (emt:eg:th:with-example-examples-2
	 (equal 
	    (emt:eg:all-tag-args 'c)
	    'no-arg)))
   
   ;;YAGNI: When the tag sometimes has an arg and sometimes not (thus
   ;;it can only be dealt with as no-arg) Signal an error?  Ignore the
   ;;situation.

   ;;Visits just values that are still populated.  Tests use data from
   ;;`emt:eg:map'

   (  "Operation: Visit values of `discriminator'
Response: Finds the expected ones."
      (emt:eg:narrow
	 ((project emtest)(library emt:eg)(topic map))
	 (assert
	    (rtest:sets= 
	       (emt:eg:all-tag-args 'discriminator)
	       '(small medium))
	    t)
	 t))
   

   

   (  "Situation: The tag has associated values (`medium') that should
already have been narrowed out of our scope.
Response: Those values are not found."
      (emt:eg:narrow
	 ((project emtest)(library emt:eg)(topic map)(not-medium))
	 (assert
	    (rtest:sets= 
	       (emt:eg:all-tag-args 'discriminator)
	       '(small))
	    t)
	 t))
   
   )
;;;_  . emt:eg:ambiguous-p
(defun emt:eg:ambiguous-p (tagset)
   "Return whether or not TAGSET is ambiguous on the existing
examples.
TAGSET should not yet refer to an example."

   ;;By filtering its heads combinatorily.  0 is safe forever, >1 is
   ;;safe but explore further, 1 means ambiguous, except at root.
   ;;Some tags imply others, so don't consider them alone. `project'
   ;;and `library' are always implied.
   (let*
      ()
      
      ))
;;;_   , Tests
;;Not yet.
;;;_ , Test support

;;$$This should be shared with previous uses in tests.  But there's a
;;bootstrapping problem.  So define real names for these, and use
;;those objects here by name.
(emt:eg:define xmp:757125c0-8d83-4e75-ae31-78104b90639f
   ((project emtest)(library emt:eg)(section external))
   (transparent-tags () (type))
   (item ((type docstring))
      emt:eg:define:td:docstring-1)
   (item ((type eg-item)(name 0))
      (make-emt:example.
	 :definer-id 'dont-care
	 :value `(doc ,(emt:eg:value 
			  :narrow ((type docstring))
			  :ignore-tags (name)))
	 :tagset '((dummy-tag 1))))
   (item ((type eg-item)(name 1))
      (make-emt:example.
	 :definer-id 'dont-care
	 :value 'dont-care
	 :tagset '((dummy-tag 1))
	 :property-list 
	 (list '(other-prop other-value))))


   )

;;;_  . emt:eg:see-err Include errors in examples
(defun emt:eg:see-err (form)
   "Capture the error raised by FORM.
Use this inside eg, otherwise you risk capturing errors that eg uses
internally.
FORM must be a form that, when evalled, raises an error"
   
   (condition-case err
      (progn
	 (eval form)
	 ;;If it failed to raise an error, raise a (meta-)error.
	 (error "Form failed to raise error: %s" form))

      ;;If it's an eg error, re-raise it (This is important to do)
      (emt:eg:err (signal (car err)(cdr err)))
      ;;Otherwise trap it and pass on its value
      (error err)))



;;;_ , Editing help
;;;_  . Fill in an example-spec from tags
;;That's being done in pcmpl-emtest

;;;_  . From `emt:eg', go to its definition
;;At least, if it's in the same file.
;;Can easily find definer-id

;;;_  . From example, go to file it refers to
;;Like the above, just one more step.
;;Would help to look at a "type" property of items.
;;;_ , Browse  (Never worked)

;;;_  . Structures
(defstruct (emt:eg:browse:relative-distinction
	      (:constructor emt:eg:browse:make-relative-distinction)
	      (:conc-name emt:eg:browse:relative-distinction->)
	      (:copier nil))
   ""
   item
   extra-kv-list
   missing-kv-list)

;;;_  . emt:eg:browse:make-relative-distinction
(defun emt:eg:browse:item->relative-distinction (item reference-tagset)
   ""
   (emt:eg:browse:make-relative-distinction
      :item item
      ;;$$Not at all sure these are in right respective positions.
      ;;Feel free to swap them.
      :extra-kv-list 
      (remove
	 nil
	 (mapcar
	    #'(lambda (kv)
		 (unless
		    (emt:eg:some-kv-matches kv reference-tagset)
		    kv))
	    (emt:example.-tagset item)))

      :missing-kv-list
      (remove
	 nil
	 (mapcar
	    #'(lambda (kv)
		 (unless
		    (emt:eg:some-kv-matches kv (emt:example.-tagset item))
		    kv))
	    reference-tagset))))

;;;_   , Tests
(rtest:deftest emt:eg:browse:item->relative-distinction
   '  ;;An inspection test
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn
	 (assert
	    (equal
	       (emt:eg:browse:item->relative-distinction
		  (emt:eg (type eg-item)(name 0))
		  '((project emtest)(library emt:eg)(topic map)))
	       ;;Persister.  But we don't care about the item
	       ;;field, which merely must match the input.  Only the
	       ;;other fields should persist.  See
	       ;;file:/development.org "Masked comparison"
	       )
	    t)
	 t)

      )
   
   )

;;;_  . emt:eg:browse:format-relative-distinction
;;$$Rename emt:eg:browse:->stage-1
;;formatter could help abbreviate this.  Supply the control stuff and
;;client supplies the clauses.
(defun emt:eg:browse:format-relative-distinction (obj)
   ""
   
   (typecase obj
      (emt:eg:browse:relative-distinction
	 `(w/headline
	     (:weight 2)
	     "An item"
	     (sep 3)
	     (data-persist-used
		,(emt:eg:browse:relative-distinction->extra-kv-list obj)
		())
	     (sep 3)
	     (data-persist-used
		,(emt:eg:browse:relative-distinction->missing-kv-list obj)
		())))
      
      ;;Here's a place to handle groupings, if ever we have them.

      ;;Lists are iterated over.  This is slightly messed up with
      ;;`list' vs type vs deep-type.  It's also a PITA wrt setting up
      ;;a list vs setting up a sequence.
      (cons
	 `(sequence
	     ,@(mapcar #'emt:eg:browse:format-relative-distinction obj)))))

;;;_   , Tests
(rtest:deftest emt:eg:browse:format-relative-distinction
   ;;Should type-check the output, if that were possible.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn
	 (assert
	    (equal
	       (emt:eg:browse:format-relative-distinction
		  (emt:eg:browse:make-relative-distinction
		     :item (emt:eg (type eg-item)(name 0))))
	    
	       ;;Here put a persisting version
	       )
	    t)
	 t))
   
   
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn
	 (assert
	    (equal
	       (emt:eg:browse:format-relative-distinction
		  (list
		     (emt:eg:browse:make-relative-distinction
			:item
			(emt:eg (type eg-item)(name 0)))
		     (emt:eg:browse:make-relative-distinction
			:item
			(emt:eg (type eg-item)(name 1)))))
	       
	       ;;Here put a persisting version
	       )
	    t)
	 t))
   

   )

;;;_  . emt:eg:browse:top

(defun emt:eg:browse:top (reference-tagset narrowing-tagset)
   ""
   
   (interactive)
   (let*
      (
	 ;;This will restrict them if called within an EG narrowing.
	 ;;Surprising but can't be helped.
	 (all-items
	    (emt:eg:filter emt:eg:all-examples narrowing-tagset))
	 ;;This may be used later, in sorting remaining tags
	 (all-tags
	    (emt:eg:all-tags all-items))
	 (all-distinctions
	    (mapcar
	       #'(lambda (item)
		    (emt:eg:browse:item->relative-distinction
		       item
		       reference-tagset))
	       all-items))
	 (sorted
	    ;;Punt for now
	    all-distinctions)
	 (grouped
	    ;;Punt for now
	    sorted)
	 (summarized
	    ;;Punt for now
	    grouped)
	 (stage-1-formatted
	    (emt:eg:browse:format-relative-distinction summarized)))

      (formatter:display-from-stage1
	 stage-1-formatted
	 "*EG browse*")))


;;;_   , Tests

;;These are inspection tests.
(rtest:deftest emt:eg:browse:top
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      ;;Inspect this
      (emt:eg:browse:top
	 '((project emtest)(library emt:eg)(topic map))
	 '((project emtest)(library emt:eg)(topic map))))
   
   )


;;;_ , Browse near-miss

;;;_  . emt:eg:browse

(defun emt:eg:browse (tagset &optional tags-too-wide) 
   "Pop up a buffer browsing the existing definitions.

TAGSET must be a tagset"
   ;;If TAGS-TOO-WIDE is given, ignore those tags for purposes of
   ;;generating near-matches.
   ;;Could be made interactive by picking from tags.
   (let*
      ((buf (generate-new-buffer "*EG browse*"))
	 ;;Generate it raw.  Sort it by felicity of fit (0 sorts last)
	 (near-matches-raw
	    (mapcar
	       #'(lambda (kv)
		    (list 
		       kv
		       (emt:eg:filter 
			  emt:eg:all-examples 
			  (remove kv tagset))))
	       tagset))
	 (near-matches-lol near-matches-raw)
	 )
      
      ;;Use formatter if this formatting becomes difficult
      (with-current-buffer buf
	 (insert "Near matches for tagset "
	    ;;Want a nicer way of printing tagset.
	    (prin1-to-string tagset)
	    "\n\n")
	 
	 (dolist (nm near-matches-lol)
	    (destructuring-bind
	       (kv-off near-matches) nm
	       (insert 
		  "** "
		  "Without restriction `" (symbol-name (car kv-off))
		  "' there are "
		  (format "%d" (length near-matches))
		  " matches")
	       (insert "\n")

	       (dolist (i near-matches)
		  (let*
		     ((i-tagset (emt:example.-tagset i))
			(diff-tagset 
			   (set-difference 
			      i-tagset tagset
			      :test #'equal)))
		     (insert
			"*** "
			"Adding " (format "%d" (length diff-tagset))
			" tags"
			"\n"
			;;First print what it adds
			"New tags "
			(format "%s"
			   diff-tagset)
			"\n"
			;;Then print its whole tagset
			"Tagset: "
			(format "%s" i-tagset)
			"\n\n")))
	       
	       (insert "\n")))
	 ;;To user can fold it, just put the buffer into outline mode.
	 (outline-mode))
      (pop-to-buffer buf)))
;;;_   , Tests

;;;_. Footers
;;;_ , Provides

(provide 'eg)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; eg.el ends here
