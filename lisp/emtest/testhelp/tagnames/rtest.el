;;;_ emtest/testhelp/tagnames/rtest.el --- Tests of eg

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

(require 'emtest/testhelp/tagnames/testhelp)
;;;_. Body

;;;_ , Tests of emtg:th:validate-helper-retval

;;;_  . Test data

;;For bootstrap reasons, these examples can't be defined with
;;`emtg:define'

(defconst emtg:define:td:typical-helper-retval
   (emtg:make-helper-rettype
      :value-info
      (list
	 (emtg:make-valuedef-type
	    :tagset '((dummy-tag 1))
	    :value-form 12)))
   
   "" )

(defconst emtg:define:td:docstring-1
   "A dummy docstring"
   "" )

;;;_ , Test support

;;$$This should be shared with previous uses in tests.  But there's a
;;bootstrapping problem.  So define real names for these, and use
;;those objects here by name.
(defconst emtg:thd:examples
   (emtg:define+ ;;xmp:757125c0-8d83-4e75-ae31-78104b90639f
      ((project emtest)(library emtg)(section external))
      (transparent-tags () (type))
      (item ((type docstring))
	 emtg:define:td:docstring-1)
      (item ((type eg-item)(name 0))
	 (emtg:make-example
	    :definer-id 'dont-care
	    :value `(doc ,(emtg:value 
			     :narrow ((type docstring))
			     :ignore-tags (name)))
	    :tagset '((dummy-tag 1))))
      (item ((type eg-item)(name 1))
	 (emtg:make-example
	    :definer-id 'dont-care
	    :value 'dont-care
	    :tagset '((dummy-tag 1))
	    :property-list 
	    (list '(other-prop other-value))))


      ))

;;;_  . emtg:see-err Include errors in examples
(defun emtg:see-err (form)
   "Capture the error raised by FORM.
Use this inside tagnames, otherwise you risk capturing errors that tagnames uses
internally.
FORM must be a form that, when evalled, raises an error"
   
   (condition-case err
      (progn
	 (eval form)
	 ;;If it failed to raise an error, raise a (meta-)error.
	 (error "Form failed to raise error: %s" form))

      ;;If it's an tagname error, re-raise it (This is important to do)
      (emtg:err (signal (car err)(cdr err)))
      ;;Otherwise trap it and pass on its value
      (error err)))




;;;_  . Tests
(rtest:deftest emtg:th:validate-helper-retval

   (  "Validate this against `emtg:define:td:typical-helper-retval'."
      (emtg:th:validate-helper-retval
	 emtg:define:td:typical-helper-retval))

   (  "Situation: We get a retval that's correct
Response: Return non-nil."
      (emtg:th:validate-helper-retval
	 (emtg:make-helper-rettype
	    :value-info
	    (list
	       (emtg:make-valuedef-type
		  :tagset
		  ()
		  :value-form
		  'the-value-form)))))
   

   (  "Situation: We get a retval that's displaced one place too high
Response: Return nil."
      (not
	 (emtg:th:validate-helper-retval
	    (emtg:make-helper-rettype
	       :value-info
	       (emtg:make-valuedef-type
		  :tagset
		  ()
		  :value-form
		  'the-value-form)))))

   (  "Situation: We get a retval whose contents are messed up
Response: Return nil."
      (not
	 (emtg:th:validate-helper-retval
	    (emtg:make-helper-rettype
	       :value-info
	       (list
		  (list
		     'value 
		     ()
		     13))))))

   

   ;;YAGNI: Could also validate that an expected tagset appears
   ;;everywhere.

   )

;;;_   , Tests
(rtest:deftest emt:see-item

   (  "Behavior: The return value validates OK."
      (emtg:th:validate-helper-retval
	 (emt:see-item () () 'the-value-form)))
   
   ;;Punt `others' for now.  Later, test that it can return also a doc
   ;;with the same tagset.

   )
;;;_   , Tests
(rtest:deftest emt:see-doc

   (  "Behavior: The return value validates OK."
      (emtg:th:validate-helper-retval
	 (emt:see-doc () () "A doc string")))

   ;;For now, punt but be structurally correct
   
   )

;;;_   , Tests
(rtest:deftest emt:see-type-must-be

   (  "Situation: In definer, item mismatches type.
Type spec is before item spec.
Response: error."
      (emtg:define:th:with-empty-tagset ()
	 (assert
	    (emth:gives-error
	       (emtg:define
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
      (emtg:define:th:with-empty-tagset ()
	 (assert
	    (emth:gives-error
	       (emtg:define
		  dummy-id ()
		  (item
		     ((dummy-type-tag a))
		     'dummy-1-type-a)
		  (type-must-be ((dummy-type-tag a)) integer))
	       wrong-type-argument))
	 t))
   
   (  "Situation: In definer, item matches type
Response: item is usable as normal."
      (emtg:define:th:with-empty-tagset ()

	 (emtg:define
	    dummy-id ()
	    (type-must-be ((dummy-type-tag a)) symbol)
	    (item
	       ((dummy-type-tag a))
	       'dummy-1-type-a))
	 (assert
	    (not
	       (emth:gives-error
		  (emtg (dummy-type-tag a)))))
	 t)))


;;;_   , Tests
(rtest:deftest emt:see-group
   
   (  "Behavior: The return value validates OK."
      (emtg:th:validate-helper-retval
	 (emt:see-group () () '(item () the-value-form)))))


;;;_   , Tests
(put 'emtg:remove-earlier-defs 'rtest:test-thru
   'emtg:define)

;;;_   , Tests
(put 'emtg:propty-match-ctxt-p 'rtest:test-thru
   'emtg:find-properties)

;;;_   , Tests
(rtest:deftest emtg:find-properties

   (  "Situation: target-tagset exactly matches the only property.
Response: Return list has just the value of that property."
      (equal
	 (emtg:find-properties
	    '((dummy-tag 1))
	    (list
	       (emtg:make-example
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value '(prop-sym1 val1))))
	 
	 '((prop-sym1 val1)))))


;;;_   , Tests
(put 'emtg:apply-proplist-specials-to-example 'rtest:test-thru
   'emt:see-type-must-be)

;;;_   , emtg:apply-prpty-makers-to-examples

;;;_    . Test helper emtg:apply-prpty-makers-to-examples:th
(defun emtg:apply-prpty-makers-to-examples:th (prpty-maker example)
   "Like `emtg:apply-prpty-makers-to-examples' but deals only with
singletons.  Returns a singleton"
   (car
      (emtg:apply-prpty-makers-to-examples
	 (list prpty-maker)
	 (list example))))

;;;_   , Tests
(rtest:deftest emtg:apply-prpty-makers-to-examples

   (  "Situation: X's tagset matches PROP's.
Response: That property is added to X's property-list."
      (emtg:define:th:with-empty-tagset ()
	 (let*
	    (
	       (docstring emtg:define:td:docstring-1)
	       (new-x
		  (emtg:apply-prpty-makers-to-examples:th
		     (emtg:make-example
			:definer-id 'dont-care
			:value `(doc ,docstring)
			:tagset '((dummy-tag 1)))
		     (emtg:make-example
			:definer-id 'dont-care
			:value 'dont-care
			:tagset '((dummy-tag 1))
			:property-list ()))))
	    
	    (equal
	       (emtg:example->property-list new-x)
	       (list
		  `(doc ,docstring))))))

   (  "Situation: X's tagset matches PROP's.
X already has a different property.
Response: The new property is added to X's property-list."
      (emtg:define:th:with-empty-tagset ()
	 (let*
	    (
	       (docstring emtg:define:td:docstring-1)
	       (new-x

		  (emtg:apply-prpty-makers-to-examples:th

		     (emtg:make-example
			:definer-id 'dont-care
			:value `(doc ,docstring)
			:tagset '((dummy-tag 1)))

		     (emtg:make-example
			:definer-id 'dont-care
			:value 'dont-care
			:tagset '((dummy-tag 1))
			:property-list (list '(other-prop other-value))))))
	    
	    (rtest:sets=
	       (emtg:example->property-list new-x)
	       (list
		  `(doc ,docstring)
		  '(other-prop other-value))))))
   
   (  "Situation: X's tagset does not match PROP's.
Response: X is unchanged."
      (emtg:define:th:with-empty-tagset ()
	 (let*
	    (
	       (docstring emtg:define:td:docstring-1)
	       (new-x

		  (emtg:apply-prpty-makers-to-examples:th

		     (emtg:make-example
			:definer-id 'dont-care
			:value `(doc ,docstring)
			:tagset '((dummy-tag 1)))

		     (emtg:make-example
			:definer-id 'dont-care
			:value 'dont-care
			:tagset '((dummy-tag 2))
			:property-list ()))))
	    
	    (equal
	       (emtg:example->property-list new-x)
	       ())))))

;;;_   , Tests
'
(put 'emtg:see-new-examples 'rtest:test-thru
   'emtg:define)

;;;_   , Tests

(put 'emtg:valuedef->example 'rtest:test-thru
   'emtg:define)

;;;_  . Tests
;;;_   , Test helper (Maybe obsolete)
(defmacro emtg:try-valuedef->example:th (&rest body)
   "
Can't use `emtg' in body."
   
   `(let
       (  (emtg:all-examples ())
	  (emtg:delayed-examples ()))
       ,@body))

;;;_   , Tests
'  ;;OBSOLETE
(rtest:deftest emtg:try-valuedef->example

   (  "Situation: Form has no error.
Response: Put it on the examples list `emtg:all-examples'."
      (emtg:try-valuedef->example:th
	 (emtg:try-valuedef->example
	    (emtg:make-valuedef-type
	       :value-form
	       12)
	    'dummy-id
	    ())
	 
	 (equal (length emtg:all-examples) 1)))
   

   (  "Situation: Form throws an error of type `emtg:err-not-available'
Response: Put it on the delayed list `emtg:delayed-examples'.
The error doesn't escape."
      (emtg:try-valuedef->example:th
	 (emtg:try-valuedef->example
	    (emtg:make-valuedef-type
	       :value-form
	       '(signal 'emtg:err:not-found nil))
	    'dummy-id
	    ())
	 (equal (length emtg:delayed-examples) 1))))
;;;_  . 


;;;_   , Test helper
;;Maybe just call `emtg:define:th:with-empty-tagset'
(defmacro emtg:try-valuedef->example-2:th (&rest body)
   "
Can't use `emtg' in body."
   
   `(let
       (  (emtg:all-examples ())
	  (emtg:*all-prpty-makers* ()))
       ,@body))


;;;_  . Tests
(rtest:deftest emtg:try-valuedef->example-2

   (  "Situation: Form has no error.
Response: Put it on the examples list `emtg:all-examples'.
Return the empty list."
      (emtg:try-valuedef->example-2:th
	 (assert
	    (null
	       (emtg:try-valuedef->example-2
		  (emtg:make-valuedef-type
		     :value-form
		     12)
		  'dummy-id)))
	 
	 (equal (length emtg:all-examples) 1)))
   

   (  "Situation: Form throws an error of type `emtg:err-not-available'
Response: The error doesn't escape.
Return a list consisting of that example."
      (emtg:try-valuedef->example-2:th
	 (let
	    ((valuedef
		(emtg:make-valuedef-type
		  :value-form
		  '(signal 'emtg:err:not-found nil))))
	 (assert
	    (equal
	       (emtg:try-valuedef->example-2
		  valuedef
		  'dummy-id)
	       (list valuedef))
	    t)
	 (assert
	    (equal (length emtg:all-examples) 0))
	 t))))

;;;_  . Tests
(put 'emtg:valuedef->property 'rtest:test-thru
   'emtg:define)

;;;_  . Tests
(put 'emtg:tagset-strip 'rtest:test-thru
   'emtg:tagset-strip-transparents)

;;;_  . Tests
(rtest:deftest emtg:tagset-strip-transparents
   ;;
   (  "Situation: No transparent-tags among props
Response: Returns exactly the tagset."
      (let
	 ((tagset '((dummy-tag 1))))
	 (equal
	    (emtg:tagset-strip-transparents tagset ())
	    tagset)))


   (  "Situation: No transparent-tags among props
Response: Returns tagset with matching tags part removed."
      (let
	 ((tagset '((dummy-tag 1)(dummy-tag-2 2))))
	 (equal
	    (emtg:tagset-strip-transparents 
	       tagset
	       '((transparent-tags (dummy-tag))))
	    
	    '((dummy-tag-2 2)))))

   (  "Situation: tagset contains a singleton item,
Behavior: Still works."
      (let
	 ((tagset '(dummy-tag)))
	 (equal
	    (emtg:tagset-strip-transparents tagset ())
	    tagset))))

;;;_  . Tests
(put 'emtg:define-f 'rtest:test-thru
   'emtg:define)
;;;_  . Tests

(rtest:deftest emtg:define

   ;;Validation:
   (  "Validation: `emtg:all-examples' does not have anything defined
by the dummy definers (which deliberately re-use the same ID)."
      (not
	 (member* 'dummy-id emtg:all-examples
	    :key #'emtg:example->definer-id)))
   
   (  "Validation: `emtg:*all-prpty-makers*' does not have anything
defined by the dummy definers (which deliberately re-use the same ID)."
      (not
	 (member* 'dummy-id emtg:*all-prpty-makers*
	    :key #'emtg:example->definer-id)))
   
   ;;These tests let `emtg:all-examples' to known values so that
   ;;tagset is known.  We can't use emtg-style examples inside them.

   ;;Mock `emt:see-group', so we can just test the inserting
   ;;functionality.  
   (  "Situation: There are no examples in `emtg:all-examples'.
Afterwards: There is an example in `emtg:all-examples'."
      (with-mock
	 (stub emt:see-group =>
	    emtg:define:td:typical-helper-retval)
	 (emtg:define:th:with-empty-tagset ()
	    (emtg:define
	       dummy-id 
	       ;;Punt the other args
	       ()
	       ())

	    (and
	       (equal 
		  (length 
		     emtg:all-examples)
		  1)

	       (member
		  (emtg:make-example
		     :definer-id 'dummy-id
		     :tagset '((dummy-tag 1))
		     :value 12)
		  emtg:all-examples)
	       t))))
   
   
   (  "Situation: One example, different definer ID.
Afterwards: There are two examples in `emtg:all-examples'.  One is
the new one"
      (with-mock
	 (stub emt:see-group => emtg:define:td:typical-helper-retval)
	 (emtg:define:th:with-empty-tagset
	    (:examples
	       (list
		  (emtg:make-example
		     :definer-id 'dummy-id-2
		     :tagset '((dummy-tag 1))
		     :value 13)))
	    (emtg:define
	       dummy-id 
	       ;;Punt the other args
	       ()
	       ())

	    (and
	       (equal 
		  (length 
		     emtg:all-examples)
		  2)

	       (member
		  (emtg:make-example
		     :definer-id 'dummy-id
		     :tagset '((dummy-tag 1))
		     :value 12)
		  emtg:all-examples)
	       t))))
   
   

   (  "Situation: One example exists, same definer ID
Afterwards: There is only the new example."
      (with-mock
	 (stub emt:see-group => emtg:define:td:typical-helper-retval)
	 (emtg:define:th:with-empty-tagset
	    (:examples
	       (list
		  (emtg:make-example
		     :definer-id 'dummy-id ;;Same ID
		     :tagset '((dummy-tag 1))
		     :value 13)))
	    (emtg:define
	       dummy-id 
	       ;;Punt the other args
	       ()
	       ())

	    (and
	       (equal 
		  (length 
		     emtg:all-examples)
		  1)

	       (member
		  (emtg:make-example
		     :definer-id 'dummy-id
		     :tagset '((dummy-tag 1))
		     :value 12)
		  emtg:all-examples)
	       t))))



   ;;These tests test the same without mocks.  They rely on the
   ;;helpers working.

   (  "Situation: There are no examples in `emtg:all-examples'.
Afterwards: There is an example in `emtg:all-examples'."
      (emtg:define:th:with-empty-tagset ()
	 (emtg:define
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
		  emtg:all-examples)
	       1)

	    (member
	       (emtg:make-example
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value 12)
	       emtg:all-examples)
	    t)))

   
   (  "Situation: One example, different definer ID.
Afterwards: There are two examples in `emtg:all-examples'.  One is
the new one"
      (emtg:define:th:with-empty-tagset
	 (:examples
	    (list
	       (emtg:make-example
		  :definer-id 'dummy-id-2
		  :tagset '((dummy-tag 1))
		  :value 13)))
	 (emtg:define
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
		  emtg:all-examples)
	       2)

	    (member
	       (emtg:make-example
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value 12)
	       emtg:all-examples)
	    t)))
   
   

   (  "Situation: One example exists, same definer ID
Afterwards: There is only the new example."
      (emtg:define:th:with-empty-tagset
	 (:examples
	    (list
	       (emtg:make-example
		  :definer-id 'dummy-id	;;Same ID
		  :tagset '((dummy-tag 1))
		  :value 13)))
	 (emtg:define
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
		  emtg:all-examples)
	       1)

	    (member
	       (emtg:make-example
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value 12)
	       emtg:all-examples)
	    ;;Needed for the moment
	    t)))

   (  "Situation: One example exists, same definer ID
Params:  The value of an item is given as a form.
Response:: The new example have the value of the form, not the form itself."
      (emtg:define:th:with-empty-tagset ()
	 (emtg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (item
	       ()
	       ;;Value
	       (+ 6 6)))

	 (and
	    (equal 
	       (length 
		  emtg:all-examples)
	       1)

	    (member
	       (emtg:make-example
		  :definer-id 'dummy-id
		  :tagset '((dummy-tag 1))
		  :value 12)
	       emtg:all-examples)
	    t)))

   ;;Fixing problems
   ("Situation: A group is nested in the definition.
Response: No error.  Behaves normally."
      (emtg:define:th:with-empty-tagset ()
	 (not
	    (rtest:gives-error
	       (emtg:define dummy-id 
		  ()
		  (group ()))))))

   ("Param: ID is something other than a symbol
Response: Error"
      (emtg:define:th:with-empty-tagset ()
	 (rtest:gives-error
	    (emtg:define
	       "String instead of symbol"
	       ()))))

   (  "Situation: There are no examples in `emtg:all-examples'.
Params: Only a doc is defined.
Behavior: Doesn't create an example.
Afterwards: There is still no example in `emtg:all-examples'."
      (emtg:define:th:with-empty-tagset ()
	 (emtg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (doc
	       ()
	       "A dummy docstring"))
	 (null emtg:all-examples)))


   ;;Examples get properties that are defined in their scopes.  
   ;;Treat their respective timing carefully

   (  "Params: A doc and an example are defined.  Doc is defined
first. 
The example's tagset is underneath the example's.
Behavior: Example gets that doc as property `documentation'.
Afterwards: There is one example in `emtg:all-examples'.
It has that documentation."
      (emtg:define:th:with-empty-tagset ()
	 (let
	    (
	       (docstring emtg:define:td:docstring-1))
	    (emtg:define
	       dummy-id 
	       ((dummy-tag 1))
	       (doc
		  ()
		  docstring)
	       (item () 12))
	 
	    (equal
	       (emtg:example->property-list
		  (car emtg:all-examples))
	       (list
		  (list 'documentation docstring))))))
   

   (  "Params: A doc and an example are defined.  
Doc is defined second. 
The example's tagset is underneath the example's.
Behavior: example gets that doc as property `documentation'.
Afterwards: There is one example in `emtg:all-examples'.
It has that documentation."
      (emtg:define:th:with-empty-tagset ()
	 (let
	    (
	       (docstring emtg:define:td:docstring-1))
	    (emtg:define
	       dummy-id 
	       ((dummy-tag 1))
	       (item () 12)
	       (doc
		  ()
		  docstring))
	 
	    (equal
	       (emtg:example->property-list
		  (car emtg:all-examples))
	       (list
		  (list 'documentation docstring))))))
   


   (  "Params: A doc and an example are defined.  
Doc is defined first, in a separate `emtg:define'. 
The example's tagset is underneath the example's.
Behavior: example gets that doc as property `documentation'.
Afterwards: There is one example in `emtg:all-examples'.
It has that documentation."
      (emtg:define:th:with-empty-tagset ()
	 (let
	    (
	       (docstring emtg:define:td:docstring-1))
	    (emtg:define
	       dummy-id 
	       ((dummy-tag 1))
	       (doc
		  ()
		  docstring))

	    (emtg:define
	       dummy-id-2
	       ((dummy-tag 1))
	       (item () 12))
	 	 
	    (equal
	       (emtg:example->property-list
		  (car emtg:all-examples))
	       (list
		  (list 'documentation docstring))))))
   
   ;;Internally using emtg
   (  "Situation: A definition form uses `emtg' to get the value of
another example. 
A `transparent-tags' has been defined that relates to this item and
allows it to see the value.
The `emtg' doesn't mention the tag that's made transparent
\(otherwise we'd only be proving that emtg manages transparency)
The `emtg' is still narrow enough to naturally get just one item.
Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'."
      (emtg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emtg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((dummy-type-tag b)(other-tag b))
	       'dummy-1-type-b)
	    (transparent-tags () (dummy-type-tag)))

	 (emtg:define dummy-id ((dummy-tag 1))
	    (item
	       ((dummy-type-tag a))
	       (list 
		  ;;Doesn't mention the tag that's made transparent
		  (emtg (other-tag b)) 
		  "Includes another item's value")))
	 (equal
	    (emtg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))

   ( "Situation: A definition form uses `emtg' to get the value of
another example.
There's no `transparent-tags' property.
And the narrowing tags `emtg' uses are distinct from the differentiating tags
between the examples.
Response: Error."
      (emtg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emtg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b))
	 (rtest:gives-error
	    (emtg:define dummy-id ((dummy-tag 1))
	       (item
		  ((another-dummy-type-tag a))
		  (list 
		     (emtg (dummy-type-tag b)) 
		     "Includes another item's value"))))))
   ;;This kinda tests emtg instead.
   ( "Situation: A definition form uses `emtg' to get the value of
another example.
There's no `transparent-tags' property.
But the narrowing tags `emtg' uses subsume the differentiating tags
between the examples.
Response: Succeeds."
      (emtg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emtg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((same-dummy-type-tag b))
	       'dummy-1-type-b))
	 (not
	    (rtest:gives-error
	       (emtg:define dummy-id ((dummy-tag 1))
		  (item
		     ((same-dummy-type-tag a))
		     (list 
			(emtg (same-dummy-type-tag b)) 
			"Includes another item's value")))))))

   ( "Situation: A definition form uses `emtg' to get the value of
another example.

There is a `transparent-tags' property but it doesn't let that
call of `emtg' see the example it's trying to use.
Response: Error."
      (emtg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emtg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b)
	    (transparent-tags () (a-different-tag)))
	 

	 (rtest:gives-error
	    (emtg:define dummy-id ((dummy-tag 1))
	       (item
		  ((another-dummy-type-tag a))
		  (list 
		     (emtg (dummy-type-tag b)) 
		     "Includes another item's value"))))))

   ( "Situation: A definition form uses `emtg' to get the value of
another example.
There is a suitable `transparent-tags' property.
The example it's trying to use doesn't exist.
Response: Error."
      (emtg:define:th:with-empty-tagset ()
	 (emtg:define dummy-id-different ((dummy-tag 1))
	    (transparent-tags () (a-different-tag)))
	 (rtest:gives-error
	    (emtg:define dummy-id ((dummy-tag 1))
	       (item
		  ((dummy-type-tag a))
		  (list 
		     (emtg (dummy-type-tag b)) 
		     "Includes another item's value"))))))


   (  "Situation: A definition form uses `emtg' to get the value of
another example.
That example existed before but now doesn't.
Response: Error."
      (emtg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emtg:define dummy-id-different ((dummy-tag 1))
	    (transparent-tags () (dummy-type-tag)))
	 
	 ;;Previous call to same definer.
	 (emtg:define dummy-id ((dummy-tag 1))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b))

	 (rtest:gives-error
	    (emtg:define dummy-id ((dummy-tag 1))
		  
	       (item
		  ((dummy-type-tag a))
		  (list 
		     (emtg (dummy-type-tag b)) 
		     "Includes another item's value"))))))

   ;;Nested transparent-tags 
   ;;Inside groups
   (  "Situation: A definition form uses `emtg' to get the value of
another example. 
A `transparent-tags' has been defined inside a group.
It relates to this item and allows it to see the value.
The `emtg' doesn't mention the tag that's made transparent
\(otherwise we'd only be proving that emtg manages transparency)
The `emtg' is still narrow enough to naturally get just one item.
Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'."
      (emtg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emtg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((all-in-this-tag 1)(dummy-type-tag b)(other-tag b))
	       'dummy-1-type-b)
	    ;;Inside a group.
	    (group ((all-in-this-tag 1))
	       (transparent-tags () (dummy-type-tag))))

	 (emtg:define dummy-id ((dummy-tag 1))
	    (item
	       ((all-in-this-tag 1)(dummy-type-tag a))
	       (list 
		  ;;Doesn't mention the tag that's made transparent
		  (emtg (other-tag b)) 
		  "Includes another item's value")))
	 (equal
	    (emtg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))

   ;;Multiple transparent-tags' that should apply.
   (  "Situation: A definition form uses `emtg' to get the value of
another example. 
Two `transparent-tags' have been defined.  Both apply to this item.
Collectively, they remove all the blocking tags to allow it to see the
value.

The `emtg' doesn't mention the tag that's made transparent
\(otherwise we'd only be proving that emtg manages transparency)
The `emtg' is still narrow enough to naturally get just one item.
Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'."
      (emtg:define:th:with-empty-tagset ()
	 ;;Create pre-existing property and example.
	 (emtg:define dummy-id-different ((dummy-tag 1))
	    (item
	       ((all-in-this-tag 1)(dummy-type-tag b)
		  (dummy-type-tag-2 b)
		  (other-tag b))
	       'dummy-1-type-b)
	    (transparent-tags () (dummy-type-tag))
	    (transparent-tags ((all-in-this-tag 1)) (dummy-type-tag-2)))

	 (emtg:define dummy-id ((dummy-tag 1))
	    (item
	       ((all-in-this-tag 1)
		  (dummy-type-tag a)
		  (dummy-type-tag-2 a))
	       (list 
		  ;;Doesn't mention the tag that's made transparent
		  (emtg (other-tag b)) 
		  "Includes another item's value")))
	 (equal
	    (emtg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))

   
   ;;Dependency handling.  If what's needed is present, no error
   ;;regardless the order they appear in.

   (  "Situation: A definition form uses the value of another example.
The other example and a suitable `transparent-tags' are defined
earlier in the same definition form.
Response: It already sees and uses the `transparent-tags' mark and the
other example."
      (emtg:define:th:with-empty-tagset ()
	 (emtg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (transparent-tags () (dummy-type-tag))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b)
	    (item
	       ((dummy-type-tag a))
	       (list 
		  (emtg (dummy-type-tag b)) 
		  "Includes another item's value")))
	 (equal
	    (emtg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))


   (  "Situation: A definition form uses the value of another example.
The other example and a suitable `transparent-tags' are defined later in the
same definition form.
Response: It already sees and uses the `transparent-tags' mark and the
other example."
      (emtg:define:th:with-empty-tagset ()
	 (emtg:define
	    dummy-id 
	    ((dummy-tag 1))
	    (item
	       ((dummy-type-tag a))
	       (list 
		  (emtg (dummy-type-tag b)) 
		  "Includes another item's value"))
	    (transparent-tags () (dummy-type-tag))
	    (item
	       ((dummy-type-tag b))
	       'dummy-1-type-b))
	 
	 (equal
	    (emtg (dummy-tag 1)(dummy-type-tag a))
	    (list 'dummy-1-type-b "Includes another item's value"))))
   

   (  "Situation: Two definitions mutually uses each others values.
Response: Error."
      (emtg:define:th:with-empty-tagset ()
	 (rtest:gives-error
	    (emtg:define
	       dummy-id 
	       ((dummy-tag 1))
	       (transparent-tags () (dummy-type-tag))
	       (item
		  ((dummy-type-tag b))
		  (list 
		     (emtg (dummy-type-tag a)) 
		     "First of two mutually recursive items"))
	       (item
		  ((dummy-type-tag a))
		  (list 
		     (emtg (dummy-type-tag b)) 
		     "Second of two mutually recursive items"))))))

   ;;YAGNI yet:

   ;;parallel-items (test in its own thing?)


   ;;Test that tag values are computed just once (use `incf' on some variable).

   ;;doc
   
   )

;;;_  . Tests
(rtest:deftest emtg:kv-matches-p

   (  "Params: Kv is the wrong type (a string)
Response: Non-nil just if kv's key matches filter."
      (rtest:gives-error
	 (emtg:kv-matches-p
	    "Wrong type"
	    'dummy-tag)))

   (  "Params: Filter is a bare symbol, kv is a key/value pair
Response: Non-nil just if kv's key matches filter."
      (emtg:kv-matches-p
	 '(dummy-tag 2)
	 'dummy-tag))
   
   (  "Params: Filter is a bare symbol, kv is a key/value pair
Response: Non-nil just if kv's key matches filter."
      (not
	 (emtg:kv-matches-p
	    '(a t)
	    'dummy-tag)))

   (  "Params: Filter and kv are both bare symbols
Response: Non-nil just if they match."
      (emtg:kv-matches-p
	 'a
	 'a))

   (  "Params: Filter and kv are both bare symbols
Response: Non-nil just if they match."
      (not
	 (emtg:kv-matches-p
	    'a
	    'b)))

   (  "Params: Filter is a key/value pair, kv is a bare symbol
Response: Error."
      (rtest:gives-error
	 (emtg:kv-matches-p
	    'dummy-tag
	    '(dummy-tag 2))))
   
   (  "Params: Filter and kv are both key/value pairs
Response: Non-nil just if they match."
      (emtg:kv-matches-p
	 '(dummy-tag 2)
	 '(dummy-tag 2)))
   
   (  "Params: Filter and kv are both key/value pairs
Response: Non-nil just if they match."
      (not 
	 (emtg:kv-matches-p
	    '(dummy-tag 2)
	    '(dummy-tag 1))))

   (  "Params: Filter and kv are both key/value pairs
Response: Non-nil just if they match."
      (not 
	 (emtg:kv-matches-p
	    '(dummy-tag 2)
	    '(a t)))))

;;;_  . Tests
(put 'emtg:some-kv-matches 'rtest:test-thru
   'emtg:filter-one)

;;;_  . Test helper
(defun emtg:filter-one:th (list expected-values)
   ""
   (rtest:sets=
      (mapcar
	 #'emtg:example->value
	 list)
      expected-values))

;;;_  . Tests
(rtest:deftest emtg:filter-one
   
   ;;Validate emtg:filter-one
   (  "Situation: Passed the entire list and a list of the values on it
Response: Gives non-nil."
      (emtg:th:with-example-examples
	 (emtg:filter-one:th
	    emtg:all-examples
	    '(tag-1-a tag-2-a tag-2-b))))

   (  "Situation: Passed the entire list and a different list of values
Response: Gives nil."
      (emtg:th:with-example-examples
	 (not
	    (emtg:filter-one:th
	       emtg:all-examples
	       '(tag-1-a tag-2-b)))))

   ;;Test emtg:filter-one itself
   (  "Param: A tag/value pair
Response: Just examples matching that tag/value pair are kept."
      (emtg:th:with-example-examples
	 (emtg:filter-one:th
	    (emtg:filter-one 
	       emtg:all-examples
	       '(dummy-tag 1))
	    '(tag-1-a))))
   

   (  "Param: A tag/value pair
Response: Just examples matching that tag/value pair are kept."
      (emtg:th:with-example-examples
	 (emtg:filter-one:th
	    (emtg:filter-one 
	       emtg:all-examples
	       '(dummy-tag 2))
	    '(tag-2-a tag-2-b))))
   

   (  "Param: A bare tag.
Response: Every example matching that tag is kept."
      (emtg:th:with-example-examples
	 (emtg:filter-one:th
	    (emtg:filter-one 
	       emtg:all-examples
	       'a)
	    '(tag-1-a tag-2-a))))
   
   (  "Situation: At least one example has a tagset has a bare tag.
Param: A bare tag.
Response: Filters normally, of course matching that tag."
      (emtg:th:with-example-examples-2
	 (emtg:filter-one:th
	    (emtg:filter-one 
	       emtg:all-examples
	       'c)
	    '(c))))
   

   (  "Situation: At least one example has a tagset has a bare tag (c).
Param: A tag/value pair matching that tag (c)
Response: Error."
      (emtg:th:with-example-examples-2
	 (rtest:gives-error
	    (emtg:filter-one:th
	       (emtg:filter-one 
		  emtg:all-examples
		  '(c t))
	       '(c))))))


;;;_  . Tests
;;It's just about direct.
(rtest:deftest emtg:filter

   ;;Essentially the same as one `emtg' test
   ( "Params: A tag/value pair that matches just one example.
Response: Return that example's value."
      (emtg:filter-one:th
	 (emtg:filter
	    emtg:thd:example-examples
	    '((dummy-tag 1)))
	 '(tag-1-a))))


;;;_  . Tests
(put 'emtg:get-value 'rtest:test-thru
   'emtg)

;;;_  . Tests
(rtest:deftest emtg

   ;;All these clauses are situated in the example tagset
   ;;`emtg:th:with-example-examples'.

   (  "Params: A tag/value pair that matches just one example.
Response: Return that example's value."
      (emtg:th:with-example-examples
	 (equal
	    (emtg (dummy-tag 1))
	    'tag-1-a)))
   
   ;;Gotta improve `rtest:gives-error' for this.
   ;;Error type is TBD
   (  "Params: A tag/value pair that overconstrains; matches no values.
Response: Throw an error of predetermined type."
      (emtg:th:with-example-examples
	 (rtest:gives-error
	    (emtg (dummy-tag 2)))))
   
   ;;Error type is TBD.  A resumable error, if that was available.
   (  "Params: A tag/value pair that underconstrains; matches two values.
Response: Throw an error of predetermined type."
      (emtg:th:with-example-examples
	 (rtest:gives-error
	    (emtg (dummy-tag 0)))))
   
   
   (  "Params: A bare symbol
Behavior: Interpret the symbol as tag whose value we don't care about.
Response: Return the value."
      (emtg:th:with-example-examples
	 (equal
	    (emtg b)
	    'tag-2-b)))
   

   (  "Params: Several (two) tag/value pairs.
Behavior: Constrain on both of them.
Response: Return the value."
      (emtg:th:with-example-examples
	 (equal
	    (emtg (dummy-tag 2)(a t))
	    'tag-2-a)))
   
   ;;Dealing with bare tags in the tagset.
   (  "Situation: Some examples' tagsets includes a bare tag.
Params: That tag, bare.
Response: Return the value."
      (emtg:th:with-example-examples-2
	 (equal
	    (emtg c)
	    'c)))
   

   (  "Situation: Some examples' tagsets includes a bare tag.
Params: That tag, with a value.
Response: Throw an error of predetermined type."
      (emtg:th:with-example-examples-2
	 (rtest:gives-error
	    (emtg (c 1)))))
   

   ;;Co-operation with emtg:narrow.  Specifically, it uses its
   ;;tagset in restricting what is seen.  
   ;;This also tests `emtg:tagset-strip'
   (  "Situation: Inside `emtg:th:with-example-examples',
`emtg:narrow' has narrowed the tagset to only those with tag `a'.
Behavior: See only the examples allowed in that narrowed tagset.
Params: (dummy-tag invalid-2), which would ordinarily fail by being
underconstrained.
Response: Return the value."
      (emtg:th:with-example-examples
	 (emtg:narrow (a) 
	    (equal
	       (emtg (dummy-tag 2))
	       'tag-2-a)))))

;;;_  . Tests
(rtest:deftest emtg:value

   (  "Params: A tag/value pair that matches just one example.
Response: Return that example's value."
      (emtg:th:with-example-examples
	 (equal
	    (emtg:value :narrow ((dummy-tag 1)))
	    'tag-1-a)))
   
   ;;This also tests `emtg:tagset-strip'
   (  "Same as an emtg test, but with `emtg:value' in place of
`emtg'.

Situation: Inside `emtg:th:with-example-examples',
`emtg:narrow' has narrowed the tagset to only those with tag `a'.
Behavior: See only the examples allowed in that narrowed tagset.
Params: (dummy-tag invalid-2), which would ordinarily fail by being
underconstrained.
Response: Return the value."

	 (emtg:th:with-example-examples
	    (emtg:narrow (a) 
	       (equal
		  (emtg:value :narrow ((dummy-tag 2)))
		  'tag-2-a)))))

;;;_  . Tests
(put 'emtg:narrow-f 'rtest:test-thru
   'emtg)

;;;_  . Tests
(rtest:deftest emtg:ignore-tags
   '  ;;Not written.  It was tested thru one use in
   ;;`org-for-code.el'
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   ;;Doesn't handle non-literal transparent-tags 
   )

;;;_  . Test

(rtest:deftest emtg:narrow

   ;;Non-looping operation
   ;;Can constrain what emtg sees
   '
   (  "Situation: There are two examples, different values for a given
tag.
Params: That tag, with one value
Response: Only the matching example is seen."
      (progn)
      )

   ;;Can co-operate with `emtg'
   

   ;;Looping operation, only after emt can accept results:
   ;;Test the looper.
   ;;Normally
   ;;Informs
   ;;With skipping over when some piece can't be found.


   ;;Is informed by emt tester.
   )
;;;_  . Test data for emtg:map
(defconst emtg:thd:examples-2
   (emtg:define+ ;;xmp:1a424ae8-1c28-4875-bdac-6ba6ad9d1a5e
      ((project emtest)(library emtg)(topic map))
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
	    ((part string)) "medium string"))))

;;;_  . Tests
(rtest:deftest emtg:map

   (  "Situation: The defined examples
Operation: Iterate over values of `discriminator'.
Response: 
 * Each part resolves without error
 * Each part passes a type test
 * Returning a list of the parts, we get the expected set."

      (emtg:with emtg:thd:examples-2
	 ((project emtest)(library emtg)(topic map))
	 (assert
	    (rtest:sets= 
	       (emtg:map discriminator x x)
	       '(small medium))
	    t)
	 (let
	    ((results
		(emtg:map discriminator x
		   (check-type (emtg (part number)) integer)
		   (check-type (emtg (part string)) string)
		   (list
		      (emtg (part number))
		      (emtg (part string))))))

	    (assert (rtest:sets= results 
		       '(  (2 "wee")
			   (14 "medium string"))))
	    t)))

   (  "Param: NAME is nil
Response: No errors."

      (emtg:with emtg:thd:examples-2
	 ((project emtest)(library emtg)(topic map))
	 (emtg:map discriminator nil t)
	 t))
   
   ( "Situation: The tag has associated values (`medium') that should
already have been narrowed out of our scope.
Response: Those values are not visited."
      (emtg:with emtg:thd:examples-2
	 ((project emtest)(library emtg)(topic map)(not-medium))
	 (assert
	    (rtest:sets= 
	       (emtg:map discriminator x x)
	       '(small))
	    t)
	 (let
	    ((results
		(emtg:map discriminator x
		   (check-type (emtg (part number)) integer)
		   (check-type (emtg (part string)) string)
		   (list
		      (emtg (part number))
		      (emtg (part string))))))

	    (assert
	       (rtest:sets= results 
		  '(  (2 "wee"))))
	    t))))


;;;_  . Tests
(rtest:deftest emtg:all-tags

   (  "Situation: Using the usual examples.
Response: Returns the expected value."
      (emtg:th:with-example-examples
	 (rtest:sets=
	    (emtg:all-tags)
	    '(dummy-tag a b))))
   
   (  "Situation: Using the second examples.
Response: Returns the expected value."
      (emtg:th:with-example-examples-2
	 (rtest:sets=
	    (emtg:all-tags)
	    '(dummy-tag a b c)))))

;;;_  . Tests
(rtest:deftest emtg:all-tag-args

   (  "Situation: Using the usual examples.
Param: dummy-tag.
Response: Returns a list of the values that dummy-tag takes."
      (emtg:th:with-example-examples
	 (equal 
	    (emtg:all-tag-args 'dummy-tag)
	    '(1 2))))

   (  "Situation: Using the second examples, which have a bare tag.
Response: Returns the symbol `no-arg'."
      (emtg:th:with-example-examples-2
	 (equal 
	    (emtg:all-tag-args 'c)
	    'no-arg)))
   
   ;;YAGNI: When the tag sometimes has an arg and sometimes not (thus
   ;;it can only be dealt with as no-arg) Signal an error?  Ignore the
   ;;situation.

   ;;Visits just values that are still populated.  Tests use data from
   ;;`emtg:map'

   (  "Operation: Visit values of `discriminator'
Response: Finds the expected ones."
      (emtg:with emtg:thd:examples-2
	 ((project emtest)(library emtg)(topic map))
	 (assert
	    (rtest:sets= 
	       (emtg:all-tag-args 'discriminator)
	       '(small medium))
	    t)
	 t))
   

   

   (  "Situation: The tag has associated values (`medium') that should
already have been narrowed out of our scope.
Response: Those values are not found."
      (emtg:with emtg:thd:examples-2
	 ((project emtest)(library emtg)(topic map)(not-medium))
	 (assert
	    (rtest:sets= 
	       (emtg:all-tag-args 'discriminator)
	       '(small))
	    t)
	 t)))

;;;_  . Tests
(rtest:deftest emtg:browse:item->relative-distinction
   '  ;;An inspection test
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn
	 (assert
	    (equal
	       (emtg:browse:item->relative-distinction
		  (emtg (type eg-item)(name 0))
		  '((project emtest)(library emtg)(topic map)))
	       ;;Persister.  But we don't care about the item
	       ;;field, which merely must match the input.  Only the
	       ;;other fields should persist.  See
	       ;;file:/development.org "Masked comparison"
	       )
	    t)
	 t)

      ))


;;;_  . Tests
(rtest:deftest emtg:browse:->stage-1
   ;;Should type-check the output, if that were possible.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn
	 (assert
	    (equal
	       (emtg:browse:->stage-1
		  (emtg:browse:make-relative-distinction
		     :item (emtg (type eg-item)(name 0))))
	    
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
	       (emtg:browse:->stage-1
		  (list
		     (emtg:browse:make-relative-distinction
			:item
			(emtg (type eg-item)(name 0)))
		     (emtg:browse:make-relative-distinction
			:item
			(emtg (type eg-item)(name 1)))))
	       
	       ;;Here put a persisting version
	       )
	    t)
	 t)))

;;;_  . Tests

;;These are inspection tests.
(rtest:deftest emtg:browse:top
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      ;;Inspect this
      (emtg:browse:top
	 '((project emtest)(library emtg)(topic map))
	 '((project emtest)(library emtg)(topic map)))))





;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tagnames/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tagnames/rtest.el ends here
