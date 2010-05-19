;;;_ emtest/testhelp/eg/tests.el --- Tests of eg

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

(require 'emtest/testhelp/eg/testhelp)
;;;_. Body

;;;_  . Define example data
;;This can't use eg because of bootstrap issues.
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

;;;_ , Tests of emt:eg:th:validate-helper-retval

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

;;;_ , Test support

;;$$This should be shared with previous uses in tests.  But there's a
;;bootstrapping problem.  So define real names for these, and use
;;those objects here by name.
(defconst emt:eg:thd:examples
   (emt:eg:define+
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
	    (list '(other-prop other-value))))))


;;;_  . Tests
(emt:deftest-3 emt:eg:th:validate-helper-retval
   (nil
      (progn
	 (emt:doc "Validate this against `emt:eg:define:td:typical-helper-retval'.")
	 (emt:eg:th:validate-helper-retval emt:eg:define:td:typical-helper-retval)))
   (nil
      (progn
	 (emt:doc "Situation: We get a retval that's correct")
	 (emt:doc "Response: Return non-nil.")
	 (emt:eg:th:validate-helper-retval
	    (make-emt:eg:helper-rettype\. :value-info
	       (list
		  (make-emt:eg:valuedef-type\. :tagset nil :value-form 'the-value-form))))))
   (nil
      (progn
	 (emt:doc "Situation: We get a retval that's displaced one place too high")
	 (emt:doc "Response: Return nil.")
	 (not
	    (emt:eg:th:validate-helper-retval
	       (make-emt:eg:helper-rettype\. :value-info
		  (make-emt:eg:valuedef-type\. :tagset nil :value-form 'the-value-form))))))
   (nil
      (progn
	 (emt:doc "Situation: We get a retval whose contents are messed up")
	 (emt:doc "Response: Return nil.")
	 (not
	    (emt:eg:th:validate-helper-retval
	       (make-emt:eg:helper-rettype\. :value-info
		  (list
		     (list 'value nil 13))))))))


;;;_   , Tests
(emt:deftest-3 emt:see-item
   (nil
      (progn
	 (emt:doc "Behavior: The return value validates OK.")
	 (emt:eg:th:validate-helper-retval
	    (emt:see-item nil nil 'the-value-form)))))

;;;_   , Tests
(emt:deftest-3 emt:see-doc
   (nil
      (progn
	 (emt:doc "Behavior: The return value validates OK.")
	 (emt:eg:th:validate-helper-retval
	    (emt:see-doc nil nil "A doc string")))))


;;;_   , Tests
(emt:deftest-3 emt:see-type-must-be
   (nil
      (progn
	 (emt:doc "Situation: In definer, item mismatches type.
Type spec is before item spec.")
	 (emt:doc "Response: error.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (assert
	       (emt:gives-error
		  (emt:eg:define dummy-id nil
		     (type-must-be
			((dummy-type-tag a))
			integer)
		     (item
			((dummy-type-tag a))
			'dummy-1-type-a))
		  wrong-type-argument))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: In definer, item mismatches type.
Item spec is before type spec.")
	 (emt:doc "Response: error.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (assert
	       (emt:gives-error
		  (emt:eg:define dummy-id nil
		     (item
			((dummy-type-tag a))
			'dummy-1-type-a)
		     (type-must-be
			((dummy-type-tag a))
			integer))
		  wrong-type-argument))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: In definer, item matches type")
	 (emt:doc "Response: item is usable as normal.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id nil
	       (type-must-be
		  ((dummy-type-tag a))
		  symbol)
	       (item
		  ((dummy-type-tag a))
		  'dummy-1-type-a))
	    (assert
	       (not
		  (emt:gives-error
		     (emt:eg
			(dummy-type-tag a)))))
	    t))))



;;;_   , Tests
(emt:deftest-3 emt:see-group
   (nil
      (progn
	 (emt:doc "Behavior: The return value validates OK.")
	 (emt:eg:th:validate-helper-retval
	    (emt:see-group nil nil
	       '(item nil the-value-form))))))



;;;_   , Tests
(put 'emt:eg:remove-earlier-defs 'emt:test-thru 'emt:eg:define)


;;;_   , Tests
(put 'emt:eg:propty-match-ctxt-p 'emt:test-thru 'emt:eg:find-properties)


;;;_   , Tests
(emt:deftest-3 emt:eg:find-properties
   (nil
      (progn
	 (emt:doc "Situation: target-tagset exactly matches the only property.")
	 (emt:doc "Response: Return list has just the value of that property.")
	 (equal
	    (emt:eg:find-properties
	       '((dummy-tag 1))
	       (list
		  (make-emt:example\. :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value
		     '(prop-sym1 val1))))
	    '((prop-sym1 val1))))))



;;;_   , Tests
(put 'emt:eg:apply-proplist-specials-to-example 'emt:test-thru 'emt:see-type-must-be)


;;;_   , emt:eg:apply-prpty-makers-to-examples

;;;_    . Test helper emt:eg:apply-prpty-makers-to-examples:th
(defun emt:eg:apply-prpty-makers-to-examples:th (prpty-maker example)
   "Like `emt:eg:apply-prpty-makers-to-examples' but deals only with
singletons.  Returns a singleton"
   (car
      (emt:eg:apply-prpty-makers-to-examples
	 (list prpty-maker)
	 (list example))))

;;;_   , Tests
(emt:deftest-3 emt:eg:apply-prpty-makers-to-examples
   (nil
      (progn
	 (emt:doc "Situation: X's tagset matches PROP's.")
	 (emt:doc "Response: That property is added to X's property-list.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (let*
	       ((docstring emt:eg:define:td:docstring-1)
		  (new-x
		     (emt:eg:apply-prpty-makers-to-examples:th
			(make-emt:example\. :definer-id 'dont-care :value
			   `(doc ,docstring)
			   :tagset
			   '((dummy-tag 1)))
			(make-emt:example\. :definer-id 'dont-care :value 'dont-care :tagset
			   '((dummy-tag 1))
			   :property-list nil))))
	       (equal
		  (emt:example\.-property-list new-x)
		  (list
		     `(doc ,docstring)))))))
   (nil
      (progn
	 (emt:doc "Situation: X's tagset matches PROP's.
X already has a different property.")
	 (emt:doc "Response: The new property is added to X's property-list.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (let*
	       ((docstring emt:eg:define:td:docstring-1)
		  (new-x
		     (emt:eg:apply-prpty-makers-to-examples:th
			(make-emt:example\. :definer-id 'dont-care :value
			   `(doc ,docstring)
			   :tagset
			   '((dummy-tag 1)))
			(make-emt:example\. :definer-id 'dont-care :value 'dont-care :tagset
			   '((dummy-tag 1))
			   :property-list
			   (list
			      '(other-prop other-value))))))
	       (rtest:sets=
		  (emt:example\.-property-list new-x)
		  (list
		     `(doc ,docstring)
		     '(other-prop other-value)))))))
   (nil
      (progn
	 (emt:doc "Situation: X's tagset does not match PROP's.")
	 (emt:doc "Response: X is unchanged.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (let*
	       ((docstring emt:eg:define:td:docstring-1)
		  (new-x
		     (emt:eg:apply-prpty-makers-to-examples:th
			(make-emt:example\. :definer-id 'dont-care :value
			   `(doc ,docstring)
			   :tagset
			   '((dummy-tag 1)))
			(make-emt:example\. :definer-id 'dont-care :value 'dont-care :tagset
			   '((dummy-tag 2))
			   :property-list nil))))
	       (equal
		  (emt:example\.-property-list new-x)
		  nil))))))


;;;_   , Tests
'
(put 'emt:eg:see-new-examples 'emt:test-thru 'emt:eg:define)


;;;_   , Tests

(put 'emt:eg:valuedef->example 'emt:test-thru 'emt:eg:define)


;;;_  . Tests
;;;_   , Test helper (Maybe obsolete)
(defmacro emt:eg:try-valuedef->example:th (&rest body)
   "
Can't use `emt:eg' in body."
   
   `(let
       (  (emt:eg:all-examples ())
	  (emt:eg:delayed-examples ()))
       ,@body))



;;;_   , Test helper
;;Maybe just call `emt:eg:define:th:with-empty-tagset'
(defmacro emt:eg:try-valuedef->example-2:th (&rest body)
   "
Can't use `emt:eg' in body."
   
   `(let
       (  (emt:eg:all-examples ())
	  (emt:eg:all-prpty-makers ()))
       ,@body))


;;;_  . Tests
(emt:deftest-3 emt:eg:try-valuedef->example-2
   (nil
      (progn
	 (emt:doc "Situation: Form has no error.")
	 (emt:doc "Response: Put it on the examples list `emt:eg:all-examples'.
Return the empty list.")
	 (emt:eg:try-valuedef->example-2:th
	    (assert
	       (null
		  (emt:eg:try-valuedef->example-2
		     (make-emt:eg:valuedef-type\. :value-form 12)
		     'dummy-id)))
	    (equal
	       (length emt:eg:all-examples)
	       1))))
   (nil
      (progn
	 (emt:doc "Situation: Form throws an error of type `emt:eg:err-not-available'")
	 (emt:doc "Response: The error doesn't escape.
Return a list consisting of that example.")
	 (emt:eg:try-valuedef->example-2:th
	    (let
	       ((valuedef
		   (make-emt:eg:valuedef-type\. :value-form
		      '(signal 'emt:eg:err:not-found nil))))
	       (assert
		  (equal
		     (emt:eg:try-valuedef->example-2 valuedef 'dummy-id)
		     (list valuedef))
		  t)
	       (assert
		  (equal
		     (length emt:eg:all-examples)
		     0))
	       t)))))


;;;_  . Tests
(put 'emt:eg:valuedef->property 'emt:test-thru 'emt:eg:define)


;;;_  . Tests
(put 'emt:eg:tagset-strip 'emt:test-thru 'emt:eg:tagset-strip-transparents)


;;;_  . Tests
(emt:deftest-3 emt:eg:tagset-strip-transparents
   (nil
      (progn
	 (emt:doc "Situation: No transparent-tags among props")
	 (emt:doc "Response: Returns exactly the tagset.")
	 (let
	    ((tagset
		'((dummy-tag 1))))
	    (equal
	       (emt:eg:tagset-strip-transparents tagset nil)
	       tagset))))
   (nil
      (progn
	 (emt:doc "Situation: No transparent-tags among props")
	 (emt:doc "Response: Returns tagset with matching tags part removed.")
	 (let
	    ((tagset
		'((dummy-tag 1)
		    (dummy-tag-2 2))))
	    (equal
	       (emt:eg:tagset-strip-transparents tagset
		  '((transparent-tags
		       (dummy-tag))))
	       '((dummy-tag-2 2))))))
   (nil
      (progn
	 (emt:doc "Situation: tagset contains a singleton item,")
	 (emt:doc "Behavior: Still works.")
	 (let
	    ((tagset
		'(dummy-tag)))
	    (equal
	       (emt:eg:tagset-strip-transparents tagset nil)
	       tagset)))))


;;;_  . Tests
(put 'emt:eg:define-f 'emt:test-thru 'emt:eg:define)

;;;_  . Tests

(emt:deftest-3 emt:eg:define
   (nil
      (progn
	 (emt:doc "Validation: `emt:eg:all-examples' does not have anything defined
by the dummy definers (which deliberately re-use the same ID).")
	 (not
	    (member* 'dummy-id emt:eg:all-examples :key #'emt:example\.-definer-id))))
   (nil
      (progn
	 (emt:doc "Validation: `emt:eg:all-prpty-makers' does not have anything
defined by the dummy definers (which deliberately re-use the same ID).")
	 (not
	    (member* 'dummy-id emt:eg:all-prpty-makers :key #'emt:example\.-definer-id))))
   (nil
      (progn
	 (emt:doc "Situation: There are no examples in `emt:eg:all-examples'.
Afterwards: There is an example in `emt:eg:all-examples'.")
	 (with-mock
	    (stub emt:see-group => emt:eg:define:td:typical-helper-retval)
	    (emt:eg:define:th:with-empty-tagset nil
	       (emt:eg:define dummy-id nil nil)
	       (and
		  (equal
		     (length emt:eg:all-examples)
		     1)
		  (member
		     (make-emt:example\. :definer-id 'dummy-id :tagset
			'((dummy-tag 1))
			:value 12)
		     emt:eg:all-examples)
		  t)))))
   (nil
      (progn
	 (emt:doc "Situation: One example, different definer ID.
Afterwards: There are two examples in `emt:eg:all-examples'.  One is
the new one")
	 (with-mock
	    (stub emt:see-group => emt:eg:define:td:typical-helper-retval)
	    (emt:eg:define:th:with-empty-tagset
	       (:examples
		  (list
		     (make-emt:example\. :definer-id 'dummy-id-2 :tagset
			'((dummy-tag 1))
			:value 13)))
	       (emt:eg:define dummy-id nil nil)
	       (and
		  (equal
		     (length emt:eg:all-examples)
		     2)
		  (member
		     (make-emt:example\. :definer-id 'dummy-id :tagset
			'((dummy-tag 1))
			:value 12)
		     emt:eg:all-examples)
		  t)))))
   (nil
      (progn
	 (emt:doc "Situation: One example exists, same definer ID
Afterwards: There is only the new example.")
	 (with-mock
	    (stub emt:see-group => emt:eg:define:td:typical-helper-retval)
	    (emt:eg:define:th:with-empty-tagset
	       (:examples
		  (list
		     (make-emt:example\. :definer-id 'dummy-id :tagset
			'((dummy-tag 1))
			:value 13)))
	       (emt:eg:define dummy-id nil nil)
	       (and
		  (equal
		     (length emt:eg:all-examples)
		     1)
		  (member
		     (make-emt:example\. :definer-id 'dummy-id :tagset
			'((dummy-tag 1))
			:value 12)
		     emt:eg:all-examples)
		  t)))))
   (nil
      (progn
	 (emt:doc "Situation: There are no examples in `emt:eg:all-examples'.
Afterwards: There is an example in `emt:eg:all-examples'.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item nil 12))
	    (and
	       (equal
		  (length emt:eg:all-examples)
		  1)
	       (member
		  (make-emt:example\. :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 12)
		  emt:eg:all-examples)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: One example, different definer ID.
Afterwards: There are two examples in `emt:eg:all-examples'.  One is
the new one")
	 (emt:eg:define:th:with-empty-tagset
	    (:examples
	       (list
		  (make-emt:example\. :definer-id 'dummy-id-2 :tagset
		     '((dummy-tag 1))
		     :value 13)))
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item nil 12))
	    (and
	       (equal
		  (length emt:eg:all-examples)
		  2)
	       (member
		  (make-emt:example\. :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 12)
		  emt:eg:all-examples)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: One example exists, same definer ID
Afterwards: There is only the new example.")
	 (emt:eg:define:th:with-empty-tagset
	    (:examples
	       (list
		  (make-emt:example\. :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 13)))
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item nil 12))
	    (and
	       (equal
		  (length emt:eg:all-examples)
		  1)
	       (member
		  (make-emt:example\. :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 12)
		  emt:eg:all-examples)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: One example exists, same definer ID
Params:  The value of an item is given as a form.")
	 (emt:doc "Response:: The new example have the value of the form, not the form itself.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item nil
		  (+ 6 6)))
	    (and
	       (equal
		  (length emt:eg:all-examples)
		  1)
	       (member
		  (make-emt:example\. :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 12)
		  emt:eg:all-examples)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: A group is nested in the definition.")
	 (emt:doc "Response: No error.  Behaves normally.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (not
	       (rtest:gives-error
		  (emt:eg:define dummy-id nil
		     (group nil)))))))
   (nil
      (progn
	 (emt:doc "Param: ID is something other than a symbol")
	 (emt:doc "Response: Error")
	 (emt:eg:define:th:with-empty-tagset nil
	    (rtest:gives-error
	       (emt:eg:define "String instead of symbol" nil)))))
   (nil
      (progn
	 (emt:doc "Situation: There are no examples in `emt:eg:all-examples'.
Params: Only a doc is defined.")
	 (emt:doc "Behavior: Doesn't create an example.
Afterwards: There is still no example in `emt:eg:all-examples'.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (doc nil "A dummy docstring"))
	    (null emt:eg:all-examples))))
   (nil
      (progn
	 (emt:doc "Params: A doc and an example are defined.  Doc is defined
first. 
The example's tagset is underneath the example's.")
	 (emt:doc "Behavior: Example gets that doc as property `documentation'.
Afterwards: There is one example in `emt:eg:all-examples'.
It has that documentation.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (let
	       ((docstring emt:eg:define:td:docstring-1))
	       (emt:eg:define dummy-id
		  ((dummy-tag 1))
		  (doc nil docstring)
		  (item nil 12))
	       (equal
		  (emt:example\.-property-list
		     (car emt:eg:all-examples))
		  (list
		     (list 'documentation docstring)))))))
   (nil
      (progn
	 (emt:doc "Params: A doc and an example are defined.  
Doc is defined second. 
The example's tagset is underneath the example's.")
	 (emt:doc "Behavior: example gets that doc as property `documentation'.
Afterwards: There is one example in `emt:eg:all-examples'.
It has that documentation.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (let
	       ((docstring emt:eg:define:td:docstring-1))
	       (emt:eg:define dummy-id
		  ((dummy-tag 1))
		  (item nil 12)
		  (doc nil docstring))
	       (equal
		  (emt:example\.-property-list
		     (car emt:eg:all-examples))
		  (list
		     (list 'documentation docstring)))))))
   (nil
      (progn
	 (emt:doc "Params: A doc and an example are defined.  
Doc is defined first, in a separate `emt:eg:define'. 
The example's tagset is underneath the example's.")
	 (emt:doc "Behavior: example gets that doc as property `documentation'.
Afterwards: There is one example in `emt:eg:all-examples'.
It has that documentation.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (let
	       ((docstring emt:eg:define:td:docstring-1))
	       (emt:eg:define dummy-id
		  ((dummy-tag 1))
		  (doc nil docstring))
	       (emt:eg:define dummy-id-2
		  ((dummy-tag 1))
		  (item nil 12))
	       (equal
		  (emt:example\.-property-list
		     (car emt:eg:all-examples))
		  (list
		     (list 'documentation docstring)))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emt:eg' to get the value of
another example. 
A `transparent-tags' has been defined that relates to this item and
allows it to see the value.
The `emt:eg' doesn't mention the tag that's made transparent
\\(otherwise we'd only be proving that emt:eg manages transparency)
The `emt:eg' is still narrow enough to naturally get just one item.")
	 (emt:doc "Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag b)
		     (other-tag b))
		  'dummy-1-type-b)
	       (transparent-tags nil
		  (dummy-type-tag)))
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag a))
		  (list
		     (emt:eg
			(other-tag b))
		     "Includes another item's value")))
	    (equal
	       (emt:eg
		  (dummy-tag 1)
		  (dummy-type-tag a))
	       (list 'dummy-1-type-b "Includes another item's value")))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emt:eg' to get the value of
another example.
There's no `transparent-tags' property.
And the narrowing tags `emt:eg' uses are distinct from the differentiating tags
between the examples.")
	 (emt:doc "Response: Error.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b))
	    (rtest:gives-error
	       (emt:eg:define dummy-id
		  ((dummy-tag 1))
		  (item
		     ((another-dummy-type-tag a))
		     (list
			(emt:eg
			   (dummy-type-tag b))
			"Includes another item's value")))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emt:eg' to get the value of
another example.
There's no `transparent-tags' property.
But the narrowing tags `emt:eg' uses subsume the differentiating tags
between the examples.")
	 (emt:doc "Response: Succeeds.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((same-dummy-type-tag b))
		  'dummy-1-type-b))
	    (not
	       (rtest:gives-error
		  (emt:eg:define dummy-id
		     ((dummy-tag 1))
		     (item
			((same-dummy-type-tag a))
			(list
			   (emt:eg
			      (same-dummy-type-tag b))
			   "Includes another item's value"))))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emt:eg' to get the value of
another example.

There is a `transparent-tags' property but it doesn't let that
call of `emt:eg' see the example it's trying to use.")
	 (emt:doc "Response: Error.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b)
	       (transparent-tags nil
		  (a-different-tag)))
	    (rtest:gives-error
	       (emt:eg:define dummy-id
		  ((dummy-tag 1))
		  (item
		     ((another-dummy-type-tag a))
		     (list
			(emt:eg
			   (dummy-type-tag b))
			"Includes another item's value")))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emt:eg' to get the value of
another example.
There is a suitable `transparent-tags' property.
The example it's trying to use doesn't exist.")
	 (emt:doc "Response: Error.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id-different
	       ((dummy-tag 1))
	       (transparent-tags nil
		  (a-different-tag)))
	    (rtest:gives-error
	       (emt:eg:define dummy-id
		  ((dummy-tag 1))
		  (item
		     ((dummy-type-tag a))
		     (list
			(emt:eg
			   (dummy-type-tag b))
			"Includes another item's value")))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emt:eg' to get the value of
another example.
That example existed before but now doesn't.")
	 (emt:doc "Response: Error.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id-different
	       ((dummy-tag 1))
	       (transparent-tags nil
		  (dummy-type-tag)))
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b))
	    (rtest:gives-error
	       (emt:eg:define dummy-id
		  ((dummy-tag 1))
		  (item
		     ((dummy-type-tag a))
		     (list
			(emt:eg
			   (dummy-type-tag b))
			"Includes another item's value")))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emt:eg' to get the value of
another example. 
A `transparent-tags' has been defined inside a group.
It relates to this item and allows it to see the value.
The `emt:eg' doesn't mention the tag that's made transparent
\\(otherwise we'd only be proving that emt:eg manages transparency)
The `emt:eg' is still narrow enough to naturally get just one item.")
	 (emt:doc "Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((all-in-this-tag 1)
		     (dummy-type-tag b)
		     (other-tag b))
		  'dummy-1-type-b)
	       (group
		  ((all-in-this-tag 1))
		  (transparent-tags nil
		     (dummy-type-tag))))
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((all-in-this-tag 1)
		     (dummy-type-tag a))
		  (list
		     (emt:eg
			(other-tag b))
		     "Includes another item's value")))
	    (equal
	       (emt:eg
		  (dummy-tag 1)
		  (dummy-type-tag a))
	       (list 'dummy-1-type-b "Includes another item's value")))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emt:eg' to get the value of
another example. 
Two `transparent-tags' have been defined.  Both apply to this item.
Collectively, they remove all the blocking tags to allow it to see the
value.

The `emt:eg' doesn't mention the tag that's made transparent
\\(otherwise we'd only be proving that emt:eg manages transparency)
The `emt:eg' is still narrow enough to naturally get just one item.")
	 (emt:doc "Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((all-in-this-tag 1)
		     (dummy-type-tag b)
		     (dummy-type-tag-2 b)
		     (other-tag b))
		  'dummy-1-type-b)
	       (transparent-tags nil
		  (dummy-type-tag))
	       (transparent-tags
		  ((all-in-this-tag 1))
		  (dummy-type-tag-2)))
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((all-in-this-tag 1)
		     (dummy-type-tag a)
		     (dummy-type-tag-2 a))
		  (list
		     (emt:eg
			(other-tag b))
		     "Includes another item's value")))
	    (equal
	       (emt:eg
		  (dummy-tag 1)
		  (dummy-type-tag a))
	       (list 'dummy-1-type-b "Includes another item's value")))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses the value of another example.
The other example and a suitable `transparent-tags' are defined
earlier in the same definition form.")
	 (emt:doc "Response: It already sees and uses the `transparent-tags' mark and the
other example.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (transparent-tags nil
		  (dummy-type-tag))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b)
	       (item
		  ((dummy-type-tag a))
		  (list
		     (emt:eg
			(dummy-type-tag b))
		     "Includes another item's value")))
	    (equal
	       (emt:eg
		  (dummy-tag 1)
		  (dummy-type-tag a))
	       (list 'dummy-1-type-b "Includes another item's value")))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses the value of another example.
The other example and a suitable `transparent-tags' are defined later in the
same definition form.")
	 (emt:doc "Response: It already sees and uses the `transparent-tags' mark and the
other example.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (emt:eg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag a))
		  (list
		     (emt:eg
			(dummy-type-tag b))
		     "Includes another item's value"))
	       (transparent-tags nil
		  (dummy-type-tag))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b))
	    (equal
	       (emt:eg
		  (dummy-tag 1)
		  (dummy-type-tag a))
	       (list 'dummy-1-type-b "Includes another item's value")))))
   (nil
      (progn
	 (emt:doc "Situation: Two definitions mutually uses each others values.")
	 (emt:doc "Response: Error.")
	 (emt:eg:define:th:with-empty-tagset nil
	    (rtest:gives-error
	       (emt:eg:define dummy-id
		  ((dummy-tag 1))
		  (transparent-tags nil
		     (dummy-type-tag))
		  (item
		     ((dummy-type-tag b))
		     (list
			(emt:eg
			   (dummy-type-tag a))
			"First of two mutually recursive items"))
		  (item
		     ((dummy-type-tag a))
		     (list
			(emt:eg
			   (dummy-type-tag b))
			"Second of two mutually recursive items"))))))))


;;;_  . Tests
(emt:deftest-3 emt:eg:kv-matches-p
   (nil
      (progn
	 (emt:doc "Params: Kv is the wrong type (a string)")
	 (emt:doc "Response: Non-nil just if kv's key matches filter.")
	 (rtest:gives-error
	    (emt:eg:kv-matches-p "Wrong type" 'dummy-tag))))
   (nil
      (progn
	 (emt:doc "Params: Filter is a bare symbol, kv is a key/value pair")
	 (emt:doc "Response: Non-nil just if kv's key matches filter.")
	 (emt:eg:kv-matches-p
	    '(dummy-tag 2)
	    'dummy-tag)))
   (nil
      (progn
	 (emt:doc "Params: Filter is a bare symbol, kv is a key/value pair")
	 (emt:doc "Response: Non-nil just if kv's key matches filter.")
	 (not
	    (emt:eg:kv-matches-p
	       '(a t)
	       'dummy-tag))))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both bare symbols")
	 (emt:doc "Response: Non-nil just if they match.")
	 (emt:eg:kv-matches-p 'a 'a)))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both bare symbols")
	 (emt:doc "Response: Non-nil just if they match.")
	 (not
	    (emt:eg:kv-matches-p 'a 'b))))
   (nil
      (progn
	 (emt:doc "Params: Filter is a key/value pair, kv is a bare symbol")
	 (emt:doc "Response: Error.")
	 (rtest:gives-error
	    (emt:eg:kv-matches-p 'dummy-tag
	       '(dummy-tag 2)))))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both key/value pairs")
	 (emt:doc "Response: Non-nil just if they match.")
	 (emt:eg:kv-matches-p
	    '(dummy-tag 2)
	    '(dummy-tag 2))))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both key/value pairs")
	 (emt:doc "Response: Non-nil just if they match.")
	 (not
	    (emt:eg:kv-matches-p
	       '(dummy-tag 2)
	       '(dummy-tag 1)))))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both key/value pairs")
	 (emt:doc "Response: Non-nil just if they match.")
	 (not
	    (emt:eg:kv-matches-p
	       '(dummy-tag 2)
	       '(a t))))))


;;;_  . Tests
(put 'emt:eg:some-kv-matches 'emt:test-thru 'emt:eg:filter-one)


;;;_  . Test helper
(defun emt:eg:filter-one:th (list expected-values)
   ""
   (rtest:sets=
      (mapcar
	 #'emt:example.-value
	 list)
      expected-values))

;;;_  . Tests
(emt:deftest-3 emt:eg:filter-one
   (nil
      (progn
	 (emt:doc "Situation: Passed the entire list and a list of the values on it")
	 (emt:doc "Response: Gives non-nil.")
	 (emt:eg:th:with-example-examples
	    (emt:eg:filter-one:th emt:eg:all-examples
	       '(tag-1-a tag-2-a tag-2-b)))))
   (nil
      (progn
	 (emt:doc "Situation: Passed the entire list and a different list of values")
	 (emt:doc "Response: Gives nil.")
	 (emt:eg:th:with-example-examples
	    (not
	       (emt:eg:filter-one:th emt:eg:all-examples
		  '(tag-1-a tag-2-b))))))
   (nil
      (progn
	 (emt:doc "Param: A tag/value pair")
	 (emt:doc "Response: Just examples matching that tag/value pair are kept.")
	 (emt:eg:th:with-example-examples
	    (emt:eg:filter-one:th
	       (emt:eg:filter-one emt:eg:all-examples
		  '(dummy-tag 1))
	       '(tag-1-a)))))
   (nil
      (progn
	 (emt:doc "Param: A tag/value pair")
	 (emt:doc "Response: Just examples matching that tag/value pair are kept.")
	 (emt:eg:th:with-example-examples
	    (emt:eg:filter-one:th
	       (emt:eg:filter-one emt:eg:all-examples
		  '(dummy-tag 2))
	       '(tag-2-a tag-2-b)))))
   (nil
      (progn
	 (emt:doc "Param: A bare tag.")
	 (emt:doc "Response: Every example matching that tag is kept.")
	 (emt:eg:th:with-example-examples
	    (emt:eg:filter-one:th
	       (emt:eg:filter-one emt:eg:all-examples 'a)
	       '(tag-1-a tag-2-a)))))
   (nil
      (progn
	 (emt:doc "Situation: At least one example has a tagset has a bare tag.
Param: A bare tag.")
	 (emt:doc "Response: Filters normally, of course matching that tag.")
	 (emt:eg:th:with-example-examples-2
	    (emt:eg:filter-one:th
	       (emt:eg:filter-one emt:eg:all-examples 'c)
	       '(c)))))
   (nil
      (progn
	 (emt:doc "Situation: At least one example has a tagset has a bare tag (c).
Param: A tag/value pair matching that tag (c)")
	 (emt:doc "Response: Error.")
	 (emt:eg:th:with-example-examples-2
	    (rtest:gives-error
	       (emt:eg:filter-one:th
		  (emt:eg:filter-one emt:eg:all-examples
		     '(c t))
		  '(c)))))))



;;;_  . Tests
;;It's just about direct.
(emt:deftest-3 emt:eg:filter
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that matches just one example.")
	 (emt:doc "Response: Return that example's value.")
	 (emt:eg:filter-one:th
	    (emt:eg:filter emt:eg:thd:example-examples
	       '((dummy-tag 1)))
	    '(tag-1-a)))))



;;;_  . Tests
(put 'emt:eg:get-value 'emt:test-thru 'emt:eg)


;;;_  . Tests
(emt:deftest-3 emt:eg
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that matches just one example.")
	 (emt:doc "Response: Return that example's value.")
	 (emt:eg:th:with-example-examples
	    (equal
	       (emt:eg
		  (dummy-tag 1))
	       'tag-1-a))))
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that overconstrains; matches no values.")
	 (emt:doc "Response: Throw an error of predetermined type.")
	 (emt:eg:th:with-example-examples
	    (rtest:gives-error
	       (emt:eg
		  (dummy-tag 2))))))
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that underconstrains; matches two values.")
	 (emt:doc "Response: Throw an error of predetermined type.")
	 (emt:eg:th:with-example-examples
	    (rtest:gives-error
	       (emt:eg
		  (dummy-tag 0))))))
   (nil
      (progn
	 (emt:doc "Params: A bare symbol")
	 (emt:doc "Behavior: Interpret the symbol as tag whose value we don't care about.")
	 (emt:doc "Response: Return the value.")
	 (emt:eg:th:with-example-examples
	    (equal
	       (emt:eg b)
	       'tag-2-b))))
   (nil
      (progn
	 (emt:doc "Params: Several (two) tag/value pairs.")
	 (emt:doc "Behavior: Constrain on both of them.")
	 (emt:doc "Response: Return the value.")
	 (emt:eg:th:with-example-examples
	    (equal
	       (emt:eg
		  (dummy-tag 2)
		  (a t))
	       'tag-2-a))))
   (nil
      (progn
	 (emt:doc "Situation: Some examples' tagsets includes a bare tag.
Params: That tag, bare.")
	 (emt:doc "Response: Return the value.")
	 (emt:eg:th:with-example-examples-2
	    (equal
	       (emt:eg c)
	       'c))))
   (nil
      (progn
	 (emt:doc "Situation: Some examples' tagsets includes a bare tag.
Params: That tag, with a value.")
	 (emt:doc "Response: Throw an error of predetermined type.")
	 (emt:eg:th:with-example-examples-2
	    (rtest:gives-error
	       (emt:eg
		  (c 1))))))
   (nil
      (progn
	 (emt:doc "Situation: Inside `emt:eg:th:with-example-examples',
`emt:eg:narrow' has narrowed the tagset to only those with tag `a'.")
	 (emt:doc "Behavior: See only the examples allowed in that narrowed tagset.
Params: (dummy-tag invalid-2), which would ordinarily fail by being
underconstrained.")
	 (emt:doc "Response: Return the value.")
	 (emt:eg:th:with-example-examples
	    (emt:eg:narrow
	       (a)
	       (equal
		  (emt:eg
		     (dummy-tag 2))
		  'tag-2-a))))))


;;;_  . Tests
(emt:deftest-3 emt:eg:value
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that matches just one example.")
	 (emt:doc "Response: Return that example's value.")
	 (emt:eg:th:with-example-examples
	    (equal
	       (emt:eg:value :narrow
		  ((dummy-tag 1)))
	       'tag-1-a))))
   (nil
      (progn
	 (emt:doc "Same as an emt:eg test, but with `emt:eg:value' in place of
`emt:eg'.")
	 (emt:doc "Situation: Inside `emt:eg:th:with-example-examples',
`emt:eg:narrow' has narrowed the tagset to only those with tag `a'.")
	 (emt:doc "Behavior: See only the examples allowed in that narrowed tagset.
Params: (dummy-tag invalid-2), which would ordinarily fail by being
underconstrained.")
	 (emt:doc "Response: Return the value.")
	 (emt:eg:th:with-example-examples
	    (emt:eg:narrow
	       (a)
	       (equal
		  (emt:eg:value :narrow
		     ((dummy-tag 2)))
		  'tag-2-a))))))


;;;_  . Tests
(put 'emt:eg:narrow-f 'emt:test-thru 'emt:eg)


;;;_  . Tests
(emt:deftest-3 emt:eg:ignore-tags
   ;;Not written.  It was tested thru one use in
   ;;`org-for-code.el'
   ;;Doesn't handle non-literal transparent-tags 
   )



;;;_  . Test
(emt:deftest-3 emt:eg:narrow
   '
   (nil
      (progn
	 (emt:doc "Situation: There are two examples, different values for a given
tag.")
	 (emt:doc "Params: That tag, with one value.")
	 (emt:doc "Response: Only the matching example is seen.")))

   ;;Can co-operate with `emt:eg'

   ;;Non-looping operation:
   ;;Can constrain what emt:eg sees
   

   ;;Looping operation, only after emt can accept results:
   ;;Test the looper.
   ;;Normally
   ;;Informs
   ;;With skipping over when some piece can't be found.


   ;;Is informed by emt tester.
   )


;;;_  . Test data for emt:eg:map
(defconst emt:eg:thd:examples-2
   (emt:eg:define+ ;;xmp:1a424ae8-1c28-4875-bdac-6ba6ad9d1a5e
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
	    ((part string)) "medium string"))))

;;;_  . Tests
(emt:deftest-3 emt:eg:map
   (nil
      (progn
	 (emt:doc "Situation: The defined examples")
	 (emt:doc "Operation: Iterate over values of `discriminator'.")
	 (emt:doc "Response: 
 * Each part resolves without error
 * Each part passes a type test
 * Returning a list of the parts, we get the expected set.")
	 (emt:eg:with emt:eg:thd:examples-2
	    ((project emtest)
	       (library emt:eg)
	       (topic map))
	    (assert
	       (rtest:sets=
		  (emt:eg:map discriminator x x)
		  '(small medium))
	       t)
	    (let
	       ((results
		   (emt:eg:map discriminator x
		      (check-type
			 (emt:eg
			    (part number))
			 integer)
		      (check-type
			 (emt:eg
			    (part string))
			 string)
		      (list
			 (emt:eg
			    (part number))
			 (emt:eg
			    (part string))))))
	       (assert
		  (rtest:sets= results
		     '((2 "wee")
			 (14 "medium string"))))
	       t))))
   (nil
      (progn
	 (emt:doc "Param: NAME is nil")
	 (emt:doc "Response: No errors.")
	 (emt:eg:with emt:eg:thd:examples-2
	    ((project emtest)
	       (library emt:eg)
	       (topic map))
	    (emt:eg:map discriminator nil t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: The tag has associated values (`medium') that should
already have been narrowed out of our scope.")
	 (emt:doc "Response: Those values are not visited.")
	 (emt:eg:with emt:eg:thd:examples-2
	    ((project emtest)
	       (library emt:eg)
	       (topic map)
	       (not-medium))
	    (assert
	       (rtest:sets=
		  (emt:eg:map discriminator x x)
		  '(small))
	       t)
	    (let
	       ((results
		   (emt:eg:map discriminator x
		      (check-type
			 (emt:eg
			    (part number))
			 integer)
		      (check-type
			 (emt:eg
			    (part string))
			 string)
		      (list
			 (emt:eg
			    (part number))
			 (emt:eg
			    (part string))))))
	       (assert
		  (rtest:sets= results
		     '((2 "wee"))))
	       t)))))



;;;_  . Tests
(emt:deftest-3 emt:eg:all-tags
   (nil
      (progn
	 (emt:doc "Situation: Using the usual examples.")
	 (emt:doc "Response: Returns the expected value.")
	 (emt:eg:th:with-example-examples
	    (rtest:sets=
	       (emt:eg:all-tags)
	       '(dummy-tag a b)))))
   (nil
      (progn
	 (emt:doc "Situation: Using the second examples.")
	 (emt:doc "Response: Returns the expected value.")
	 (emt:eg:th:with-example-examples-2
	    (rtest:sets=
	       (emt:eg:all-tags)
	       '(dummy-tag a b c))))))


;;;_  . Tests
(emt:deftest-3 emt:eg:all-tag-args
   (nil
      (progn
	 (emt:doc "Situation: Using the usual examples.
Param: dummy-tag.")
	 (emt:doc "Response: Returns a list of the values that dummy-tag takes.")
	 (emt:eg:th:with-example-examples
	    (equal
	       (emt:eg:all-tag-args 'dummy-tag)
	       '(1 2)))))
   (nil
      (progn
	 (emt:doc "Situation: Using the second examples, which have a bare tag.")
	 (emt:doc "Response: Returns the symbol `no-arg'.")
	 (emt:eg:th:with-example-examples-2
	    (equal
	       (emt:eg:all-tag-args 'c)
	       'no-arg))))
   (nil
      (progn
	 (emt:doc "Operation: Visit values of `discriminator'")
	 (emt:doc "Response: Finds the expected ones.")
	 (emt:eg:with emt:eg:thd:examples-2
	    ((project emtest)
	       (library emt:eg)
	       (topic map))
	    (assert
	       (rtest:sets=
		  (emt:eg:all-tag-args 'discriminator)
		  '(small medium))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: The tag has associated values (`medium') that should
already have been narrowed out of our scope.")
	 (emt:doc "Response: Those values are not found.")
	 (emt:eg:with emt:eg:thd:examples-2
	    ((project emtest)
	       (library emt:eg)
	       (topic map)
	       (not-medium))
	    (assert
	       (rtest:sets=
		  (emt:eg:all-tag-args 'discriminator)
		  '(small))
	       t)
	    t))))


;;;_  . Tests
(emt:deftest-3 emt:eg:browse:item->relative-distinction
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (progn
	     (assert
		(equal
		   (emt:eg:browse:item->relative-distinction
		      (emt:eg
			 (type eg-item)
			 (name 0))
		      '((project emtest)
			  (library emt:eg)
			  (topic map))))
		t)
	     t))))



;;;_  . Tests
(emt:deftest-3 emt:eg:browse:format-relative-distinction
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (progn
	     (assert
		(equal
		   (emt:eg:browse:format-relative-distinction
		      (emt:eg:browse:make-relative-distinction :item
			 (emt:eg
			    (type eg-item)
			    (name 0)))))
		t)
	     t)))
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (progn
	     (assert
		(equal
		   (emt:eg:browse:format-relative-distinction
		      (list
			 (emt:eg:browse:make-relative-distinction :item
			    (emt:eg
			       (type eg-item)
			       (name 0)))
			 (emt:eg:browse:make-relative-distinction :item
			    (emt:eg
			       (type eg-item)
			       (name 1))))))
		t)
	     t))))


;;;_  . Tests

;;These are inspection tests.
(emt:deftest-3 emt:eg:browse:top
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (emt:eg:browse:top
	     '((project emtest)
		 (library emt:eg)
		 (topic map))
	     '((project emtest)
		 (library emt:eg)
		 (topic map))))))






;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/eg/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/eg/tests.el ends here
