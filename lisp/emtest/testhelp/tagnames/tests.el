;;;_ emtest/testhelp/tagnames/tests.el --- Tests of tagnames

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

;;;_ , Tests


;;;_  . emtg:th:validate-helper-retval
(emt:deftest-3 emtg:th:validate-helper-retval
   (nil
      (progn
	 (emt:doc "Validate this against `emtg:define:td:typical-helper-retval'.")
	 (emtg:th:validate-helper-retval emtg:define:td:typical-helper-retval)))
   (nil
      (progn
	 (emt:doc "Situation: We get a retval that's correct")
	 (emt:doc "Response: Return non-nil.")
	 (emtg:th:validate-helper-retval
	    (emtg:make-helper-rettype :value-info
	       (list
		  (emtg:make-valuedef-type :tagset nil :value-form 'the-value-form))))))
   (nil
      (progn
	 (emt:doc "Situation: We get a retval that's displaced one place too high")
	 (emt:doc "Response: Return nil.")
	 (not
	    (emtg:th:validate-helper-retval
	       (emtg:make-helper-rettype :value-info
		  (emtg:make-valuedef-type :tagset nil :value-form 'the-value-form))))))
   (nil
      (progn
	 (emt:doc "Situation: We get a retval whose contents are messed up")
	 (emt:doc "Response: Return nil.")
	 (not
	    (emtg:th:validate-helper-retval
	       (emtg:make-helper-rettype :value-info
		  (list
		     (list 'value nil 13))))))))


;;;_   , Tests
(emt:deftest-3 emt:see-item
   (nil
      (progn
	 (emt:doc "Behavior: The return value validates OK.")
	 (emtg:th:validate-helper-retval
	    (emt:see-item nil nil 'the-value-form)))))

;;;_   , Tests
(emt:deftest-3 emt:see-doc
   (nil
      (progn
	 (emt:doc "Behavior: The return value validates OK.")
	 (emtg:th:validate-helper-retval
	    (emt:see-doc nil nil "A doc string")))))


;;;_   , Tests
(emt:deftest-3 emt:see-type-must-be
   (nil
      (progn
	 (emt:doc "Situation: In definer, item mismatches type.
Type spec is before item spec.")
	 (emt:doc "Response: error.")
	 (emtg:define:th:with-empty-tagset nil
	    (emt:assert
	       (emth:gives-error
		  (emtg:define dummy-id nil
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
	 (emtg:define:th:with-empty-tagset nil
	    (emt:assert
	       (emth:gives-error
		  (emtg:define dummy-id nil
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
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id nil
	       (type-must-be
		  ((dummy-type-tag a))
		  symbol)
	       (item
		  ((dummy-type-tag a))
		  'dummy-1-type-a))
	    (emt:assert
	       (not
		  (emth:gives-error
		     (emtg
			(dummy-type-tag a)))))
	    t))))



;;;_   , Tests
(emt:deftest-3 emt:see-group
   (nil
      (progn
	 (emt:doc "Behavior: The return value validates OK.")
	 (emtg:th:validate-helper-retval
	    (emt:see-group nil nil
	       '(item nil the-value-form))))))



;;;_   , Tests
(put 'emtg:remove-earlier-defs 'emt:test-thru 'emtg:define)


;;;_   , Tests
(put 'emtg:propty-match-ctxt-p 'emt:test-thru 'emtg:find-properties)


;;;_   , Tests
(emt:deftest-3 emtg:find-properties
   (nil
      (progn
	 (emt:doc "Situation: target-tagset exactly matches the only property.")
	 (emt:doc "Response: Return list has just the value of that property.")
	 (equal
	    (emtg:find-properties
	       '((dummy-tag 1))
	       (list
		  (emtg:make-example :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value
		     '(prop-sym1 val1))))
	    '((prop-sym1 val1))))))



;;;_   , Tests
(put 'emtg:apply-proplist-specials-to-example 'emt:test-thru 'emt:see-type-must-be)


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
(emt:deftest-3 emtg:apply-prpty-makers-to-examples
   (nil
      (progn
	 (emt:doc "Situation: X's tagset matches PROP's.")
	 (emt:doc "Response: That property is added to X's property-list.")
	 (emtg:define:th:with-empty-tagset nil
	    (let*
	       ((docstring emtg:define:td:docstring-1)
		  (new-x
		     (emtg:apply-prpty-makers-to-examples:th
			(emtg:make-example :definer-id 'dont-care :value
			   `(doc ,docstring)
			   :tagset
			   '((dummy-tag 1)))
			(emtg:make-example :definer-id 'dont-care :value 'dont-care :tagset
			   '((dummy-tag 1))
			   :property-list nil))))
	       (equal
		  (emtg:example-property-list new-x)
		  (list
		     `(doc ,docstring)))))))
   (nil
      (progn
	 (emt:doc "Situation: X's tagset matches PROP's.
X already has a different property.")
	 (emt:doc "Response: The new property is added to X's property-list.")
	 (emtg:define:th:with-empty-tagset nil
	    (let*
	       ((docstring emtg:define:td:docstring-1)
		  (new-x
		     (emtg:apply-prpty-makers-to-examples:th
			(emtg:make-example :definer-id 'dont-care :value
			   `(doc ,docstring)
			   :tagset
			   '((dummy-tag 1)))
			(emtg:make-example :definer-id 'dont-care :value 'dont-care :tagset
			   '((dummy-tag 1))
			   :property-list
			   (list
			      '(other-prop other-value))))))
	       (emth:sets=
		  (emtg:example-property-list new-x)
		  (list
		     `(doc ,docstring)
		     '(other-prop other-value)))))))
   (nil
      (progn
	 (emt:doc "Situation: X's tagset does not match PROP's.")
	 (emt:doc "Response: X is unchanged.")
	 (emtg:define:th:with-empty-tagset nil
	    (let*
	       ((docstring emtg:define:td:docstring-1)
		  (new-x
		     (emtg:apply-prpty-makers-to-examples:th
			(emtg:make-example :definer-id 'dont-care :value
			   `(doc ,docstring)
			   :tagset
			   '((dummy-tag 1)))
			(emtg:make-example :definer-id 'dont-care :value 'dont-care :tagset
			   '((dummy-tag 2))
			   :property-list nil))))
	       (equal
		  (emtg:example-property-list new-x)
		  nil))))))


;;;_   , Tests
'
(put 'emtg:see-new-examples 'emt:test-thru 'emtg:define)


;;;_   , Tests

(put 'emtg:valuedef->example 'emt:test-thru 'emtg:define)


;;;_  . Tests
;;;_   , Test helper (Maybe obsolete)
(defmacro emtg:try-valuedef->example:th (&rest body)
   "
Can't use `emtg' in body."
   
   `(let
       (  (emtg:all-examples ())
	  (emtg:delayed-examples ()))
       ,@body))



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
(emt:deftest-3 emtg:try-valuedef->example-2
   (nil
      (progn
	 (emt:doc "Situation: Form has no error.")
	 (emt:doc "Response: Put it on the examples list `emtg:all-examples'.
Return the empty list.")
	 (emtg:try-valuedef->example-2:th
	    (emt:assert
	       (null
		  (emtg:try-valuedef->example-2
		     (emtg:make-valuedef-type :value-form 12)
		     'dummy-id)))
	    (equal
	       (length emtg:all-examples)
	       1))))
   (nil
      (progn
	 (emt:doc "Situation: Form throws an error of type `emtg:err-not-available'")
	 (emt:doc "Response: The error doesn't escape.
Return a list consisting of that example.")
	 (emtg:try-valuedef->example-2:th
	    (let
	       ((valuedef
		   (emtg:make-valuedef-type :value-form
		      '(signal 'emtg:err:not-found nil))))
	       (emt:assert
		  (equal
		     (emtg:try-valuedef->example-2 valuedef 'dummy-id)
		     (list valuedef))
		  t)
	       (emt:assert
		  (equal
		     (length emtg:all-examples)
		     0))
	       t)))))


;;;_  . Tests
(put 'emtg:valuedef->property 'emt:test-thru 'emtg:define)


;;;_  . Tests
(put 'emtg:tagset-strip 'emt:test-thru 'emtg:tagset-strip-transparents)


;;;_  . Tests
(emt:deftest-3 emtg:tagset-strip-transparents
   (nil
      (progn
	 (emt:doc "Situation: No transparent-tags among props")
	 (emt:doc "Response: Returns exactly the tagset.")
	 (let
	    ((tagset
		'((dummy-tag 1))))
	    (equal
	       (emtg:tagset-strip-transparents tagset nil)
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
	       (emtg:tagset-strip-transparents tagset
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
	       (emtg:tagset-strip-transparents tagset nil)
	       tagset)))))


;;;_  . Tests
(put 'emtg:define-f 'emt:test-thru 'emtg:define)

;;;_  . Tests

(emt:deftest-3 emtg:define
   (nil
      (progn
	 (emt:doc "Validation: `emtg:all-examples' does not have anything defined
by the dummy definers (which deliberately re-use the same ID).")
	 (not
	    (member* 'dummy-id emtg:all-examples :key #'emtg:example-definer-id))))
   (nil
      (progn
	 (emt:doc "Validation: `emtg:*all-prpty-makers*' does not have anything
defined by the dummy definers (which deliberately re-use the same ID).")
	 (not
	    (member* 'dummy-id emtg:*all-prpty-makers* :key #'emtg:example-definer-id))))
   (nil
      (progn
	 (emt:doc "Situation: There are no examples in `emtg:all-examples'.
Afterwards: There is an example in `emtg:all-examples'.")
	 (with-mock
	    (stub emt:see-group => emtg:define:td:typical-helper-retval)
	    (emtg:define:th:with-empty-tagset nil
	       (emtg:define dummy-id nil nil)
	       (and
		  (equal
		     (length emtg:all-examples)
		     1)
		  (member
		     (emtg:make-example :definer-id 'dummy-id :tagset
			'((dummy-tag 1))
			:value 12)
		     emtg:all-examples)
		  t)))))
   (nil
      (progn
	 (emt:doc "Situation: One example, different definer ID.
Afterwards: There are two examples in `emtg:all-examples'.  One is
the new one")
	 (with-mock
	    (stub emt:see-group => emtg:define:td:typical-helper-retval)
	    (emtg:define:th:with-empty-tagset
	       (:examples
		  (list
		     (emtg:make-example :definer-id 'dummy-id-2 :tagset
			'((dummy-tag 1))
			:value 13)))
	       (emtg:define dummy-id nil nil)
	       (and
		  (equal
		     (length emtg:all-examples)
		     2)
		  (member
		     (emtg:make-example :definer-id 'dummy-id :tagset
			'((dummy-tag 1))
			:value 12)
		     emtg:all-examples)
		  t)))))
   (nil
      (progn
	 (emt:doc "Situation: One example exists, same definer ID
Afterwards: There is only the new example.")
	 (with-mock
	    (stub emt:see-group => emtg:define:td:typical-helper-retval)
	    (emtg:define:th:with-empty-tagset
	       (:examples
		  (list
		     (emtg:make-example :definer-id 'dummy-id :tagset
			'((dummy-tag 1))
			:value 13)))
	       (emtg:define dummy-id nil nil)
	       (and
		  (equal
		     (length emtg:all-examples)
		     1)
		  (member
		     (emtg:make-example :definer-id 'dummy-id :tagset
			'((dummy-tag 1))
			:value 12)
		     emtg:all-examples)
		  t)))))
   (nil
      (progn
	 (emt:doc "Situation: There are no examples in `emtg:all-examples'.
Afterwards: There is an example in `emtg:all-examples'.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item nil 12))
	    (and
	       (equal
		  (length emtg:all-examples)
		  1)
	       (member
		  (emtg:make-example :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 12)
		  emtg:all-examples)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: One example, different definer ID.
Afterwards: There are two examples in `emtg:all-examples'.  One is
the new one")
	 (emtg:define:th:with-empty-tagset
	    (:examples
	       (list
		  (emtg:make-example :definer-id 'dummy-id-2 :tagset
		     '((dummy-tag 1))
		     :value 13)))
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item nil 12))
	    (and
	       (equal
		  (length emtg:all-examples)
		  2)
	       (member
		  (emtg:make-example :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 12)
		  emtg:all-examples)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: One example exists, same definer ID
Afterwards: There is only the new example.")
	 (emtg:define:th:with-empty-tagset
	    (:examples
	       (list
		  (emtg:make-example :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 13)))
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item nil 12))
	    (and
	       (equal
		  (length emtg:all-examples)
		  1)
	       (member
		  (emtg:make-example :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 12)
		  emtg:all-examples)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: One example exists, same definer ID
Params:  The value of an item is given as a form.")
	 (emt:doc "Response:: The new example have the value of the form, not the form itself.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item nil
		  (+ 6 6)))
	    (and
	       (equal
		  (length emtg:all-examples)
		  1)
	       (member
		  (emtg:make-example :definer-id 'dummy-id :tagset
		     '((dummy-tag 1))
		     :value 12)
		  emtg:all-examples)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: A group is nested in the definition.")
	 (emt:doc "Response: No error.  Behaves normally.")
	 (emtg:define:th:with-empty-tagset nil
	    (not
	       (emth:gives-error
		  (emtg:define dummy-id nil
		     (group nil)))))))
   (nil
      (progn
	 (emt:doc "Param: ID is something other than a symbol")
	 (emt:doc "Response: Error")
	 (emtg:define:th:with-empty-tagset nil
	    (emth:gives-error
	       (emtg:define "String instead of symbol" nil)))))
   (nil
      (progn
	 (emt:doc "Situation: There are no examples in `emtg:all-examples'.
Params: Only a doc is defined.")
	 (emt:doc "Behavior: Doesn't create an example.
Afterwards: There is still no example in `emtg:all-examples'.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (doc nil "A dummy docstring"))
	    (null emtg:all-examples))))
   (nil
      (progn
	 (emt:doc "Params: A doc and an example are defined.  Doc is defined
first. 
The example's tagset is underneath the example's.")
	 (emt:doc "Behavior: Example gets that doc as property `documentation'.
Afterwards: There is one example in `emtg:all-examples'.
It has that documentation.")
	 (emtg:define:th:with-empty-tagset nil
	    (let
	       ((docstring emtg:define:td:docstring-1))
	       (emtg:define dummy-id
		  ((dummy-tag 1))
		  (doc nil docstring)
		  (item nil 12))
	       (equal
		  (emtg:example-property-list
		     (car emtg:all-examples))
		  (list
		     (list 'documentation docstring)))))))
   (nil
      (progn
	 (emt:doc "Params: A doc and an example are defined.  
Doc is defined second. 
The example's tagset is underneath the example's.")
	 (emt:doc "Behavior: example gets that doc as property `documentation'.
Afterwards: There is one example in `emtg:all-examples'.
It has that documentation.")
	 (emtg:define:th:with-empty-tagset nil
	    (let
	       ((docstring emtg:define:td:docstring-1))
	       (emtg:define dummy-id
		  ((dummy-tag 1))
		  (item nil 12)
		  (doc nil docstring))
	       (equal
		  (emtg:example-property-list
		     (car emtg:all-examples))
		  (list
		     (list 'documentation docstring)))))))
   (nil
      (progn
	 (emt:doc "Params: A doc and an example are defined.  
Doc is defined first, in a separate `emtg:define'. 
The example's tagset is underneath the example's.")
	 (emt:doc "Behavior: example gets that doc as property `documentation'.
Afterwards: There is one example in `emtg:all-examples'.
It has that documentation.")
	 (emtg:define:th:with-empty-tagset nil
	    (let
	       ((docstring emtg:define:td:docstring-1))
	       (emtg:define dummy-id
		  ((dummy-tag 1))
		  (doc nil docstring))
	       (emtg:define dummy-id-2
		  ((dummy-tag 1))
		  (item nil 12))
	       (equal
		  (emtg:example-property-list
		     (car emtg:all-examples))
		  (list
		     (list 'documentation docstring)))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emtg' to get the value of
another example. 
A `transparent-tags' has been defined that relates to this item and
allows it to see the value.
The `emtg' doesn't mention the tag that's made transparent
\\(otherwise we'd only be proving that emtg manages transparency)
The `emtg' is still narrow enough to naturally get just one item.")
	 (emt:doc "Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag b)
		     (other-tag b))
		  'dummy-1-type-b)
	       (transparent-tags nil
		  (dummy-type-tag)))
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag a))
		  (list
		     (emtg
			(other-tag b))
		     "Includes another item's value")))
	    (equal
	       (emtg
		  (dummy-tag 1)
		  (dummy-type-tag a))
	       (list 'dummy-1-type-b "Includes another item's value")))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emtg' to get the value of
another example.
There's no `transparent-tags' property.
And the narrowing tags `emtg' uses are distinct from the differentiating tags
between the examples.")
	 (emt:doc "Response: Error.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b))
	    (emth:gives-error
	       (emtg:define dummy-id
		  ((dummy-tag 1))
		  (item
		     ((another-dummy-type-tag a))
		     (list
			(emtg
			   (dummy-type-tag b))
			"Includes another item's value")))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emtg' to get the value of
another example.
There's no `transparent-tags' property.
But the narrowing tags `emtg' uses subsume the differentiating tags
between the examples.")
	 (emt:doc "Response: Succeeds.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((same-dummy-type-tag b))
		  'dummy-1-type-b))
	    (not
	       (emth:gives-error
		  (emtg:define dummy-id
		     ((dummy-tag 1))
		     (item
			((same-dummy-type-tag a))
			(list
			   (emtg
			      (same-dummy-type-tag b))
			   "Includes another item's value"))))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emtg' to get the value of
another example.

There is a `transparent-tags' property but it doesn't let that
call of `emtg' see the example it's trying to use.")
	 (emt:doc "Response: Error.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id-different
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b)
	       (transparent-tags nil
		  (a-different-tag)))
	    (emth:gives-error
	       (emtg:define dummy-id
		  ((dummy-tag 1))
		  (item
		     ((another-dummy-type-tag a))
		     (list
			(emtg
			   (dummy-type-tag b))
			"Includes another item's value")))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emtg' to get the value of
another example.
There is a suitable `transparent-tags' property.
The example it's trying to use doesn't exist.")
	 (emt:doc "Response: Error.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id-different
	       ((dummy-tag 1))
	       (transparent-tags nil
		  (a-different-tag)))
	    (emth:gives-error
	       (emtg:define dummy-id
		  ((dummy-tag 1))
		  (item
		     ((dummy-type-tag a))
		     (list
			(emtg
			   (dummy-type-tag b))
			"Includes another item's value")))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emtg' to get the value of
another example.
That example existed before but now doesn't.")
	 (emt:doc "Response: Error.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id-different
	       ((dummy-tag 1))
	       (transparent-tags nil
		  (dummy-type-tag)))
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b))
	    (emth:gives-error
	       (emtg:define dummy-id
		  ((dummy-tag 1))
		  (item
		     ((dummy-type-tag a))
		     (list
			(emtg
			   (dummy-type-tag b))
			"Includes another item's value")))))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emtg' to get the value of
another example. 
A `transparent-tags' has been defined inside a group.
It relates to this item and allows it to see the value.
The `emtg' doesn't mention the tag that's made transparent
\\(otherwise we'd only be proving that emtg manages transparency)
The `emtg' is still narrow enough to naturally get just one item.")
	 (emt:doc "Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id-different
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
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((all-in-this-tag 1)
		     (dummy-type-tag a))
		  (list
		     (emtg
			(other-tag b))
		     "Includes another item's value")))
	    (equal
	       (emtg
		  (dummy-tag 1)
		  (dummy-type-tag a))
	       (list 'dummy-1-type-b "Includes another item's value")))))
   (nil
      (progn
	 (emt:doc "Situation: A definition form uses `emtg' to get the value of
another example. 
Two `transparent-tags' have been defined.  Both apply to this item.
Collectively, they remove all the blocking tags to allow it to see the
value.

The `emtg' doesn't mention the tag that's made transparent
\\(otherwise we'd only be proving that emtg manages transparency)
The `emtg' is still narrow enough to naturally get just one item.")
	 (emt:doc "Response: Those definitions are available for use.  
Value-maker sees that same tagset that its item sees, except for tags
mentioned in `transparent-tags'.")
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id-different
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
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((all-in-this-tag 1)
		     (dummy-type-tag a)
		     (dummy-type-tag-2 a))
		  (list
		     (emtg
			(other-tag b))
		     "Includes another item's value")))
	    (equal
	       (emtg
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
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (transparent-tags nil
		  (dummy-type-tag))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b)
	       (item
		  ((dummy-type-tag a))
		  (list
		     (emtg
			(dummy-type-tag b))
		     "Includes another item's value")))
	    (equal
	       (emtg
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
	 (emtg:define:th:with-empty-tagset nil
	    (emtg:define dummy-id
	       ((dummy-tag 1))
	       (item
		  ((dummy-type-tag a))
		  (list
		     (emtg
			(dummy-type-tag b))
		     "Includes another item's value"))
	       (transparent-tags nil
		  (dummy-type-tag))
	       (item
		  ((dummy-type-tag b))
		  'dummy-1-type-b))
	    (equal
	       (emtg
		  (dummy-tag 1)
		  (dummy-type-tag a))
	       (list 'dummy-1-type-b "Includes another item's value")))))
   (nil
      (progn
	 (emt:doc "Situation: Two definitions mutually uses each others values.")
	 (emt:doc "Response: Error.")
	 (emtg:define:th:with-empty-tagset nil
	    (emth:gives-error
	       (emtg:define dummy-id
		  ((dummy-tag 1))
		  (transparent-tags nil
		     (dummy-type-tag))
		  (item
		     ((dummy-type-tag b))
		     (list
			(emtg
			   (dummy-type-tag a))
			"First of two mutually recursive items"))
		  (item
		     ((dummy-type-tag a))
		     (list
			(emtg
			   (dummy-type-tag b))
			"Second of two mutually recursive items"))))))))


;;;_  . Tests
(emt:deftest-3 emtg:kv-matches-p
   (nil
      (progn
	 (emt:doc "Params: Kv is the wrong type (a string)")
	 (emt:doc "Response: Non-nil just if kv's key matches filter.")
	 (emth:gives-error
	    (emtg:kv-matches-p "Wrong type" 'dummy-tag))))
   (nil
      (progn
	 (emt:doc "Params: Filter is a bare symbol, kv is a key/value pair")
	 (emt:doc "Response: Non-nil just if kv's key matches filter.")
	 (emtg:kv-matches-p
	    '(dummy-tag 2)
	    'dummy-tag)))
   (nil
      (progn
	 (emt:doc "Params: Filter is a bare symbol, kv is a key/value pair")
	 (emt:doc "Response: Non-nil just if kv's key matches filter.")
	 (not
	    (emtg:kv-matches-p
	       '(a t)
	       'dummy-tag))))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both bare symbols")
	 (emt:doc "Response: Non-nil just if they match.")
	 (emtg:kv-matches-p 'a 'a)))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both bare symbols")
	 (emt:doc "Response: Non-nil just if they match.")
	 (not
	    (emtg:kv-matches-p 'a 'b))))
   (nil
      (progn
	 (emt:doc "Params: Filter is a key/value pair, kv is a bare symbol")
	 (emt:doc "Response: Error.")
	 (emth:gives-error
	    (emtg:kv-matches-p 'dummy-tag
	       '(dummy-tag 2)))))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both key/value pairs")
	 (emt:doc "Response: Non-nil just if they match.")
	 (emtg:kv-matches-p
	    '(dummy-tag 2)
	    '(dummy-tag 2))))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both key/value pairs")
	 (emt:doc "Response: Non-nil just if they match.")
	 (not
	    (emtg:kv-matches-p
	       '(dummy-tag 2)
	       '(dummy-tag 1)))))
   (nil
      (progn
	 (emt:doc "Params: Filter and kv are both key/value pairs")
	 (emt:doc "Response: Non-nil just if they match.")
	 (not
	    (emtg:kv-matches-p
	       '(dummy-tag 2)
	       '(a t))))))


;;;_  . Tests
(put 'emtg:some-kv-matches 'emt:test-thru 'emtg:filter-one)


;;;_  . Test helper
(defun emtg:filter-one:th (list expected-values)
   ""
   (emth:sets=
      (mapcar
	 #'emtg:example->value
	 list)
      expected-values))

;;;_  . Tests
(emt:deftest-3 emtg:filter-one
   (nil
      (progn
	 (emt:doc "Situation: Passed the entire list and a list of the values on it")
	 (emt:doc "Response: Gives non-nil.")
	 (emtg:th:with-example-examples
	    (emtg:filter-one:th emtg:all-examples
	       '(tag-1-a tag-2-a tag-2-b)))))
   (nil
      (progn
	 (emt:doc "Situation: Passed the entire list and a different list of values")
	 (emt:doc "Response: Gives nil.")
	 (emtg:th:with-example-examples
	    (not
	       (emtg:filter-one:th emtg:all-examples
		  '(tag-1-a tag-2-b))))))
   (nil
      (progn
	 (emt:doc "Param: A tag/value pair")
	 (emt:doc "Response: Just examples matching that tag/value pair are kept.")
	 (emtg:th:with-example-examples
	    (emtg:filter-one:th
	       (emtg:filter-one emtg:all-examples
		  '(dummy-tag 1))
	       '(tag-1-a)))))
   (nil
      (progn
	 (emt:doc "Param: A tag/value pair")
	 (emt:doc "Response: Just examples matching that tag/value pair are kept.")
	 (emtg:th:with-example-examples
	    (emtg:filter-one:th
	       (emtg:filter-one emtg:all-examples
		  '(dummy-tag 2))
	       '(tag-2-a tag-2-b)))))
   (nil
      (progn
	 (emt:doc "Param: A bare tag.")
	 (emt:doc "Response: Every example matching that tag is kept.")
	 (emtg:th:with-example-examples
	    (emtg:filter-one:th
	       (emtg:filter-one emtg:all-examples 'a)
	       '(tag-1-a tag-2-a)))))
   (nil
      (progn
	 (emt:doc "Situation: At least one example has a tagset has a bare tag.
Param: A bare tag.")
	 (emt:doc "Response: Filters normally, of course matching that tag.")
	 (emtg:th:with-example-examples-2
	    (emtg:filter-one:th
	       (emtg:filter-one emtg:all-examples 'c)
	       '(c)))))
   (nil
      (progn
	 (emt:doc "Situation: At least one example has a tagset has a bare tag (c).
Param: A tag/value pair matching that tag (c)")
	 (emt:doc "Response: Error.")
	 (emtg:th:with-example-examples-2
	    (emth:gives-error
	       (emtg:filter-one:th
		  (emtg:filter-one emtg:all-examples
		     '(c t))
		  '(c)))))))



;;;_  . Tests
;;It's just about direct.
(emt:deftest-3 emtg:filter
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that matches just one example.")
	 (emt:doc "Response: Return that example's value.")
	 (emtg:filter-one:th
	    (emtg:filter emtg:thd:example-examples
	       '((dummy-tag 1)))
	    '(tag-1-a)))))



;;;_  . Tests
(put 'emtg:get-value 'emt:test-thru 'emtg)


;;;_  . Tests
(emt:deftest-3 emtg
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that matches just one example.")
	 (emt:doc "Response: Return that example's value.")
	 (emtg:th:with-example-examples
	    (equal
	       (emtg
		  (dummy-tag 1))
	       'tag-1-a))))
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that overconstrains; matches no values.")
	 (emt:doc "Response: Throw an error of predetermined type.")
	 (emtg:th:with-example-examples
	    (emth:gives-error
	       (emtg
		  (dummy-tag 2))))))
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that underconstrains; matches two values.")
	 (emt:doc "Response: Throw an error of predetermined type.")
	 (emtg:th:with-example-examples
	    (emth:gives-error
	       (emtg
		  (dummy-tag 0))))))
   (nil
      (progn
	 (emt:doc "Params: A bare symbol")
	 (emt:doc "Behavior: Interpret the symbol as tag whose value we don't care about.")
	 (emt:doc "Response: Return the value.")
	 (emtg:th:with-example-examples
	    (equal
	       (emtg b)
	       'tag-2-b))))
   (nil
      (progn
	 (emt:doc "Params: Several (two) tag/value pairs.")
	 (emt:doc "Behavior: Constrain on both of them.")
	 (emt:doc "Response: Return the value.")
	 (emtg:th:with-example-examples
	    (equal
	       (emtg
		  (dummy-tag 2)
		  (a t))
	       'tag-2-a))))
   (nil
      (progn
	 (emt:doc "Situation: Some examples' tagsets includes a bare tag.
Params: That tag, bare.")
	 (emt:doc "Response: Return the value.")
	 (emtg:th:with-example-examples-2
	    (equal
	       (emtg c)
	       'c))))
   (nil
      (progn
	 (emt:doc "Situation: Some examples' tagsets includes a bare tag.
Params: That tag, with a value.")
	 (emt:doc "Response: Throw an error of predetermined type.")
	 (emtg:th:with-example-examples-2
	    (emth:gives-error
	       (emtg
		  (c 1))))))
   (nil
      (progn
	 (emt:doc "Situation: Inside `emtg:th:with-example-examples',
`emtg:narrow' has narrowed the tagset to only those with tag `a'.")
	 (emt:doc "Behavior: See only the examples allowed in that narrowed tagset.
Params: (dummy-tag invalid-2), which would ordinarily fail by being
underconstrained.")
	 (emt:doc "Response: Return the value.")
	 (emtg:th:with-example-examples
	    (emtg:narrow
	       (a)
	       (equal
		  (emtg
		     (dummy-tag 2))
		  'tag-2-a))))))


;;;_  . Tests
(emt:deftest-3 emtg:value
   (nil
      (progn
	 (emt:doc "Params: A tag/value pair that matches just one example.")
	 (emt:doc "Response: Return that example's value.")
	 (emtg:th:with-example-examples
	    (equal
	       (emtg:value :narrow
		  ((dummy-tag 1)))
	       'tag-1-a))))
   (nil
      (progn
	 (emt:doc "Same as an emtg test, but with `emtg:value' in place of
`emtg'.")
	 (emt:doc "Situation: Inside `emtg:th:with-example-examples',
`emtg:narrow' has narrowed the tagset to only those with tag `a'.")
	 (emt:doc "Behavior: See only the examples allowed in that narrowed tagset.
Params: (dummy-tag invalid-2), which would ordinarily fail by being
underconstrained.")
	 (emt:doc "Response: Return the value.")
	 (emtg:th:with-example-examples
	    (emtg:narrow
	       (a)
	       (equal
		  (emtg:value :narrow
		     ((dummy-tag 2)))
		  'tag-2-a))))))


;;;_  . Tests
(put 'emtg:narrow-f 'emt:test-thru 'emtg)


;;;_  . Tests
(emt:deftest-3 emtg:ignore-tags
   ;;Not written.  It was tested thru one use in
   ;;`org-for-code.el'
   ;;Doesn't handle non-literal transparent-tags 
   )



;;;_  . Test
(emt:deftest-3 emtg:narrow
   '
   (nil
      (progn
	 (emt:doc "Situation: There are two examples, different values for a given
tag.")
	 (emt:doc "Params: That tag, with one value.")
	 (emt:doc "Response: Only the matching example is seen.")))

   ;;Can co-operate with `emtg'

   ;;Non-looping operation:
   ;;Can constrain what emtg sees
   

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
(emt:deftest-3 emtg:map
   (nil
      (progn
	 (emt:doc "Situation: The defined examples")
	 (emt:doc "Operation: Iterate over values of `discriminator'.")
	 (emt:doc "Response: 
 * Each part resolves without error
 * Each part passes a type test
 * Returning a list of the parts, we get the expected set.")
	 (emtg:with emtg:thd:examples-2
	    ((project emtest)
	       (library emtg)
	       (topic map))
	    (emt:assert
	       (emth:sets=
		  (emtg:map discriminator x x)
		  '(small medium))
	       t)
	    (let
	       ((results
		   (emtg:map discriminator x
		      (check-type
			 (emtg
			    (part number))
			 integer)
		      (check-type
			 (emtg
			    (part string))
			 string)
		      (list
			 (emtg
			    (part number))
			 (emtg
			    (part string))))))
	       (emt:assert
		  (emth:sets= results
		     '((2 "wee")
			 (14 "medium string"))))
	       t))))
   (nil
      (progn
	 (emt:doc "Param: NAME is nil")
	 (emt:doc "Response: No errors.")
	 (emtg:with emtg:thd:examples-2
	    ((project emtest)
	       (library emtg)
	       (topic map))
	    (emtg:map discriminator nil t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: The tag has associated values (`medium') that should
already have been narrowed out of our scope.")
	 (emt:doc "Response: Those values are not visited.")
	 (emtg:with emtg:thd:examples-2
	    ((project emtest)
	       (library emtg)
	       (topic map)
	       (not-medium))
	    (emt:assert
	       (emth:sets=
		  (emtg:map discriminator x x)
		  '(small))
	       t)
	    (let
	       ((results
		   (emtg:map discriminator x
		      (check-type
			 (emtg
			    (part number))
			 integer)
		      (check-type
			 (emtg
			    (part string))
			 string)
		      (list
			 (emtg
			    (part number))
			 (emtg
			    (part string))))))
	       (emt:assert
		  (emth:sets= results
		     '((2 "wee"))))
	       t)))))



;;;_  . Tests
(emt:deftest-3 emtg:all-tags
   (nil
      (progn
	 (emt:doc "Situation: Using the usual examples.")
	 (emt:doc "Response: Returns the expected value.")
	 (emtg:th:with-example-examples
	    (emth:sets=
	       (emtg:all-tags)
	       '(dummy-tag a b)))))
   (nil
      (progn
	 (emt:doc "Situation: Using the second examples.")
	 (emt:doc "Response: Returns the expected value.")
	 (emtg:th:with-example-examples-2
	    (emth:sets=
	       (emtg:all-tags)
	       '(dummy-tag a b c))))))


;;;_  . Tests
(emt:deftest-3 emtg:all-tag-args
   (nil
      (progn
	 (emt:doc "Situation: Using the usual examples.
Param: dummy-tag.")
	 (emt:doc "Response: Returns a list of the values that dummy-tag takes.")
	 (emtg:th:with-example-examples
	    (equal
	       (emtg:all-tag-args 'dummy-tag)
	       '(1 2)))))
   (nil
      (progn
	 (emt:doc "Situation: Using the second examples, which have a bare tag.")
	 (emt:doc "Response: Returns the symbol `no-arg'.")
	 (emtg:th:with-example-examples-2
	    (equal
	       (emtg:all-tag-args 'c)
	       'no-arg))))
   (nil
      (progn
	 (emt:doc "Operation: Visit values of `discriminator'")
	 (emt:doc "Response: Finds the expected ones.")
	 (emtg:with emtg:thd:examples-2
	    ((project emtest)
	       (library emtg)
	       (topic map))
	    (emt:assert
	       (emth:sets=
		  (emtg:all-tag-args 'discriminator)
		  '(small medium))
	       t)
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: The tag has associated values (`medium') that should
already have been narrowed out of our scope.")
	 (emt:doc "Response: Those values are not found.")
	 (emtg:with emtg:thd:examples-2
	    ((project emtest)
	       (library emtg)
	       (topic map)
	       (not-medium))
	    (emt:assert
	       (emth:sets=
		  (emtg:all-tag-args 'discriminator)
		  '(small))
	       t)
	    t))))


;;;_  . Tests
(emt:deftest-3 emtg:browse:item->relative-distinction
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (progn
	     (emt:assert
		(equal
		   (emtg:browse:item->relative-distinction
		      (emtg
			 (type eg-item)
			 (name 0))
		      '((project emtest)
			  (library emtg)
			  (topic map))))
		t)
	     t))))



;;;_  . Tests
(emt:deftest-3 emtg:browse:->stage-1
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (progn
	     (emt:assert
		(equal
		   (emtg:browse:->stage-1
		      (emtg:browse:make-relative-distinction :item
			 (emtg
			    (type eg-item)
			    (name 0)))))
		t)
	     t)))
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (progn
	     (emt:assert
		(equal
		   (emtg:browse:->stage-1
		      (list
			 (emtg:browse:make-relative-distinction :item
			    (emtg
			       (type eg-item)
			       (name 0)))
			 (emtg:browse:make-relative-distinction :item
			    (emtg
			       (type eg-item)
			       (name 1))))))
		t)
	     t))))


;;;_  . Tests

;;These are inspection tests.
(emt:deftest-3 emtg:browse:top
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (emtg:browse:top
	     '((project emtest)
		 (library emtg)
		 (topic map))
	     '((project emtest)
		 (library emtg)
		 (topic map))))))






;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tagnames/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tagnames/tests.el ends here
