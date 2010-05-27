;;;_ emtest/testhelp/tagnames/testhelp.el --- Testhelp for tagnames

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
(require 'emtest/testhelp/tagnames)
;;;_. Body

;;;_  .  Test helper emtg:th:validate-helper-retval
;;$$Could just use deep-type-check
(defun emtg:th:validate-helper-retval (retval)
   "Validate a helpers' return value as the right type"
   (and
      (emtg:helper-rettype-p retval)
      (let
	 ((value-info (emtg:helper-rettype->value-info retval)))
	 (and
	    (listp value-info)
	    (every
	       #'emtg:valuedef-type-p
	       value-info)))))

;;;_  . Handle adding definitions

;;;_   , Test helper
;;$$CHANGING  The new design barely needs this.
(defmacro* emtg:define:th:with-empty-tagset
   ((&key examples) &rest body)
   ""
   
   `(let
       (  (emtg:all-examples        ,(or examples ()))
	  (emtg:*all-prpty-makers*     ()))
       ,@body))

;;;_  . Define example data
;;This can't use tagnames because of bootstrap issues.
;;;_   , emtg:thd:example-examples
(defconst emtg:thd:example-examples
   (list
      ;;0 examples in (dummy-tag 0)

      ;;1 example in (dummy-tag 1)
      ;;Also tagged (a t)
      (emtg:make-example
	 :definer-id 'dummy-id
	 :tagset    '((dummy-tag 1) (a t))
	 :value      'tag-1-a)

      ;;2 examples in (dummy-tag 2)
      ;;Also tagged (a t) and (b t) (one each) 
      (emtg:make-example
	 :definer-id 'dummy-id
	 :tagset    '((dummy-tag 2) (a t))
	 :value      'tag-2-a)

      (emtg:make-example
	 :definer-id 'dummy-id
	 :tagset    '((dummy-tag 2) (b t))
	 :value      'tag-2-b))
   
   "Example of a list of examples in their full storage format" )

;;;_   , emtg:thd:example-examples-2
(defconst emtg:thd:example-examples-2
   (cons
      (emtg:make-example
	 :definer-id 'dummy-id
	 :tagset    '(c)
	 :value      'c)
      emtg:thd:example-examples)
   "" )

;;;_   , emtg:th:with-example-examples
;;$$CHANGING  The new design barely needs this.
(defmacro emtg:th:with-example-examples (&rest body)
   ""
   ;;Values all name the example they occur in
   `(let
       ;;The larger tagset constrainer is empty.
       (  (emtg:tagset ())
	  (emtg:all-examples emtg:thd:example-examples))
       ,@body))

;;;_   , emtg:th:with-example-examples-2
;;$$CHANGING  The new design barely needs this.
(defmacro emtg:th:with-example-examples-2 (&rest body)
   ""
   ;;Values all name the example they occur in
   `(let
       ;;The larger tagset constrainer is empty.
       (  (emtg:tagset ())
	  (emtg:all-examples emtg:thd:example-examples-2))
       ,@body))

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

;;;_  . emtg:thd:examples

(defconst emtg:thd:examples
   (emtg:define+
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
	    (list '(other-prop other-value))))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tagnames/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tagnames/testhelp.el ends here
