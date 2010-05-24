;;;_ emtest/testhelp/misc/rtest.el --- Rtest tests for misc testhelp

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

(require 'rtest-util)
(require 'rtest-define)

;;;_. Body
;;;_  . uti:form-by-option

(rtest:deftest uti:form-by-option

   ("Expands form according to data in options"
      (rtest:retvalue->status
	 (equal
	    (uti:form-by-option
	       '((x bar baz))
	       'x
	       #'(lambda
		    (data)
		    `(foo ,(second data)
			,(third data))))
	    '(foo bar baz))))

	  
   ("With a FORM-ELSE given, expands it when option isn't given."
      (rtest:retvalue->status
	 (equal
	    (uti:form-by-option nil 'x nil
	       #'(lambda
		    (x)
		    '(foo)))
	    '(foo)))))

;;;_ , emt:throws
(rtest:deftest emt:throws

   (  "Situation: Body does not throw anything
Response: Return nil."
      (not 
	 (emt:throws 'example-tag 'just-return-a-value)))
   
   (  "Situation: Body does throw the tag in question, non-nil value.
Response: Return non-nil."
      (and
	 (emt:throws 'example-tag (throw 'example-tag t))
	 t))

   (  "Situation: Body does throw the tag in question, nil value.
Response: Return non-nil."
      (and
	 (emt:throws 'example-tag (throw 'example-tag nil))
	 t))
   
   (  "Situation: Body perversely throws our own tag, `emt:throws:ok'.
Response: Return non-nil."
      (and
	 (emt:throws 'emt:throws:ok (throw 'emt:throws:ok nil))
	 t))

   (  "Situation: tag-name is given by a variable, not a literal
Body does throw the tag in question.
Response: Return non-nil."
      (let
	 ((tag-name 'example-tag))
	 (and
	    (emt:throws tag-name (throw 'example-tag t))
	    t)))
   
   (  "Param: Two tags are given
Situation: Body throws to the first tag.
Response: Return non-nil."
      (and
	 (emt:throws '(example-tag-1 example-tag-2) 
	    (throw 'example-tag-1 nil))
	 t))

   (  "Param: Two tags are given
Situation: Body throws to the second tag.
Response: Return non-nil."
      (and
	 (emt:throws '(example-tag-1 example-tag-2) 
	    (throw 'example-tag-2 nil))
	 t))

   )

;;;_ , emt:assert-throws

(rtest:deftest emt:assert-throws

   (  "Situation: Body does not throw anything
Response: Signals an error."
      (rtest:gives-error
	 (emt:assert-throws 'example-tag 'just-return-a-value)))
   
   (  "Situation: Body does throw the tag in question, non-nil value.
Response: Return non-nil."
      (equal
	 (emt:assert-throws 'example-tag (throw 'example-tag 12))
	 12))


   (  "Situation: tag-name is given by a variable, not a literal
Body does throw the tag in question.
Response: Return non-nil."
      (let
	 ((tag-name 'example-tag))
	 (equal
	    (emt:assert-throws tag-name (throw 'example-tag 12))
	    12)))

   ;;Multiple-tag behavior parallels that of emt:throws.

   )

;;;_ , emt:sets=

(rtest:deftest emt:sets=
   ("Situation: Lists contain the same elements, permuted.
Response: Return non-nil"
      (rtest:retvalue->status
	 (emt:sets= '(1 2 3) '(3 2 1))))
   
   ("Situation: First list contains an element not in second.
Response: Return nil"
      (rtest:retvalue->status
	 (null
	    (emt:sets=
	       '(1 2 3 4)
	       '(3 2 1)))))

   ("Situation: Second list contains an element not in first.
Response: Return nil"
      (rtest:retvalue->status
	 (null
	    (emt:sets=
	       '(3 2 1)
	       '(1 2 3 4)))))

   
   ("Situation: Members are equal but not eq.
Response: Even so, compares true"
      (rtest:retvalue->status
	 (emt:sets=
	    '((1 10))
	    '((1 10)))))

      
   ("Situation: Members are the same but repeated in one.
Response: Compares false"
      (rtest:retvalue->status
	 (not 
	    (emt:sets=
	       '((1 10 1 10))
	       '((1 10))))))
   )
;;;_ , emt:somewhere-in-tree
(rtest:deftest emt:somewhere-in-tree

   (  "Situation: OBJ is in TREE.
Response: Return non-nil."
      (progn
	 (assert
	    (emt:somewhere-in-tree #'eq '(((12))) 12))
	 t))
   
   (  "Situation: OBJ is not in TREE.
Response: Return nil."
      (progn
	 (assert
	    (not
	       (emt:somewhere-in-tree #'eq '(((12))) 13)))
	 t))
      
   )
;;;_ , emt:collect-in-tree
(rtest:deftest emt:collect-in-tree

   (  "Situation: The root of the tree matches PREDICATE.
Response: Return a list of just it."
      (equal
	 (emt:collect-in-tree #'integerp 12)
	 '(12)))
   
      (  "Situation: Some items in tree match PREDICATE.
Response: Return a list of them."
      (equal
	 (emt:collect-in-tree #'integerp '((a 12 b) 12))
	 '(12 12)))
   
   )

;;;_ , emt:let-noprops
(rtest:deftest emt:let-noprops

   (  "Proves: `emt:let-noprops' temporarily rebinds properties."
      (emt:let-noprops '(foo)
	 ;;Now `foo' has no properties
	 (assert (null (symbol-plist 'foo)))
	 ;;Inside a nested emt:let-noprops, give `foo' a property
	 (emt:let-noprops '(foo)
	    (put 'foo 'example-prop 13))

	 ;;Outside, `foo' once again has no properties
	 (assert (null (symbol-plist 'foo)))
	 t))
   
   (  "Proves: `emt:let-noprops' temporarily sets properties null."
      (emt:let-noprops '(foo)
	 ;;Now `foo' has no properties
	 (assert (null (symbol-plist 'foo)))
	 ;;Give `foo' a property
	 (put 'foo 'example-prop 13)
	 (assert (equal (symbol-plist 'foo) '(example-prop 13)))

	 (emt:let-noprops '(foo)
	    ;;Inside a nested emt:let-noprops, `foo' once again has no
	    ;;properties
	    (assert (null (symbol-plist 'foo))))
	 
	 ;;Outside, `foo' once again has the properties we gave it.
	 (assert (equal (symbol-plist 'foo) '(example-prop 13)))
	 t))
   
   )

;;;_ , emt:let-unbound
(rtest:deftest emt:let-unbound
   ;;Bootstrap problem: To test this, we'd really like to have itself,
   ;;to insulate the test from the environment.

   (  "Param: literal list of symbol `foo'.
Response: `foo' is initially unbound in the body.
After `emt:let-unbound' runs, foo is bound again."
      (progn
	 (defconst foo t)
	 ;;Validation
	 (assert (boundp 'foo))

	 (emt:let-unbound '(foo)
	    (assert (not (boundp 'foo))))
	 (assert (boundp 'foo))

	 ;;Clean up after ourselves
	 (makunbound 'foo) 
	 t))
   
   (  "Param: symbolic list of symbol `foo'.
Response: `foo' is initially unbound in the body.
After `emt:let-unbound' runs, foo is bound again."
      (let
	 ((syms '(foo)))
	 (defconst foo t)
	 ;;Validation
	 (assert (boundp 'foo))

	 (emt:let-unbound syms
	    (assert (not (boundp 'foo))))
	 (assert (boundp 'foo))

	 ;;Clean up after ourselves
	 (makunbound 'foo) 
	 t))
   
   ;;Not tested: Cleanliness: Doesn't capture `syms' etc.
   )
;;;_ , emt:flet-unbound
(rtest:deftest emt:flet-unbound
   ;;Bootstrap problem: To test this, we'd really like to have itself,
   ;;to insulate the test from the environment.

   (  "Param: literal list of symbol `foo'.
Response: `foo' is initially unbound in the body.
After `emt:let-unbound' runs, foo is bound again."
      (progn
	 (defun foo ())
	 ;;Validation
	 (assert (fboundp 'foo))

	 (emt:flet-unbound '(foo)
	    (assert (not (fboundp 'foo))))
	 (assert (fboundp 'foo))

	 ;;Clean up after ourselves
	 (fmakunbound 'foo) 
	 t))
   
   (  "Param: symbolic list of symbol `foo'.
Response: `foo' is initially unbound in the body.
After `emt:let-unbound' runs, foo is bound again."
      (let
	 ((syms '(foo)))
	 (defun foo ())
	 ;;Validation
	 (assert (fboundp 'foo))

	 (emt:flet-unbound syms
	    (assert (not (fboundp 'foo))))
	 (assert (fboundp 'foo))

	 ;;Clean up after ourselves
	 (fmakunbound 'foo) 
	 t))
   
   ;;Not tested: Cleanliness: Doesn't capture `syms' etc.
   )


;;;_ , emt:util:all-different
(rtest:deftest emt:util:all-different

   (  "Proves: `emt:util:all-different' returns non-nil just if all
members of SET are different."
      (progn
	 (assert
	    (emt:util:all-different '(1 2)))
	 (assert
	    (not
	       (emt:util:all-different '(1 1))))
	 t)))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/misc/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/misc/rtest.el ends here
