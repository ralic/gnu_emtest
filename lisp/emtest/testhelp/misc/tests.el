;;;_ emtest/testhelp/misc/tests.el --- Rtest tests for misc testhelp

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


;;;_. Body
;;;_  . uti:form-by-option

(emt:deftest-3 uti:form-by-option
   (nil
      (progn
	 (emt:doc "Expands form according to data in options")
	 (rtest:retvalue->status
	    (equal
	       (uti:form-by-option
		  '((x bar baz))
		  'x
		  #'(lambda
		       (data)
		       `(foo ,(second data)
			   ,(third data))))
	       '(foo bar baz)))))
   (nil
      (progn
	 (emt:doc "With a FORM-ELSE given, expands it when option isn't given.")
	 (rtest:retvalue->status
	    (equal
	       (uti:form-by-option nil 'x nil
		  #'(lambda
		       (x)
		       '(foo)))
	       '(foo))))))


;;;_ , emt:throws
(emt:deftest-3 emt:throws
   (nil
      (progn
	 (emt:doc "Situation: Body does not throw anything")
	 (emt:doc "Response: Return nil.")
	 (not
	    (emt:throws 'example-tag 'just-return-a-value))))
   (nil
      (progn
	 (emt:doc "Situation: Body does throw the tag in question, non-nil value.")
	 (emt:doc "Response: Return non-nil.")
	 (and
	    (emt:throws 'example-tag
	       (throw 'example-tag t))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Body does throw the tag in question, nil value.")
	 (emt:doc "Response: Return non-nil.")
	 (and
	    (emt:throws 'example-tag
	       (throw 'example-tag nil))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Body perversely throws our own tag, `emt:throws:ok'.")
	 (emt:doc "Response: Return non-nil.")
	 (and
	    (emt:throws 'emt:throws:ok
	       (throw 'emt:throws:ok nil))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: tag-name is given by a variable, not a literal
Body does throw the tag in question.")
	 (emt:doc "Response: Return non-nil.")
	 (let
	    ((tag-name 'example-tag))
	    (and
	       (emt:throws tag-name
		  (throw 'example-tag t))
	       t))))
   (nil
      (progn
	 (emt:doc "Param: Two tags are given")
	 (emt:doc "Situation: Body throws to the first tag.")
	 (emt:doc "Response: Return non-nil.")
	 (and
	    (emt:throws
	       '(example-tag-1 example-tag-2)
	       (throw 'example-tag-1 nil))
	    t)))
   (nil
      (progn
	 (emt:doc "Param: Two tags are given")
	 (emt:doc "Situation: Body throws to the second tag.")
	 (emt:doc "Response: Return non-nil.")
	 (and
	    (emt:throws
	       '(example-tag-1 example-tag-2)
	       (throw 'example-tag-2 nil))
	    t))))
;;;_  . Tests
(put 'emt:throws-x 'emt:test-thru
   'emt:throws)

;;;_ , emt:assert-throws

(emt:deftest-3 emt:assert-throws
   (nil
      (progn
	 (emt:doc "Situation: Body does not throw anything")
	 (emt:doc "Response: Signals an error.")
	 (rtest:gives-error
	    (emt:assert-throws 'example-tag 'just-return-a-value))))
   (nil
      (progn
	 (emt:doc "Situation: Body does throw the tag in question, non-nil value.")
	 (emt:doc "Response: Return non-nil.")
	 (equal
	    (emt:assert-throws 'example-tag
	       (throw 'example-tag 12))
	    12)))
   (nil
      (progn
	 (emt:doc "Situation: tag-name is given by a variable, not a literal
Body does throw the tag in question.")
	 (emt:doc "Response: Return non-nil.")
	 (let
	    ((tag-name 'example-tag))
	    (equal
	       (emt:assert-throws tag-name
		  (throw 'example-tag 12))
	       12)))))


;;;_ , emt:sets=

(emt:deftest-3 emt:sets=
   (nil
      (progn
	 (emt:doc "Situation: Lists contain the same elements, permuted.")
	 (emt:doc "Response: Return non-nil")
	 (rtest:retvalue->status
	    (emt:sets=
	       '(1 2 3)
	       '(3 2 1)))))
   (nil
      (progn
	 (emt:doc "Situation: First list contains an element not in second.")
	 (emt:doc "Response: Return nil")
	 (rtest:retvalue->status
	    (null
	       (emt:sets=
		  '(1 2 3 4)
		  '(3 2 1))))))
   (nil
      (progn
	 (emt:doc "Situation: Second list contains an element not in first.")
	 (emt:doc "Response: Return nil")
	 (rtest:retvalue->status
	    (null
	       (emt:sets=
		  '(3 2 1)
		  '(1 2 3 4))))))
   (nil
      (progn
	 (emt:doc "Situation: Members are equal but not eq.")
	 (emt:doc "Response: Even so, compares true")
	 (rtest:retvalue->status
	    (emt:sets=
	       '((1 10))
	       '((1 10))))))
   (nil
      (progn
	 (emt:doc "Situation: Members are the same but repeated in one.")
	 (emt:doc "Response: Compares false")
	 (rtest:retvalue->status
	    (not
	       (emt:sets=
		  '((1 10 1 10))
		  '((1 10))))))))

;;;_ , emt:somewhere-in-tree
(emt:deftest-3 emt:somewhere-in-tree
   (nil
      (progn
	 (emt:doc "Situation: OBJ is in TREE.")
	 (emt:doc "Response: Return non-nil.")
	 (progn
	    (assert
	       (emt:somewhere-in-tree #'eq
		  '(((12)))
		  12))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: OBJ is not in TREE.")
	 (emt:doc "Response: Return nil.")
	 (progn
	    (assert
	       (not
		  (emt:somewhere-in-tree #'eq
		     '(((12)))
		     13)))
	    t))))

;;;_ , emt:collect-in-tree
(emt:deftest-3 emt:collect-in-tree
   (nil
      (progn
	 (emt:doc "Situation: The root of the tree matches PREDICATE.")
	 (emt:doc "Response: Return a list of just it.")
	 (equal
	    (emt:collect-in-tree #'integerp 12)
	    '(12))))
   (nil
      (progn
	 (emt:doc "Situation: Some items in tree match PREDICATE.")
	 (emt:doc "Response: Return a list of them.")
	 (equal
	    (emt:collect-in-tree #'integerp
	       '((a 12 b)
		   12))
	    '(12 12)))))


;;;_ , emt:let-noprops
(emt:deftest-3 emt:let-noprops
   (nil
      (progn
	 (emt:doc "Proves: `emt:let-noprops' temporarily rebinds properties.")
	 (emt:let-noprops
	    '(foo)
	    (assert
	       (null
		  (symbol-plist 'foo)))
	    (emt:let-noprops
	       '(foo)
	       (put 'foo 'example-prop 13))
	    (assert
	       (null
		  (symbol-plist 'foo)))
	    t)))
   (nil
      (progn
	 (emt:doc "Proves: `emt:let-noprops' temporarily sets properties null.")
	 (emt:let-noprops
	    '(foo)
	    (assert
	       (null
		  (symbol-plist 'foo)))
	    (put 'foo 'example-prop 13)
	    (assert
	       (equal
		  (symbol-plist 'foo)
		  '(example-prop 13)))
	    (emt:let-noprops
	       '(foo)
	       (assert
		  (null
		     (symbol-plist 'foo))))
	    (assert
	       (equal
		  (symbol-plist 'foo)
		  '(example-prop 13)))
	    t))))


;;;_ , emt:let-unbound
(emt:deftest-3 emt:let-unbound
   (nil
      (progn
	 (emt:doc "Param: literal list of symbol `foo'.")
	 (emt:doc "Response: `foo' is initially unbound in the body.
After `emt:let-unbound' runs, foo is bound again.")
	 (progn
	    (defconst foo t)
	    (assert
	       (boundp 'foo))
	    (emt:let-unbound
	       '(foo)
	       (assert
		  (not
		     (boundp 'foo))))
	    (assert
	       (boundp 'foo))
	    (makunbound 'foo)
	    t)))
   (nil
      (progn
	 (emt:doc "Param: symbolic list of symbol `foo'.")
	 (emt:doc "Response: `foo' is initially unbound in the body.
After `emt:let-unbound' runs, foo is bound again.")
	 (let
	    ((syms
		'(foo)))
	    (defconst foo t)
	    (assert
	       (boundp 'foo))
	    (emt:let-unbound syms
	       (assert
		  (not
		     (boundp 'foo))))
	    (assert
	       (boundp 'foo))
	    (makunbound 'foo)
	    t))))

;;;_ , emt:flet-unbound
(emt:deftest-3 emt:flet-unbound
   (nil
      (progn
	 (emt:doc "Param: literal list of symbol `foo'.")
	 (emt:doc "Response: `foo' is initially unbound in the body.
After `emt:let-unbound' runs, foo is bound again.")
	 (progn
	    (defun foo nil)
	    (assert
	       (fboundp 'foo))
	    (emt:flet-unbound
	       '(foo)
	       (assert
		  (not
		     (fboundp 'foo))))
	    (assert
	       (fboundp 'foo))
	    (fmakunbound 'foo)
	    t)))
   (nil
      (progn
	 (emt:doc "Param: symbolic list of symbol `foo'.")
	 (emt:doc "Response: `foo' is initially unbound in the body.
After `emt:let-unbound' runs, foo is bound again.")
	 (let
	    ((syms
		'(foo)))
	    (defun foo nil)
	    (assert
	       (fboundp 'foo))
	    (emt:flet-unbound syms
	       (assert
		  (not
		     (fboundp 'foo))))
	    (assert
	       (fboundp 'foo))
	    (fmakunbound 'foo)
	    t))))



;;;_ , emt:util:all-different
(emt:deftest-3 emt:util:all-different
   (nil
      (progn
	 (emt:doc "Proves: `emt:util:all-different' returns non-nil just if all
members of SET are different.")
	 (progn
	    (assert
	       (emt:util:all-different
		  '(1 2)))
	    (assert
	       (not
		  (emt:util:all-different
		     '(1 1))))
	    t))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/misc/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/misc/tests.el ends here
