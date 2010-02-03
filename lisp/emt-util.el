;;;_ emt-util.el --- Utilities for defining tests with emtest

;; Copyright (C) 2007  Dis

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

;;; Commentary:

;; Test helpers of general usefulness

;;;_. Code:

;;;_ , form-by-option

;;$$Rename me.
;;;###autoload
(defmacro form-by-option (options key make-form &optional make-form-else)
   "Return a form that interprets KEY in OPTIONS.
MAKE-FORM and MAKE-FORM-ELSE should be functions of one variable that
return a form."

   `(let
      ((data (assoc ,key ,options)))
       ;;Only at eval-time do we know whether `key' is found in
       ;;`options'.
      (if data
	 ;;At expansion-time we know whether `make-form' and
	 ;;`make-form-else' are given.
	 ,(if make-form `(funcall ,make-form data))
	 ,(if make-form-else `(funcall ,make-form-else data)))))



;;;_  . Tests

(rtest:deftest form-by-option

   ("Expands form according to data in options"
      (rtest:retvalue->status
	 (equal
	    (form-by-option
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
	    (form-by-option nil 'x nil
	       #'(lambda
		    (x)
		    '(foo)))
	    '(foo)))))



;;;_ , emt:expand-filename-by-load-file

;;;###autoload
(defun emt:expand-filename-by-load-file (filename)
   ""
   (expand-file-name filename
      (if load-file-name
	 (file-name-directory load-file-name))))

;;;_ , emt:gives-error

;;If you want to know what error, use `condition-case' directly.

(defmacro emt:gives-error (form &optional error-sym)
   ""
   (let
      ((error-sym (or error-sym 'error)))
      `(condition-case nil
	  (progn
	     ,form
	     nil)
	  (',error-sym t)
	  ;;We don't catchall with (error nil), because unexpected
	  ;;errors should escape here.
	  )))

;;;_ , emt:example-error

(put 'emt:example-error 'error-conditions
   '(error emt:example-error))
(put 'emt:example-error 'error-message
   "This is an example error, probably thrown on purpose")

;;;_ , emt:throws-x

(defun emt:throws-x (tags inner-body)
   "Make nested catches for TAGS, with INNER-BODY as the inner form"
   
   (reduce
      #'(lambda (form tag)
	   `(catch ',tag ,form))
      (if (listp tags) tags (list tags))
      :initial-value inner-body))
;;;_  . Tests
(put 'emt:throws-x 'emt:test-thru
   'emt:throws)
;;;_ , Test helper `emt:throws'

(defmacro emt:throws (tags &rest body)
   ""
   
   `(catch 'emt:throws:ok
       (progn
	  (eval
	     (emt:throws-x
		,tags
		'(progn
		   ,@body
		   ;;If we fall thru to here, tag was not thrown by BODY.
		   (throw 'emt:throws:ok nil))))
	  ;;If we reach here, we didn't throw to `emt:throws:ok'.
	  t)))


;;;_  . Tests
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
(defmacro emt:assert-throws (tags &rest body)
   ""
   
   `(eval
       (emt:throws-x
	  ,tags
	  '(progn
	      ,@body
	      ;;If we fall thru to here, tag was not thrown by BODY.
	      (error "Tag was not thrown")))))

;;;_  . Tests

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
;;;_ , emt:bags=

(defsubst emt:bags= (set1 set2 &rest flags)
   "Like `emt:sets=' but consider duplicate elements."
   ;;This works because `subsetp' accepts improper subsets.
   (let
      ((flags 
	  (if (memq :test flags) 
	     flags 
	     (list* :test #'equal flags))))
      (and
	 (= (length set1) (length set2))
	 (apply #'subsetp set1 set2 flags)
	 (apply #'subsetp set2 set1 flags))))

(put 'emt:sets= 'emt:equal-test t)

;;;_ , emt:sets=

(defsubst emt:sets= (set1 set2 &rest flags)
   ""

   ;;Propagate `:test' etc to `remove-duplicates'
   (apply #'emt:bags=
      (apply #'remove-duplicates set1 flags)
      (apply #'remove-duplicates set2 flags)
      flags))



(put 'emt:sets= 'emt:equal-test t)

;;;_  . Tests

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

;;;###autoload
(defun emt:somewhere-in-tree (func tree &rest args)
   ""
   ;;Trick to detect a thing in a tree: Use subst, but the `:test'
   ;;function if successful throws and does not return.
   (catch 'found
      (progn
	 (subst nil nil tree 
	    :test
	    #'(lambda (dummy a)
		 (when
		    (apply func a args)
		    ;;If we found it, give `t'.
		    (throw 'found t))))
	 ;;If we fell thru, we never found it, so give `nil'
	 nil)))

;;;_  . Tests
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
;;;_ , emt/util/collect-in-tree
(defun emt/util/collect-in-tree (predicate tree)
   "Return a list of the items in TREE satisfying PREDICATE.
Do not return matching items that are within other matching items."
   (if (funcall predicate tree)
      (list tree)
      (cond
	 ((listp tree)
	    (apply
	       #'append
	       (mapcar
		  #'(lambda (el)
		       (emt/util/collect-in-tree predicate el))
		  tree)))
	 (t ()))))

;;;_  . Tests
(rtest:deftest emt/util/collect-in-tree

   (  "Situation: The root of the tree matches PREDICATE.
Response: Return a list of just it."
      (equal
	 (emt/util/collect-in-tree #'integerp 12)
	 '(12)))
   
      (  "Situation: Some items in tree match PREDICATE.
Response: Return a list of them."
      (equal
	 (emt/util/collect-in-tree #'integerp '((a 12 b) 12))
	 '(12 12)))
   
   )


;;;_ , let-noprops
;;;###autoload
(defmacro let-noprops (syms-form &rest body)
   "Run BODY with symbols temporarily stripped of its properties.
When done, restore each symbol's original list of properties.
SYMS-FORM is a form to make a list of symbols."
   (let
      ((syms (gensym)))
      `(let*
	  (  ;;Get the symbols now, so we don't call SYMS-FORM more
	     ;;than once. 
	     (,syms ,syms-form)
	     ;;Remember their old properties
	     (old-props (mapcar #'symbol-plist ,syms)))
	  (unwind-protect
	     (progn
		;;Set each symbol's property list empty
		(dolist (sym ,syms) (setplist sym ()))
		,@body)
	     ;;Restore each symbol's old property list (Uses cl)
	     (map nil #'setplist ,syms old-props)))))

;;;_  . Tests
(rtest:deftest let-noprops

   (  "Proves: `let-noprops' temporarily rebinds properties."
      (let-noprops '(foo)
	 ;;Now `foo' has no properties
	 (assert (null (symbol-plist 'foo)))
	 ;;Inside a nested let-noprops, give `foo' a property
	 (let-noprops '(foo)
	    (put 'foo 'example-prop 13))

	 ;;Outside, `foo' once again has no properties
	 (assert (null (symbol-plist 'foo)))
	 t))
   
   (  "Proves: `let-noprops' temporarily sets properties null."
      (let-noprops '(foo)
	 ;;Now `foo' has no properties
	 (assert (null (symbol-plist 'foo)))
	 ;;Give `foo' a property
	 (put 'foo 'example-prop 13)
	 (assert (equal (symbol-plist 'foo) '(example-prop 13)))

	 (let-noprops '(foo)
	    ;;Inside a nested let-noprops, `foo' once again has no
	    ;;properties
	    (assert (null (symbol-plist 'foo))))
	 
	 ;;Outside, `foo' once again has the properties we gave it.
	 (assert (equal (symbol-plist 'foo) '(example-prop 13)))
	 t))
   
   )

;;;_ , let-unbound
(defmacro let-unbound (syms-form &rest body)
   ""
   (let
      ((syms (eval syms-form)))
      `(let
	  ,syms
	  (mapcar
	     #'makunbound
	     ',syms))))



;;;_  . Tests
(rtest:deftest let-unbound
   ;;Bootstrap problem: To test this, we'd really like to have itself,
   ;;to insulate the test from the environment.

   (  "Param: literal list of symbol `foo'.
Response: `foo' is initially unbound in the body.
After `let-unbound' runs, foo is bound again."
      (progn
	 (defconst foo t)
	 ;;Validation
	 (assert (boundp 'foo))

	 (let-unbound '(foo)
	    (assert (not (boundp 'foo))))
	 (assert (boundp 'foo))

	 ;;Clean up after ourselves
	 (makunbound 'foo) 
	 t))
   
   (  "Param: symbolic list of symbol `foo'.
Response: `foo' is initially unbound in the body.
After `let-unbound' runs, foo is bound again."
      (let
	 ((syms '(foo)))
	 (defconst foo t)
	 ;;Validation
	 (assert (boundp 'foo))

	 (let-unbound syms
	    (assert (not (boundp 'foo))))
	 (assert (boundp 'foo))

	 ;;Clean up after ourselves
	 (makunbound 'foo) 
	 t))
   
   ;;Not tested: Cleanliness: Doesn't capture `syms' etc.
   )
;;;_ , flet-unbound
(defmacro flet-unbound (syms-form &rest body)
   ""
   (let
      ((syms (eval syms-form)))
      `(progn
	  (require 'cl)
	  (flet
	     ,(mapcar 
		 #'(lambda (sym) `(,sym ()))
		 syms)
	     (mapcar
		#'fmakunbound
		',syms)))))

;;;_  . Tests
(rtest:deftest flet-unbound
   ;;Bootstrap problem: To test this, we'd really like to have itself,
   ;;to insulate the test from the environment.

   (  "Param: literal list of symbol `foo'.
Response: `foo' is initially unbound in the body.
After `let-unbound' runs, foo is bound again."
      (progn
	 (defun foo ())
	 ;;Validation
	 (assert (fboundp 'foo))

	 (flet-unbound '(foo)
	    (assert (not (fboundp 'foo))))
	 (assert (fboundp 'foo))

	 ;;Clean up after ourselves
	 (fmakunbound 'foo) 
	 t))
   
   (  "Param: symbolic list of symbol `foo'.
Response: `foo' is initially unbound in the body.
After `let-unbound' runs, foo is bound again."
      (let
	 ((syms '(foo)))
	 (defun foo ())
	 ;;Validation
	 (assert (fboundp 'foo))

	 (flet-unbound syms
	    (assert (not (fboundp 'foo))))
	 (assert (fboundp 'foo))

	 ;;Clean up after ourselves
	 (fmakunbound 'foo) 
	 t))
   
   ;;Not tested: Cleanliness: Doesn't capture `syms' etc.
   )



;;;_ , emt:util:all-different
;;;###autoload
(defun emt:util:all-different (set &optional test)
   "Return non-nil just if all members of SET are different."
   
   (let
      ((w/o-dups (remove-duplicates set)))
      (equal (length set) (length w/o-dups))))

;;;_  . Tests
(rtest:deftest emt:util:all-different

   (  "Proves: `emt:util:all-different' returns non-nil just if all
members of SET are different."
      (progn
	 (assert
	    (emt:util:all-different '(1 2)))
	 (assert
	    (not
	       (emt:util:all-different '(1 1))))
	 t))
   
   
   )

;;;_: Footers
;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + mode: allout
;;;_  + outline-regexp: ";;;_ *"
;;;_  + End:

;;;_ , Provide

(provide 'emt-util)
;;; emt-util.el ends here
