;;;_ emtest/testhelp/misc.el --- Utilities for defining tests with emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2007  Tom Breton (Tehom)

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

;; Test helpers of general usefulness
;;;_ , Requires

;;;_. Code:


;;;_ , emth:gives-error
(defmacro emth:gives-error (form &optional error-sym)
   "Non-nil just if FORM gives an error.
If ERROR-SYM is given, the error must be of that type.

This does not support knowing the error data, just the error symbol.
If you want to check that, use `condition-case' directly."
   (let
      ((error-sym (or error-sym 'error)))
      `(condition-case nil
	  (progn
	     ,form
	     nil)
	  ;;We don't catchall with (error nil), because unexpected
	  ;;errors should escape here.
	  (',error-sym t))))

;;;_ , emth:example-error
(put 'emth:example-error 'error-conditions
   '(error emth:example-error))
(put 'emth:example-error 'error-message
   "This is an example error, probably thrown on purpose")

;;;_ , emth:throws-x

(defun emth:throws-x (tags inner-body)
   "Make nested catches for TAGS, with INNER-BODY as the inner form"
   
   (reduce
      #'(lambda (form tag)
	   `(catch ',tag ,form))
      (if (listp tags) tags (list tags))
      :initial-value inner-body))
;;;_ , Test helper `emth:throws'
;;;###autoload
(defmacro emth:throws (tags &rest body)
   "Non-nil just if BODY throws one of TAGS.
TAGS must be a list of symbols."
   
   `(catch 'emth:throws:ok
       (progn
	  (eval
	     (emth:throws-x
		,tags
		'(progn
		   ,@body
		   ;;If we fall thru to here, tag was not thrown by BODY.
		   (throw 'emth:throws:ok nil))))
	  ;;If we reach here, we didn't throw to `emth:throws:ok'.
	  t)))



;;;_ , emth:assert-throws
;;;###autoload
(defmacro emth:assert-throws (tags &rest body)
   ""
   
   `(eval
       (emth:throws-x
	  ,tags
	  '(progn
	      ,@body
	      ;;If we fall thru to here, tag was not thrown by BODY.
	      (error "Tag was not thrown")))))

;;;_ , emth:bags=
;;;###autoload
(defsubst emth:bags= (set1 set2 &rest flags)
   "Like `emth:sets=' but consider duplicate elements."
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

(put 'emth:sets= 'emt:equal-test t)

;;;_ , emth:sets=
;;;###autoload
(defsubst emth:sets= (set1 set2 &rest flags)
   ""

   ;;Propagate `:test' etc to `remove-duplicates'
   (apply #'emth:bags=
      (apply #'remove-duplicates set1 flags)
      (apply #'remove-duplicates set2 flags)
      flags))



(put 'emth:sets= 'emt:equal-test t)

;;;_ , emth:all-different
;;;###autoload
(defun emth:all-different (set &optional test)
   "Return non-nil just if all members of SET are different."
   
   (let
      ((w/o-dups (remove-duplicates set)))
      (equal (length set) (length w/o-dups))))

;;;_ , emth:somewhere-in-tree

;;;###autoload
(defun emth:somewhere-in-tree (func tree &rest args)
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

;;;_ , emth:collect-in-tree
;;;###autoload
(defun emth:collect-in-tree (predicate tree)
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
		       (emth:collect-in-tree predicate el))
		  tree)))
	 (t ()))))


;;;_ , emth:let-noprops
;;;###autoload
(defmacro emth:let-noprops (syms-form &rest body)
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

;;;_ , emth:let-unbound
;;;###autoload
(defmacro emth:let-unbound (syms-form &rest body)
   ""
   (let
      ((syms (eval syms-form)))
      `(let
	  ,syms
	  (mapcar
	     #'makunbound
	     ',syms))))



;;;_ , emth:flet-unbound
;;;###autoload
(defmacro emth:flet-unbound (syms-form &rest body)
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


;;;_: Footers
;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + mode: allout
;;;_  + outline-regexp: ";;;_ *"
;;;_  + End:

;;;_ , Provide

(provide 'emtest/testhelp/misc)
;;; emtest/testhelp/misc.el ends here
