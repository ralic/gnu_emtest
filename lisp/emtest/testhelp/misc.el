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

;;;_ , form-by-option

;;$$RENAME ME.
;;$$MOVE ME - into utilities
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




;;;_ , emt:expand-filename-by-load-file

;;;###autoload
(defun emt:expand-filename-by-load-file (filename)
   ""
   (expand-file-name filename
      (if load-file-name
	 (file-name-directory (file-truename load-file-name)))))

;;;_ , emt:gives-error

;;This does not support knowing the error data, just the error symbol.
;;If you want to check that, use `condition-case' directly.

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


;;;_ , emt:util:all-different
;;;###autoload
(defun emt:util:all-different (set &optional test)
   "Return non-nil just if all members of SET are different."
   
   (let
      ((w/o-dups (remove-duplicates set)))
      (equal (length set) (length w/o-dups))))

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
