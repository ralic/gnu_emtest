;;;_ emtest/testhelp/match/testhelp.el --- Testhelp for match

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

(require 'emtest/testhelp/match)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/structs)

;;;_. Body

;;;_ , Test examples
(defconst emtm:govs:thd:examples
   (emtg:define+ ;;xmp:3be9cd9e-25f7-4f9d-9304-8afc1680d17a
      ((project emtest)
	 (library emtm)
	 (section emtm:govs:list))

      ;;Structure matches & mismatches together?  Not until tester use of
      ;;docs and iterating-over is stronger.  We'd want to iterate over
      ;;combinations of 2 things: pattern-situation and subordinately,
      ;;object-situation. 
      (group ((situation one-item-t))
	 (doc ()
	    "One item in the list.  It matches.")
	 (item ((type pattern)(subtype list))
	    '(list 12))
	 (item ((type pattern)(subtype set))
	    '(set 12))
	 (item ((type object))
	    '(12))
	 (item ((type matches-p)) 
	    t))

      (group ((situation one-item-f))
	 (doc ()
	    "One item in the list.  It mismatches.")
	 (item ((type pattern)(subtype list))
	    '(list 12))
	 (item ((type pattern)(subtype set))
	    '(set 12))
	 (item ((type object))
	    '(13))
	 (item ((type matches-p)) 
	    nil))
      ;;Similarly for the other test data.

   
      ))

;;;_  . emtm:ts:single-gov
(defun emtm:ts:single-gov (func pattern obj)

   "Match OBJ vs the pattern PATTERN, forcing FUNC to be the governor
function when parsing PATTERN.  PATTERN should be quoted and not
contain the symbol `testhelp-483s'.

Intended for testing governor functions in isolation."

   ;;$$IMPROVE ME: Use a make-symbol instead of testhelp-483s
   ;;For this, punt: Always use `testhelp-483s' as the symbol.
   (let
      ((testhelp-483s obj))
      (emtt:testral:with-prestn-path ()
	 (eval
	    (emtm:build-form 
	       '(testhelp-483s) 
	       (funcall func 'testhelp-483s pattern) 
	       't)))))

;;;_  . Literal version, to validate against
(defun emths:struct:gov-literal (sym pattern &optional other-deps prestn-prefix)
   ""

   (destructuring-bind (&key (my-field () sv-my-field))
      (cdr pattern)

      (let* 
	 (
	    ;;Object must be of type emths:struct.
	    (check-type
	       (emtm:make-typecheck-form
		  sym #'emths:struct-p ()))

	    (my-field-sym (gensym))
	    ;;Get the my-field accessor `emths:struct->my-field'
	    ;;Make a binding that accesses field
	    (bind-field-my-field
	       (emtm:make-binding-form-data
		  :uses 
		  (emtm:parse-dependencies
		     (list sym) (list check-type))
		  :bind my-field-sym
		  :form `(emths:struct->my-field ,sym)))
	    ;;Match the given `my-field' pattern to that object.
	    (child-formdata
	       (emtm:parse-pattern
		  my-field-sym
		  my-field)))
	 
	 (utiacc:list->object
	    (append
	       (list 
		  (emtm:make-formdata
		     :form-LIST 
		     (list check-type)))
	       
	       (if sv-my-field
		  (list
		     (emtm:make-formdata
			:form-LIST 
			(list bind-field-my-field))
		     child-formdata)
		  ()))
	    'emtm:formdata))))

;;And make it a governor.
(put 'emths:struct:literal-gov 'emtm:makepattern
   #'emths:struct:gov-literal)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/match/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/match/testhelp.el ends here
