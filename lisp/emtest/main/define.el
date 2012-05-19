;;;_ emtest/main/define.el --- Test definer for Emtest 

;;;_. Headers
;;;_ , License

;; Copyright (C) 2007,2009

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

;;This file contains the entry point for defining emtest tests.

;;There are two ways to require this file.  The first is for files
;;that contain only test code.  For those, just write:

;;  (require 'emtest/main/define)

;;The second is for files that include both main code and tests.  For
;;these, command `emtest:insert' and choose `emt:ed:insert:requires'.  It
;;will insert something like:

;; (unless (fboundp 'emt:deftest)
;;    (defmacro emt:deftest (&rest dummy))
;;    (defmacro emt:if-avail (&rest dummy)))

;;The code above means: If `emt:deftest' is not bound (not even as an
;;autoload), define dummies for the entry points.

;;Then put everything emtest-specific except deftests inside
;;`emt:if-avail'.  This lets the file be loaded even by users who
;;don't have emtest.

;;;_ , Requires

;;$$IMPROVE ME.  We should instead pull testhelp's autoload file(s)
;;in.  Which have yet to be created.
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/persist)

;;;_. Body
;;;_ , emt:if-avail
;;;###autoload
(defmacro emt:if-avail (&rest body)
   "Equivalent to `progn' just if emtest is available."
   (cons 'progn body))

;;;_ , emt:def:make-prop-eval-form
(defun emt:def:make-prop-eval-form (spec)
   ""
   
   (destructuring-bind (sym val-form) spec
      `(list ',sym ,val-form)))

;;;_ , Version 3
;;;_  . emt:deftest
(defalias 'emt:deftest 'emt:deftest-3)
;;;_  . emt:deftest-3
;;;###autoload
(defmacro emt:deftest-3 (name-or-props &rest clauses)
   "Define a test in Emtest"
   (let*
      ((props 
	  ;;In PROPS, the second element of each item is a form, which
	  ;;later will be evaluated.
	  (cond
	     ;;If PROPS is a bare symbol, it's treated as the name.
	     ((symbolp name-or-props)
		`((of ',name-or-props)))
	     ((atom name-or-props)
		(error 
		   "Deftest first arg must be a symbol or a key-value list"))
	     (t
		name-or-props)))
	 (of-what
	    (second (assq 'of props))))
      (unless of-what
	 (error "Deftest needs an `of' property"))
      `(progn
	  (put ,of-what 'emt:properties
	     (list
		;;Get the name of the file the test lives in.  This
		;;can be `nil', eg if user used `eval-region'.
		'(load-file-name ,(or load-file-name buffer-file-name))
		,@(if props
		     (mapcar
			#'emt:def:make-prop-eval-form
			props)
		     ())))
	  
	  (put ,of-what 'emt:suite ',clauses))))

;;;_  . emt:def:destructure-suite-3

(defmacro emt:def:destructure-suite-3 (suite &rest body)
   "Run BODY with the contents of SUITE bound as indicated.
SUITE must be a symbol naming a test suite.

 * clause-list - List of the clauses in SUITE.
 * props - The property list of SUITE (an alist)."
   (declare (debug (sexp body)))
   `(let
       (
	  (clause-list
	     (get ,suite 'emt:suite))
	  (props
	     (get ,suite 'emt:properties)))
       
       ,@body))

;;;_  . emt:def:clause->governor
(defalias 'emt:def:clause->governor 'car)
;;;_  . emt:def:clause->form
(defalias 'emt:def:clause->form     'cdr)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/define)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:
;;;_ , End
;;; emtest/main/define.el ends here
