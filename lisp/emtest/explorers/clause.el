;;;_ emtest/explorers/clause.el --- Clause explorer for Emtest

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

(require 'emtest/main/all-runners)
(require 'emtest/main/define)
(require 'emtest/main/find-tests)
(require 'emtest/support/keepup)

;;;_. Body
;;;_ , Launcher
;;;_  . emtest:sexp-at-point
;;;###autoload
(defun emtest:sexp-at-point (form)
   ""
   (interactive 
      (list 
	 (save-excursion (read (current-buffer)))))
   
   (emt:lch:run
      `(form ,form)
      emt:lch:proplist:vanilla
      (list "form")))
;;;_  . emt:xp:clause:eval
(defun emt:xp:clause:eval (expression)
   ""
   (emt:lch:run
      `(form (list nil expression))
      emt:lch:proplist:vanilla
      (list "expression")))

;;;_  . emtest:eval-last-sexp
;;;###autoload
(defun emtest:eval-last-sexp (arg)
   ""
   
   (interactive
      (list (preceding-sexp)))
   ;;Unlike eval-last-sexp, this does not try to print value in
   ;;minibuffer, nor in current buffer, nor optionally trigger the
   ;;debugger.
   (emt:xp:clause:eval arg))

;;;_  . emtest:eval-expression
;;;###autoload
(defun emtest:eval-expression (arg &optional insert-value)
   ""
   ;;Interactive form borrowed from "simple.el"
   (interactive
      (list (let ((minibuffer-completing-symbol t))
	       (read-from-minibuffer "Eval: "
		  nil read-expression-map t
		  'read-expression-history))
	 current-prefix-arg))
   ;;Unlike eval-expression, this does not do the extra stuff
   (emt:xp:clause:eval arg))
;;;_ , Helper
;;;_  . emt:xp:clause:run-p
(defun emt:xp:clause:run-p (test-id props)
   "Non-nil if TEST-ID should be explored"
   (prog1
      (if
	 (utim:get-properties 'no-redo-passes props)
	 (or
	    (not
	       (emt:ind:get-prop test-id 'has-run))
	    (emt:ind:get-prop test-id 'grade-says-rerun)
	    (emt:ind:get-prop test-id 'user-says-rerun))
	 t)
      ;;$$HACK  Doing this properly would require rewriting to get
      ;;this info from receive alist.  Then we'd check whether test-id
      ;;was present instead of setting a property.
      (emt:ind:set-prop test-id 'has-run t)))


;;;_ , Explorers
;;;_  . emt:xp:clause

(defun emt:xp:clause (clause props report-f)
   "Explore one clause in Emtest.
This is the heart of Emtest exploration: A test itself."
   (funcall 
      (emt:runner:get-func (emt:def:clause->governor clause))
      props 
      (emt:def:clause->form clause)
      report-f))

;;;_  . emt:xp:clause:literal
;;;###autoload
(defun emt:xp:clause:literal (test-id props path report-f)
   "Explore a literal clause in Emtest."
   ;; If no clause is given. we can do little.
   (if (emt:xp:clause:run-p test-id props)
      (emt:xp:clause
	 (second test-id)
	 props
	 report-f)
      (funcall report-f nil nil nil 1)))

;;;_   , Register
;;;###autoload (eval-after-load 'emtest/main/all-explorers
;;;###autoload  '(emt:exps:add 'form #'emt:xp:clause:literal
;;;###autoload  "Literal clause"))
;;;_  . emt:xp:clause:indexed
;;;###autoload
(defun emt:xp:clause:indexed (test-id props path report-f)
   "Explore an indexed clause in a suite in Emtest."
   ;; If no index is given, we could possibly list them but since they
   ;; have no names it'd accomplish little.
   (if (emt:xp:clause:run-p test-id props)
      (destructuring-bind (suite-sym clause-index) (cdr test-id)
	 (emt:def:update-for-sym suite-sym)
	 (emt:def:destructure-suite-3 suite-sym
	    (emt:xp:clause 
	       (nth clause-index clause-list)
	       props
	       report-f)))
      (funcall report-f nil nil nil 1)))


;;;_   , Register
;;;###autoload (eval-after-load 'emtest/main/all-explorers
;;;###autoload  '(emt:exps:add 'indexed-clause #'emt:xp:clause:indexed
;;;###autoload  "Indexed clause"))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/explorers/clause)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/clause.el ends here
