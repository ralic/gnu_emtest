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
;;;_  . emt:sexp-at-point
;;;###autoload
(defun emt:sexp-at-point (form)
   ""
   (interactive 
      (list 
	 (save-excursion (read (current-buffer)))))
   
   (emt:lch:run
      `(form ,form)
      emt:lch:proplist:vanilla
      (list "form")))
;;;_  . emtt:eval
(defun emtt:eval (expression)
   ""
   (emt:lch:run
      `(form (list nil expression))
      emt:lch:proplist:vanilla
      (list "expression")))

;;;_  . emt:eval-last-sexp
;;;###autoload
(defun emt:eval-last-sexp (arg)
   ""
   
   (interactive
      (list (preceding-sexp)))
   ;;Unlike eval-last-sexp, this does not try to print value in
   ;;minibuffer, nor in current buffer, nor optionally trigger the
   ;;debugger.
   (emtt:eval arg))

;;;_  . emt:eval-expression
;;;###autoload
(defun emt:eval-expression (arg &optional insert-value)
   ""
   ;;Interactive form borrowed from "simple.el"
   (interactive
      (list (let ((minibuffer-completing-symbol t))
	       (read-from-minibuffer "Eval: "
		  nil read-expression-map t
		  'read-expression-history))
	 current-prefix-arg))
   ;;Unlike eval-expression, this does not do the extra stuff
   (emtt:eval arg))
;;;_ , Helper
;;;_  . emtt:explore-clause-p
(defun emtt:explore-clause-p (test-id props)
   "Non-nil if TEST-ID should be explored"
   (if
      (utim:get-properties 'no-redo-passes props)
      ;;$$IMPROVE ME Directly use passedness
      (>= (emt:ind:get-score test-id) 0)
      t))

;;;_ , Explorers
;;;_  . emtt:explore-clause

(defun emtt:explore-clause (clause props report-f)
   "Explore one clause in Emtest.
This is the heart of Emtest exploration: A test itself."
   (funcall 
      (emt:runner:get-func (emtd:clause->governor clause))
      props 
      (emtd:clause->form clause)
      report-f))

;;;_  . emtt:explore-literal-clause
;;;###autoload
(defun emtt:explore-literal-clause (test-id props path report-f)
   "Explore a literal clause in Emtest."
   (if (emtt:explore-clause-p test-id props)
      (emtt:explore-clause
	 (second test-id)
	 props
	 report-f)
      (funcall report-f nil nil nil 1)))

;;;_   , Register
;;;###autoload (eval-after-load 'emtest/main/all-explorers
;;;###autoload  '(emt:exps:add 'form #'emtt:explore-literal-clause
;;;###autoload  "Literal clause" 0))
;;;_  . emtt:explore-indexed-clause
;;;###autoload
(defun emtt:explore-indexed-clause (test-id props path report-f)
   "Explore an indexed clause in a suite in Emtest."
   (if (emtt:explore-clause-p test-id props)
      (destructuring-bind (suite-sym clause-index) (cdr test-id)
	 (emtd:update-for-sym suite-sym)
	 (emtd:destructure-suite-3 suite-sym
	    (emtt:explore-clause 
	       (nth clause-index clause-list)
	       props
	       report-f)))
      (funcall report-f nil nil nil 1)))


;;;_   , Register
;;;###autoload (eval-after-load 'emtest/main/all-explorers
;;;###autoload  '(emt:exps:add 'indexed-clause #'emtt:explore-indexed-clause
;;;###autoload  "Indexed clause" 0))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/explorers/clause)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/clause.el ends here
