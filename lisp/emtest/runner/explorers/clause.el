;;;_ emtest/runner/explorers/clause.el --- Clause explorer for Emtest

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

(require 'emtest/common/result-types)
(require 'emtest/common/testral-types)
(require 'emtest/runner/surrounders)
(require 'emtest/runner/define)
(require 'emtest/runner/testral)
(require 'emtest/runner/keepup)
(require 'emtest/runner/runners/external)

;;;_. Body
;;;_ , Structures
;;Maybe define `emthow:form' here, since it need not be
;;exposed. 
;;;_ , Runners (emtr prefix)
;;;_  . nil runner emtr:quoted
(defun emtr:quoted (props form report-f)
   "Report a dormant result.  For quoted test-cases."

   (funcall report-f
      (emt:testral:make-suite
	 :contents '()
	 :badnesses '(dormant)
	 :info '())))


;;;_  . emtr:vanilla
(defun emtr:vanilla (props form report-f)
   "Run a vanilla test-case and report the result."
   (emtt:testral:with
      (let
	 ( 
	    ;;$$IMPROVE ME  Push more of this out to `emth:abortscope'
	    ;;These badnesses are only for problems that manifest right
	    ;;here, not lower down. 
	    ;;$$RETHINK ME: Instead, be signalled to abort (that's
	    ;;compatible with `emth:trap-errors' and if we see
	    ;;emtt:*abort-p*, set that badness)
	    (badnesses '())	 
	    (emt:trace:properties props) ;;OBSOLESCENT.
	    (form-1
	       (emts:add-surrounders 
		  form 
		  (emtts:get-surrounders props)
		  props)))
	 (emth:abortscope
	    ;;$$USE STANDARD
	    ;;(emth:trap-errors (eval form-1))
	    (condition-case err
	       (eval form-1)
	       (error
		  (emtt:testral:add-note
		     (emt:testral:make-error-raised
			:err err
			:badnesses '(ungraded)))
		  (push
		     'ungraded
		     badnesses))))

	 (funcall report-f
	    (emt:testral:make-suite
	       :contents
	       (emtt:testral:note-list)
	       ;;Need to acquire this.  At least errors that we
	       ;;handle here - which may be just overall abort.
	       ;;See the call to `emth:trap-errors'
	       :badnesses badnesses
	       ;;$$WRITEME Use `emt:trace:properties' for this?  But change
	       ;;its name?  (And watch the scoping)
	       :info '())))))
;;;_ , Functions
;;;_  . emtt:explore-clause

(defun emtt:explore-clause (clause props report-f)
   "Explore one clause in Emtest.
This is the heart of Emtest exploration: A test itself."
   (emtd:destructure-clause-3 clause
      (case governor
	 (quote (emtr:quoted props form report-f))
	 ((nil) (emtr:vanilla props form report-f))
	 (external (emtr:external props form report-f))
	 (t
	    (funcall report-f
	       (emt:testral:make-suite
		  :contents '()
		  :badnesses '(ungraded)
		  :info '()))))))


;;;_  . emtt:explore-literal-clause
;;;###autoload
(defun emtt:explore-literal-clause (test-id props path report-f)
   "Explore a literal clause in Emtest."
   (emtt:explore-clause
      (emthow:form->test-form test-id)
      props
      report-f))

;;;_   , Insinuate
;;;###autoload (require 'emtest/runner/explorers/all)
;;;###autoload (emtt:add-explorer #'emthow:form-p #'emtt:explore-literal-clause
;;;###autoload "Literal clause") 
;;;_  . emtt:explore-indexed-clause
;;;###autoload
(defun emtt:explore-indexed-clause (test-id props path report-f)
   "Explore an indexed clause in a suite in Emtest."
   (let*
      (
	 (suite-sym 
	    (emthow:indexed-clause->suite-sym
	       test-id))
	 (index
	    (emthow:indexed-clause->clause-index
	       test-id)))
      (emtd:update-for-sym suite-sym)
      (emtd:destructure-suite-3 suite-sym
	 (emtt:explore-clause 
	    (nth index clause-list)
	    props
	    report-f))))


;;;_   , Insinuate
;;;###autoload (emtt:add-explorer #'emthow:indexed-clause-p #'emtt:explore-indexed-clause
;;;###autoload "Indexed clause") 

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/explorers/clause)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/explorers/clause.el ends here
