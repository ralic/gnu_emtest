;;;_ emtest/runner/explorers/all.el --- Explorer-collection functionality

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint, lisp

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

;;$$IMPROVE ME later  When we have truly separated autoloads, require
;;the file these explorers' autoloads are defined in, and change their
;;autoload forms to no longer require this (else we'll be circular)

;;;_. Body
;;;_ , Registering explorers
;;;_  . List of explorers
(defvar emtt:test-finder:method-list 
   '()
   "List of explorer methods.
Files that define explorers should call `emtt:add-explorer' to add
their methods.

Format: Each entry is (PREDICATE FUNCTION NAME), where 
 * PREDICATE is a predicate to tell whether 
 * FUNCTION explores the test or suite.

THIS FORMAT MAY CHANGE." )

;;;_  . emtt:add-explorer

(defun emtt:add-explorer (pred func &optional name &rest dummy)
   ""
   
   (unless (assq pred emtt:test-finder:method-list)
      (push 
	 (list pred func name)
	 emtt:test-finder:method-list)))

;;;_  . emtt:match-explorer
(defsubst emtt:match-explorer (how method)
   "Return non-nil if METHOD is right for HOW"
   (funcall (car method) how))

;;;_  . emtt:get-explore-func 
(defun emtt:get-explore-func (how)
   "Get a relevant function for HOW.
Should not fail.
HOW must be of a subtype of emthow"
   
   (catch 'emtt:explore-func
      (progn
	 (dolist (method emtt:test-finder:method-list)
	    (when (emtt:match-explorer how method)
	       (throw 'emtt:explore-func (second method))))
	 #'emtt:explore-fallback)))

;;;_  . Special explorers
;;;_   , emtt:explore-hello
;;This doesn't require an autoload but all others do.
(defun emtt:explore-hello (test-id props path report-f)
   "Report about Emtest, listing the explore methods."
   
   (funcall report-f
      (emt:testral:make-test-runner-info
	 :name "Emtest"
	 :version emtt:version
	 :explore-methods-supported
	 (mapcar #'third emtt:test-finder:method-list))))

;;;_    . Insinuate

(emtt:add-explorer #'emthow:hello-p #'emtt:explore-hello
   "Tester signature") 

;;;_   , emtt:explore-fallback
;;Not part of the list of methods.
(defun emtt:explore-fallback (test-id props path report-f)
   "Report that no matching explore method could be found."

   (funcall local-report-f
      (emt:testral:make-suite
	 :contents 
	 (emt:testral:make-note-list
	    :notes 
	    (list
	       (emt:testral:make-error-raised
		  :err 
		  '(error 
		      "Unrecognized internal explore type")
		  :badnesses 
		  '((ungraded 'error 
		       "Unrecognized internal explore type")))))
	 ;;Actual form is TBD.
	 :badnesses 
	 '((ungraded 'error 
	      "Unrecognized internal explore type"))
	 ;;Punt info for now.
	 :info '())))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/explorers/all)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/explorers/all.el ends here
