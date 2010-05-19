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

(require 'emtest/common/testral-types)
(require 'emtest/runner/surrounders)
(require 'emtest/runner/define)
(require 'emtest/runner/testral)


;;;_. Body
;;;_ , Structures
;;Maybe define `emt:test-ID:e-n:form' here?
;;;_ , Functions
;;;_  . emtt:explore-clause

(defun emtt:explore-clause (clause)
   "Explore one clause in Emtest.
This is the heart of Emtest exploration: A test itself."
   (let
      (
	 (emt:testral:*parent-id* 0)
	 ;;Counter to make unique IDs.  Although UUIDs are appealing,
	 ;;they are slower to make.
	 (emt:testral:*id-counter* 1)
	 (emt:testral:*events-seen* (emt:testral:create))
	 (emtt:*abort-p* nil)
	 ;;These badnesses are only for problems that manifest right
	 ;;here, not lower down. 
	 ;;$$RETHINK ME: Instead, be signalled to abort (that's
	 ;;compatible with `emtt:trap-errors' and if we see
	 ;;emtt:*abort-p*, set that badness)
	 (badnesses '()))

      ;;This defines `props'
      (emtt:destructure-clause-3 clause
	 (let
	    (
	       (emt:trace:properties props) ;;OBSOLESCENT.
	       (form-1
		  (emtt:add-surrounders 
		     form 
		     (emtts:get-surrounders props)
		     props)))
	    ;;$$USE STANDARD
	    ;;(emtt:trap-errors (eval form-1))
	    (condition-case err
	       (eval form-1)
	       (error
		  (push
		     (make-emt:testral:error-raised
			:err err
			:badnesses '(ungraded))
		     emt:testral:*events-seen*)
		  (push
		     'ungraded
		     badnesses)))))
      
      (make-emt:testral:suite
	 :contents
	 (make-emt:testral:note-list
	    :notes
	    ;;Reverse the note list so it's in the order that it
	    ;;occured in.
	    (nreverse emt:testral:*events-seen*))
	 ;;Need to acquire this.  At least errors that we
	 ;;handle here - which may be just overall abort.
	 ;;See the call to `emtt:trap-errors'
	 :badnesses badnesses
	 ;;$$WRITEME Use `emt:trace:properties' for this?  But change
	 ;;its name?  (And watch the scoping)
	 :info '())))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/explorers/clause)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/explorers/clause.el ends here