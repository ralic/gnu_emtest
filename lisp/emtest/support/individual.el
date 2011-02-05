;;;_ emtest/support/individual.el --- Properties for specific clauses or suites

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint

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

;; For now, there's only one thing in this file and it's not
;; individuated yet, though it will be.

;;;_ , Requires
(require 'emtest/main/all-explorers)

;;;_. Body
;;;_ , Types
(defstruct (emt:ind:entry
	      (:type list)
	      (:copier nil)
	      (:conc-name emt:ind:entry->)
	      (:constructor emt:ind:make-entry))
   "An individual test entry"
   (test-id () :type emthow)
   (cached-score () :type (or integer null))
   (score-properties-alist () :type (repeat (list symbol integer))) 
   (property-alist () :type (repeat (list symbol t))))

;;;_ , List of individual tests
(defvar emt:ind:alist 
   '()
   "Alist of properties of individual tests" )
;;;_ , emt:ind:get-entry
(defsubst emt:ind:get-entry (test-id)
   "Get the entry for TEST-ID"
   (assoc test-id emt:ind:alist))
;;;_ , emt:ind:add-entry
(defsubst emt:ind:add-entry (entry)
   "Add entry ENTRY"
   (push entry emt:ind:alist))
;;;_ , emt:ind:get-prop Get properties of a test
;;;_ , emt:ind:set-prop Set properties of a test
;;;_ , emt:ind:set-score-component
(defun emt:ind:set-score-component (test-id key bonus)
   "Set a score component KEY of TEST-ID to BONUS"
   (let
      ((entry (emt:ind:get-entry test-id)))
      (if entry
	 (setf
	    (emt:ind:entry->score-properties-alist entry)
	    (cons
	       (cons key bonus)
	       (assq-delete-all 
		  key 
		  (emt:ind:entry->score-properties-alist entry))))
	 (emt:ind:add-entry
	    (emt:ind:make-entry 
	       :test-id test-id
	       :score-properties-alist (list (cons key bonus)))))))


;;;_ , emt:ind:get-score 
(defun emt:ind:get-score (test-id)
   "Get the score of a given test.
The higher, the more easily the test will be run."

   ;;$$IMPROVE ME If TEST-ID has test properties, use them.
   (emt:exps:get-base-score (car test-id)))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/support/individual)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/support/individual.el ends here
