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
   (test-path () :type emt:t:test-path) 
   (property-alist () :type (repeat (list symbol t))))

;;;_ , List of individual tests
(defvar emt:ind:alist 
   '()
   "Alist of properties of individual tests" )
;;;_ , emt:ind:get-entry
(defsubst emt:ind:get-entry (test-path)
   "Get the entry for TEST-PATH"
   (check-type test-path emt:t:test-path)
   (assoc test-path emt:ind:alist))
;;;_ , emt:ind:add-entry
(defsubst emt:ind:add-entry (entry)
   "Add entry ENTRY"
   (check-type test-path emt:t:test-path)
   (push entry emt:ind:alist))
;;;_ , emt:ind:get-prop 
(defun emt:ind:get-prop (test-path key)
   "Get property KEY of test TEST-PATH"
   (check-type test-path emt:t:test-path)
   (let
      ((entry (emt:ind:get-entry test-path)))
      (if entry
	 (let
	    ((apair (assq key  (emt:ind:entry->property-alist entry))))
	    (when apair (cdr apair)))
	 nil)))

;;;_ , emt:ind:set-prop 
(defun emt:ind:set-prop (test-path key value)
   "Set property KEY of test TEST-PATH to VALUE"
   (check-type test-path emt:t:test-path)
   (let
      ((entry (emt:ind:get-entry test-path)))
      (if entry
	 (setf
	    (emt:ind:entry->property-alist entry)
	    (cons
	       (cons key value)
	       (assq-delete-all 
		  key 
		  (emt:ind:entry->property-alist entry))))
	 (emt:ind:add-entry
	    (emt:ind:make-entry 
	       :test-path test-path
	       :property-alist (list (cons key value)))))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/support/individual)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/support/individual.el ends here
