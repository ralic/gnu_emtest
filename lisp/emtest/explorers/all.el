;;;_ emtest/explorers/all.el --- Explorer-collection functionality

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

(require 'emtest/main/config)
(require 'emtest/types/run-types)
(require 'emtest/types/testral-types)

;;;_. Body
;;;_ , Types
;;;_  . emthow:hello
'(defstruct (emthow:hello
	      (:copier nil)
	      (:constructor emthow:make-hello)
	      (:conc-name emthow:hello->)
	      (:include emthow))
   "Pseudo-explore method used to tell about the tester")

;;;_ , Collecting explorers
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
   "Add an explorer to our alist of explorers"
   
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
   (let* 
       ((cell
	   (assq (car how) emtt:test-finder:method-list)))
       (if
	  cell
	  (second cell)
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

;;;_    . Register it

(emtt:add-explorer 'hello #'emtt:explore-hello
   "Tester signature") 

;;;_   , emtt:explore-fallback
;;Not part of the list of methods.
(defun emtt:explore-fallback (test-id props path report-f)
   "Report that no matching explore method could be found."

   (funcall report-f
      (emt:testral:make-suite
	 :contents nil
	 :grade 
	 'ungraded)))

;;;_. Footers
;;;_ , Provides
(provide 'emtest/explorers/all)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/all.el ends here
