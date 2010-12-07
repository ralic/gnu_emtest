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
 * FUNCTION explores the test or suite" )

;;;_  . Add an explorer

(defun emtt:add-explorer (pred func &optional name &rest dummy)
   ""
   
   (unless (assq pred emtt:test-finder:method-list)
      (push 
	 (list pred func name)
	 emtt:test-finder:method-list)))
;;;_  . emtt:get-explore-func Get relevant function for emthow object
;;$$WRITE ME

;;;_  . emtt:explore-hello
;;This doesn't require an autoload but all others do.
(defun emtt:explore-hello (test-id props path report-f)
   ""
   
   (funcall report-f
      (emt:testral:make-test-runner-info
	 :name "Emtest"
	 :version emtt:version
	 :explore-methods-supported
	 (mapcar #'third emtt:test-finder:method-list))))

;;;_  . Insinuate

(emtt:add-explorer #'emthow:hello-p #'emtt:explore-hello
   "Tester signature") 


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/explorers/all)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/explorers/all.el ends here
