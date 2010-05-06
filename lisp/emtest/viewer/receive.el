;;;_ emtest/viewer/receive.el --- Receive and organize test results, for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
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

;; 


;;;_ , Requires
(require 'emtest/common/testral-types)

;;;_. Body
;;;_ , Receive reports alist
;;;_  . Structure holding parameters for alist receive
(defstruct emtvr:data
   ""
   alist
   tree-insert-cb
   tree-remove-cb)

;;;_  . emtvr:make-empty-alist
;;Maybe the only ctor
(defun emtvr:make-empty-alist (insert remove)
   ""
   
   (make-emtvr:data
      :alist '()
      :tree-insert-cb insert
      :tree-remove-cb remove))


;;;_  . emtvr:newstyle
(defun emtvr:newstyle (receiver report)
   ""
   (check-type receiver emtvr:data)
   (let
      (  (testrun-id 
	    (emt:testral:report-testrun-id report))
	 (prefix 
	    (append
	       (list (emt:testral:report-tester-id report))
	       (emt:testral:report-test-id-prefix report))))
      
      ;;For each suite in the report, 
      (dolist (entry (emt:testral:report-suites report))
	 (emtvr:one-newstyle receiver entry testrun-id prefix))))

;;;_  . emtvr:test-gone-p
(defun emtvr:test-gone-p (suite)
   ""
   ;;`emt:testral:test-runner-info' is never test-gone
   ;;This representation is tentative.
   (when (emt:testral:suite-p suite)
      (find
	 '(bad-before-test not-found)
	 ;;Can also be test-runner
	 (emt:testral:suite-badnesses suite)
	 :test #'equal)))

;;;_  . emtvr:one-newstyle
(defun emtvr:one-newstyle 
   (receiver entry testrun-id prefix)
   ""
   (check-type receiver emtvr:data)
   (destructuring-bind (how-to-run id suite) entry
      (let
	 ( 
	    (presentation-path (append prefix id))
	    ;;When we allow reporting unique ids, key will prefer to
	    ;;be one when possible.
	    (key how-to-run))
	 ;;Handle special case: If suite reports that it has disappeared,
	 ;;remove it from alist and from tree.  (How from tree?)
	 (if
	    (emtvr:test-gone-p suite)
	    (progn
	       (setf
		  (emtvr:data-alist receiver)
		  (delete* key (emtvr:data-alist receiver)
		     :key #'emtvr:suite-newstyle-id))
	       (funcall 
		  (emtvr:data-tree-remove-cb receiver)
		  presentation-path))
	    
	    ;;The normal case:
	    (let
	       ((old-cell
		   (find key (emtvr:data-alist receiver)
		      :key #'emtvr:suite-newstyle-id)))
	       (if old-cell
		  ;;Cell is already present.  Alter it.  Still replace
		  ;;the node in the pathtree.
		  (progn
		     (setf
			(emtvr:suite-newstyle-testrun-id old-cell)
			testrun-id
			(emtvr:suite-newstyle-suite old-cell)
			suite)
		     (funcall 
			(emtvr:data-tree-insert-cb receiver)
			presentation-path old-cell))
	    
		  ;;It's not present in alist.  Insert it.
		  (let 
		     ((cell
			 (make-emtvr:suite-newstyle
			    :id key
			    :how-to-run how-to-run
			    :presentation-path presentation-path
			    :testrun-id testrun-id
			    :suite suite)))
		     (push cell (emtvr:data-alist receiver))
		     (funcall 
			(emtvr:data-tree-insert-cb receiver)
			presentation-path cell))))))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/receive)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/receive.el ends here
