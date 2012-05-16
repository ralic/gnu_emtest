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
(require 'emtest/types/testral-types)
(require 'emtest/types/run-types)
(require 'emtest/viewer/view-types)

;;;_. Body
;;;_ , Receive reports alist
;;;_  . Structure holding parameters for alist receive
(defstruct (emtvr:data
	    (:constructor emtvr:make-data)
	    (:conc-name emtvr:data->)
	    (:copier nil))
   ""
   alist
   tree-insert-cb
   tree-remove-cb)

;;;_  . emtvr:make-empty-alist
;;Maybe the only ctor
(defun emtvr:make-empty-alist (insert remove)
   ""
   
   (emtvr:make-data
      :alist '()
      :tree-insert-cb insert
      :tree-remove-cb remove))


;;;_  . emtvr:receive
(defun emtvr:receive (receiver report)
   ""
   (check-type receiver emtvr:data)
   (check-type report emt:testral:report)
   (let
      (  (testrun-id 
	    (emt:testral:report->testrun-id report))
	 (prefix 
	    (emt:testral:report->test-id-prefix report)))
      
      ;;For each suite in the report, 
      (dolist (entry (emt:testral:report->suites report))
	 (emtvr:receive-one receiver entry testrun-id prefix))))

;;;_  . emtvr:test-gone-p
(defun emtvr:test-gone-p (suite)
   ""
   ;;This representation is tentative.
   (when (emt:testral:suite-p suite)
      '(find
	 ;;$$UPDATE ME  This is no longer what we look for.
	 ;;$$RETHINK ME Find gone-ness in a different way, perhaps by
	 ;;recognizing an explorable type for invalid.
	 '(bad-before-test not-found)
	 ;;$$ADD ME Type can also be test-runner info.
	 (emt:testral:suite->grade suite)
	 :test #'equal)
      nil
      ))

;;;_  . emtvr:receive-one

;;$$IMPROVE ME Become smarter about aliases.  Pick the "best" one as
;;the id, favoring UUIDs (recognized as strings), allowing viewer's
;;recommendations and the test-runner's.  Try all aliases in deleting
;;it.

;;$$ADD ME suites (sometimes) indicate what their current children
;;are.  If other children are recorded, they no longer exist so remove
;;them.  Perhaps suite reports flag whether they have exhaustively
;;listed their children.
(defun emtvr:receive-one (receiver entry testrun-id prefix)
   "Receive and store a single test report.

Stores it on receiver' alist, keyed by how to run it."

   (check-type receiver emtvr:data)
   (destructuring-bind (explorable suite) entry
      (check-type explorable emt:t:explorable)
      (let*
	 ( 
	    (id (emt:t:explorable->prestn-path explorable))
	    (presentation-path (append prefix id))
	    (key (emt:t:explorable->how-to-run explorable)))
	 (check-type key emt:t:how)

	 ;;Handle special case: If suite reports that it has disappeared,
	 ;;remove it from alist and from tree.  (How from tree?)
	 (if
	    (emtvr:test-gone-p suite)
	    (progn
	       (setf
		  (emtvr:data->alist receiver)
		  (delete* key (emtvr:data->alist receiver)
		     :test #'equal
		     :key #'emt:view:suite->id))
	       (funcall 
		  (emtvr:data->tree-remove-cb receiver)
		  presentation-path))
	    
	    ;;The normal case:
	    (let
	       ((old-cell
		   (find key (emtvr:data->alist receiver)
		      :test #'equal
		      :key #'emt:view:suite->id)))

	       
	       (if old-cell
		  ;;Update old cell.
		  (progn
		     (setf
			(emt:view:suite->testrun-id old-cell)
			testrun-id
			(emt:view:suite->result old-cell)
			suite)

		     ;;$$RETHINK ME Maybe should just dirty it in
		     ;;pathtree.  This puts it where it was, but also
		     ;;dirties it
		     (funcall 
			(emtvr:data->tree-insert-cb receiver)
			presentation-path old-cell))
	    
		  ;;It's not present in alist.  Insert it.
		  (let 
		     ((cell
			 (emt:view:make-suite
			    :id                key
			    :explorable        explorable
			    :presentation-path presentation-path
			    :testrun-id        testrun-id
			    :result            suite)))
		     (push cell (emtvr:data->alist receiver))
		     (funcall 
			(emtvr:data->tree-insert-cb receiver)
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
