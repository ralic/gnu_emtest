;;;_ viewers/receive.el --- Receive and organize test results, for Emtest

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

(rtest:if-avail
   (require 'common/testral-types/testhelp)
   (require 'tester/testhelp/match))

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
(put 'emtvr:one-newstyle 'rtest:test-thru
   'emtvr:newstyle)
;;;_   , Test helpers

(emtm:define-struct-governor
   emtvr:suite-newstyle
   id how-to-run presentation-path testrun-id suite)

;;;_   , Tests

(rtest:deftest emtvr:newstyle

   ;;Add reports.  Lists should contain what's expected.
   (  "Situation: Empty report.
Response: List still contains nothing."
      (let*
	 ((nodes-freshened '())
	    (remember-freshened-node
	       #'(lambda (x y)
		    (push (list x y) nodes-freshened)))
	    (receiver (emtvr:make-empty-alist remember-freshened-node #'ignore))
	    (report (emt:eg
		       (project emtest)
		       (sub-project testral)
		       (library types)
		       (type report)
		       (name empty))))
	 (emtvr:newstyle receiver report)
	 ;;Still an empty list
	 (assert
	    (equal
	       (emtvr:data-alist receiver)
	       '())
	    t)
	 ;;No callbacks happened
	 (assert
	    (emtm
	       nodes-freshened
	       (list))
	    t)
	 t))
   
   
   ;;Report w/1 entry
   (  "Situation: Have added a report w/1 entry
Response: List contains that one entry."
      (let*
	 ((nodes-freshened '())
	    (remember-freshened-node
	       #'(lambda (x y)
		    (push (list x y) nodes-freshened)))
	    (receiver (emtvr:make-empty-alist remember-freshened-node #'ignore))
	    (report (emt:eg
		       (project emtest)
		       (sub-project testral)
		       (library types)
		       (type report)
		       (role original-add)
		       (what-test test-1))))
	 (emtvr:newstyle receiver report)

	 ;;A list with just that entry
	 (assert
	    (emtm
	       (emtvr:data-alist receiver)
	       (list
		  (eval 
		     '(emt:eg
			 (type receive-alist-item)
			 (role original-add)
			 (what-test test-1)))))
	    t)
	 ;;One callback happened
	 (assert
	    (emtm
	       nodes-freshened
	       (list
		  (list
		     (eval 
			'(emt:eg (type presentation-path)(what-test test-1)))
		     (make-emtvr:suite-newstyle
			:presentation-path
			(eval 
			   '(emt:eg (type presentation-path)(what-test test-1)))
			:suite
			(eval 
			   '(emt:eg (type suite)(what-test test-1)(role original-add)))
			))))
	    t)
	 t))
   

   ;;Add successive reports on the same test.
   (  "Situation: Have added a report w/1 entry.
Operation:  Another report with a different result for the same test.
Response: List contains just that one entry, not duplicated."
      (let*
	 ((nodes-freshened '())
	    (remember-freshened-node
	       #'(lambda (x y)
		    (push (list x y) nodes-freshened)))
	    (receiver (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	 (emtvr:newstyle receiver 
	    (emt:eg
	       (project emtest)
	       (sub-project testral)
	       (library types)
	       (type report)
	       (what-test test-1)
	       (role original-add)))
	 
	 (setq nodes-freshened '())
	 ;;Add a report that just overrides the original
	 (emtvr:newstyle receiver 
	    (emt:eg
	       (project emtest)
	       (sub-project testral)
	       (library types)
	       (type report)
	       (what-test test-1)
	       (role replace)))

	 ;;A list with just that entry
	 (assert
	    (emtm
	       (emtvr:data-alist receiver)
	       (list
		  (eval 
		     '(emt:eg
			 (type receive-alist-item)
			 (role replace)
			 (what-test test-1)))))
	    t)

	 ;;One (new) callback happened
	 (assert
	    (emtm
	       nodes-freshened
	       (list
		  (list
		     (eval 
			'(emt:eg (type presentation-path)(what-test test-1)))
		     (make-emtvr:suite-newstyle
			:presentation-path 
			(eval 
			   '(emt:eg (type presentation-path)(what-test test-1)))
			:suite
			(eval 
			   '(emt:eg (type suite)(what-test test-1)(role replace)))
			))))
	    t)
	 t))

   ;;Remove a no-longer-existing runnable.
   (  "Situation: Have added a report w/1 entry.
Operation: Report removes previous report.
Response: List no longer contains that entry; it is empty."
      (emt:eg:narrow ((project emtest)(sub-project testral)(library types))
	 (let*
	    ((nodes-freshened '())
	       (remember-freshened-node
		  #'(lambda (x y)
		       (push (list x y) nodes-freshened)))
	       (receiver 
		  (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	    (emtvr:newstyle receiver 
	       (emt:eg
		  (type report)
		  (role original-add)
		  (what-test test-1)))

	    (emtvr:newstyle receiver 
	       (emt:eg
		  (type report)
		  (role remove-previous)
		  (what-test test-1)))

	    ;;An empty list again
	    (assert
	       (equal
		  (emtvr:data-alist receiver)
		  '())
	       t)

	    ;;Not yet doing callbacks wrt this.

	    t)))

   (  "Situation: Have added a report w/1 entry.
Operation: Add a second report
Response: List contains both entries."
      (emt:eg:narrow ((project emtest)(sub-project testral)(library types))
	 (let*
	    ((nodes-freshened '())
	       (remember-freshened-node
		  #'(lambda (x y)
		       (push (list x y) nodes-freshened)))
	       (receiver 
		  (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	    (emtvr:newstyle receiver 
	       (emt:eg
		  (type report)
		  (role original-add)
		  (what-test test-1)))

	    ;;Empty the callback list
	    (setq nodes-freshened '())
	    (emtvr:newstyle receiver 
	       (emt:eg (type report)(what-test test-2)))

	    ;;Test that we have the right contents.  Skip for now
	    ;;because emtm doesn't have a set matcher yet.

	    ;;One (new) callback happened
	    (assert
	       (emtm
		  nodes-freshened
		  (list
		     (list
			(eval 
			   '(emt:eg (type presentation-path)(what-test test-2)))
			(make-emtvr:suite-newstyle
			   :presentation-path 
			   (eval 
			      '(emt:eg (type presentation-path)(what-test test-2)))
			   :suite
			   (eval 
			      '(emt:eg (type suite)(what-test test-2)))
			   ))))
	       t)

	    t)))


   ;;Report w/2 entries
   ;;
   '
   (  "Situation: Empty tree.
Operation: Add a report w/2 entries.
Response: List contains both entries."
      (let*
	 ((nodes-freshened '())
	    (remember-freshened-node
	       #'(lambda (x y)
		    (push (list x y) nodes-freshened)))
	    (receiver (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	 ;;List length.

	 ;;Matches the pattern - but we don't have a set-match pattern
	 ;;yet
	 
	 ))
   )

;;;_ , Emviewer operations on pathtree
;;;_  . emtvr:summarize
(defun emtvr:summarize (tree)
   ""
   ;;For now, just call `emtvr:sum-node-badnesses' just before
   ;;printing.
   
   ;;As yet we have no good way of making a predicate that takes
   ;;arguments.  Contents should be a `emt:view:presentable'
   (check-type tree emtvp-node)
   ;;Traverse the tree recursively:  Recurse on all children, then
   ;;join those sets together, put it in sum-badnesses field, and
   ;;return it.

   ;;And `dirtinesses' should propagate upwards.

   ;;No, organizing that is the business of pathtree.  Which will have
   ;;a field for a list of dirtinesses
   (let*
      ()
      
      ))
;;;_   , emtvr:combine-badnesses
(require 'cl)  ;;For `union'
(defsubst emtvr:combine-badnesses (bads)
   (reduce #'union bads))

;;;_   , emtvr:sum-testral-note-badnesses
(defun emtvr:sum-testral-note-badnesses (data)
   ""
   
   (let*
      ()
      ;;Not correct because it maps the tail too, but
      ;;won't hurt much.  Could write
      ;;`mapcar-but-tail'.
      ;;Encap.  This handles notes.
      (emtvr:combine-badnesses
	 (mapcar
	    #'emt:testral:base-badnesses
	    (emt:view:suite-newstyle-start data)))))
;;;_   , emtvr:sum-node-badnesses
;;Rename `emtvr:sum-viewnode-badnesses'
;;Design: Maybe split into accessor and summer.  Accessor should be
;;conformer: It will set its own node right after children are all made right.

;;Node data is covariant with this.
(defun emtvr:sum-node-badnesses (node)
   ""
   (check-type node emtvp-node)
   
   (let*
      (
	 (data
	    (emtvp-node-data node))
	 (input-badnesses
	    (mapcar
	       #'(lambda (child)
		    (emt:view:presentable-sum-badnesses
		       (emtvp-node-data child)))
	       (emtvp-node-children node)))
	 ;;Gives a list of badnesses.
	 ;;Accessor
	 (own-badnesses
	    (let
	       ()
	       (etypecase data
		  (emt:view:suite-newstyle
		     (let
			((s
			    (emtvr:suite-newstyle-suite
			       (emt:view:suite-newstyle-cell data))))
			(etypecase s 
			   (null) 
			   (emt:testral:suite
			      (emt:testral:suite-badnesses s))
			   (emt:testral:test-runner-info
			      '()))))
		  (emt:view:TESTRAL '())
		  (emt:view:TESTRAL-unexpanded
		     (emtvr:sum-testral-note-badnesses data))
		  (emt:view:presentable '()))))
	 (sum-badnesses
	    (emtvr:combine-badnesses
	       (cons
		  own-badnesses
		  input-badnesses))))
      (setf
	 (emt:view:presentable-sum-badnesses data)
	 sum-badnesses)
      sum-badnesses))



;;;_   , Tests
(rtest:deftest emtvr:sum-node-badnesses

   ;;Given a tree, compare result to expected example.
   ;;Summary of the one-error example gets propagated upwards.

   ;;Even if the TESTRAL notes haven't been expanded yet.

   ;;Looking at a tree with two errors, from a corresponding report.
   ;;Summary combines them at the appropriate spot.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   
   )
;;;_  . emtvr:conform-stages
(defun emtvr:conform-stages (note-list)
   ""
   
   (let*
      ()
      
      ))
;;Make it be bounded by stages.

;;Conforming is only done in the course of expanding it.
;;;_   , Tests
(rtest:deftest emtvr:conform-stages
   ;;Given the note-list, transform it to the conformed version
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   
   )
;;;_  . emtvr:expand-testral
(defun emtvr:expand-testral (node)
   ""
   (let*
      ()
      ;;Expand the TESTRAL list of notes

      ))

;;;_   , Tests
;;After 
(rtest:deftest emtvr:expand-testral
   ;;Given a view testral node of one level, expand it.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )

   ;;(Later) Different function: Recursively to the leaves
   ;;(Later, or YAGNI) As far as an expansion policy says to.
   
   )

;;;_. Footers
;;;_ , Provides

(provide 'viewers/receive)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/receive.el ends here
