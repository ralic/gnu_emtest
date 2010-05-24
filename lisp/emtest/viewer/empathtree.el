;;;_ empathtree.el --- Emtest operations on pathtree

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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

(require 'utility/pathtree)
(require 'emtest/common/testral-types)
(require 'emtest/viewer/view-types)
(require 'cl)  ;;For `union'

;;;_. Body
;;;_ , emtvr:summarize
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
;;;_  . emtvr:combine-badnesses
(defsubst emtvr:combine-badnesses (bads)
   (reduce #'union bads))

;;;_  . emtvr:sum-testral-note-badnesses
(defun emtvr:sum-testral-note-badnesses (data)
   ""
   
   (let*
      ()
      ;;Not correct because it maps the tail too, but won't hurt much.
      ;;Could write `mapcar-but-tail'.  Could reduce instead, and
      ;;throw out when `end' is seen.
      (emtvr:combine-badnesses
	 (mapcar
	    #'emt:testral:base-badnesses
	    ;;Was `emt:view:suite-newstyle-start'.
	    (emt:view:TESTRAL-unexpanded-start data)))))
;;;_  . emtvr:sum-node-badnesses
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
			    (emtvr:suite-newstyle-result
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



;;;_ , emtvr:conform-stages
(defun emtvr:conform-stages (note-list)
   ""
   
   (let*
      ()
      
      ))
;;Make it be bounded by stages.

;;Conforming is only done in the course of expanding it.
;;;_ , emtvr:expand-testral
(defun emtvr:expand-testral (node)
   ""
   (let*
      ()
      ;;Expand the TESTRAL list of notes

      ))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/empathtree)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; empathtree.el ends here
