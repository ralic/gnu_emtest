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
(eval-when-compile (require 'cl))
(require 'utility/pathtree)
(require 'emtest/common/testral-types)
(require 'emtest/viewer/view-types)
(require 'emtest/common/grade-types)

;;;_. Body
;;;_ , Summing grades
;;;_  . emtvr:grade->summary
(defun emtvr:grade->summary (obj)
   "Change OBJ object into a grade summary.
OBJ must be a emt:testral:grade-aux and may already be a summary."
   (check-type obj emt:testral:grade-aux)
   (typecase obj 
      (emt:testral:grade:summary obj)
      (t
	 (let
	    ((obj-aux (emt:testral:make-grade:summary)))
	    (emtvr:add-badnesses obj-aux obj)
	    obj-aux))))

;;;_  . emtvr:add-badnesses
(defun emtvr:add-badnesses (sums a)
   ""
   (typecase a
      (emt:testral:grade:summary
	 (incf 
	    (emt:testral:grade:summary->test-cases sums)
	    (emt:testral:grade:summary->test-cases a))
	 (incf 
	    (emt:testral:grade:summary->fails sums)
	    (emt:testral:grade:summary->fails a))	 
	 (incf 
	    (emt:testral:grade:summary->ungradeds sums)
	    (emt:testral:grade:summary->ungradeds a))
	 (incf 
	    (emt:testral:grade:summary->dormants sums)
	    (emt:testral:grade:summary->dormants a))
	 (incf 
	    (emt:testral:grade:summary->blowouts sums)
	    (emt:testral:grade:summary->blowouts a)))
      (emt:testral:grade:test-case
	 (incf 
	    (emt:testral:grade:summary->test-cases sums)))
      (emt:testral:grade:fail
	 (incf 
	    (emt:testral:grade:summary->fails      sums)))
      (emt:testral:grade:ungraded
	 (incf 
	    (emt:testral:grade:summary->ungradeds  sums)))
      (emt:testral:grade:dormant
	 (incf 
	    (emt:testral:grade:summary->dormants   sums)))
      (emt:testral:grade:blowout
	 (incf 
	    (emt:testral:grade:summary->blowouts   sums)))      
      (t nil)))

;;;_  . emtvr:combine-badnesses
(defun emtvr:combine-badnesses (bads)
   "Combine the list BADS into one entry"
   (let
      ((all
	  (reduce
	     #'(lambda (a b)
		  (check-type a emt:testral:grade-aux)
		  (check-type b emt:testral:grade-aux)
		  (cond
		     ((null a) b)
		     ((null b) a)
		     ((and
			 (emt:testral:grade-p a)
			 (emt:testral:grade-p b))
			(let
			   ((sums (emt:testral:make-grade:summary)))
			   (emtvr:add-badnesses sums a)
			   (emtvr:add-badnesses sums b)
			   sums))
		     (t
			(error "Shouldn't get here"))))
	     bads)))
      (check-type all emt:testral:grade-aux)
      all))


;;;_  . emtvr:notelist-raw-badnesses
(defun emtvr:notelist-raw-badnesses (note-list)
   ""
   (mapcar
      #'emt:testral:base->badnesses
      (emt:testral:note-list->notes note-list)))

;;;_  . emtvr:badnesses:get-own
(defun emtvr:badnesses:get-own (node)
   "Get a node's own badnesses.

This includes badnesses that are not expressed in its children but
could be, such as when a note-list hasn't been expanded."
   (check-type node emt:view:presentable)
   (etypecase node
      (emt:view:suite-newstyle
	 (let
	    ((s
		(emt:view:suite-newstyle->result node)))
	    (etypecase s 
	       (null) 
	       (emt:testral:suite
		  (let
		     ((own-badnesses (emt:testral:suite->badnesses s)))
		     ;;If NODE is an inner node (wrt pathtree), it
		     ;;contributes only its intrinsic grading.
		     (if
			(emtvp:node->children node)
			own-badnesses
			;;But if NODE is a leaf node, it contributes
			;;all its contents' grading.
			(let
			   ((contents
			       (emt:testral:suite->contents s)))
			   (typecase contents
			      (emt:testral:note-list
				 (emtvr:combine-badnesses
				    (cons
				       own-badnesses
				       (emtvr:notelist-raw-badnesses contents))))
			      (t own-badnesses))))))
	       (emt:testral:test-runner-info
		  '()))))
      (emt:view:TESTRAL '())
      (emt:view:TESTRAL-unexpanded
	 '())
      ;;The base case shouldn't be here, but accept it for now
      (emt:view:presentable '())))


;;;_  . emtvr:get-subtree-badnesses
(defun emtvr:get-subtree-badnesses (node)
   ""
   (check-type node emt:view:presentable)
   (let*
      (
	 (childrens-badnesses
	    (mapcar
	       #'(lambda (child)
		    (emt:view:presentable->sum-badnesses
		       child))
	       (emtvp:node->children node)))

	 ;;Accessor
	 (own-badnesses
	    (emtvr:badnesses:get-own node)))
      (emtvr:combine-badnesses
	 (cons
	    own-badnesses
	    childrens-badnesses))))

;;;_  . emtvr:cache-subtree-badnesses
(defun emtvr:cache-subtree-badnesses (node)
   ""
   (check-type node emtvp:node)
   (when (typep node 'emt:view:presentable)
      (setf
	 (emt:view:presentable->sum-badnesses node)
	 (emtvr:get-subtree-badnesses node))))

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
