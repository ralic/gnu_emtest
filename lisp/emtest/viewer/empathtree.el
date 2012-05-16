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
(require 'utility/pending)
(require 'emtest/types/testral-types)
(require 'emtest/viewer/view-types)
(require 'emtest/viewer/sumgrades)

;;;_. Body
;;;_ , Getting summed grades

;;;_  . emtvr:notelist-raw-grade
(defun emtvr:notelist-raw-grade (note-list)
   ""
   (mapcar
      #'emt:testral:note->grade
      (emt:testral:note-list->notes note-list)))
;;;_  . emtvr:grade:summarize-suite+notes
;;UNUSED but held ready if we change to dynamic reprinting.
(defun emtvr:grade:summarize-suite+notes (s)
   "Return a summary grade for a suite node and its notes
Intended for suites whose notes haven't been placed on pathtree"
   
   (let
      ((own-grade (emt:testral:suite->grade s))
	 (contents
	    (emt:testral:suite->contents s)))
      (typecase contents
	 (emt:testral:note-list
	    (emtvr:combine-grade
	       (cons
		  own-grade
		  (emtvr:notelist-raw-grade contents))))
	 (t own-grade))))

;;;_  . emtvr:grade:get-own
(defun emtvr:grade:get-own (node)
   "Get a node's own grade.

This includes grade that are not expressed in its children but
could be, such as when a note-list hasn't been expanded."
   (check-type node emt:view:presentable)
   (etypecase node
      (emt:view:suite
	 (let
	    ((s
		(emt:view:suite->result node)))
	    (etypecase s 
	       (null) 
	       (emt:testral:suite
		  (emt:testral:suite->grade s)))))
      (emt:view:note-placeholder '())
      (emt:view:note
	 (emt:testral:note->grade
	    (emt:view:note->contents node)))
      ;;Only the root will have this type.
      (emt:view:presentable '())))


;;;_  . emtvr:get-subtree-grade
(defun emtvr:get-subtree-grade (node)
   ""
   (check-type node emt:view:presentable)
   (let*
      (
	 (childrens-grade
	    (mapcar
	       #'(lambda (child)
		    (emt:view:presentable->sum-grades
		       child))
	       (emtvp:node->children node)))

	 ;;Accessor
	 (own-grade
	    (emtvr:grade:get-own node)))
      (emtvr:combine-grade
	 (cons
	    own-grade
	    childrens-grade))))

;;;_  . emtvr:cache-subtree-grade
(defun emtvr:cache-subtree-grade (node)
   ""
   (check-type node emtvp:node)
   (when (typep node 'emt:view:presentable)
      (setf
	 (emt:view:presentable->sum-grades node)
	 (emtvr:get-subtree-grade node))))

;;;_ , Collecting TESTRAL notes
;;;_  . emtvr:alist-cell-t
(deftype emtvr:alist-cell-t ()
   "Alist items have this type"
   '(list
       emt:testral:id-element
       emt:view:presentable
       (repeat emt:view:presentable)))

;;;_  . emtvr:collect-testral-2-aux
(defun emtvr:collect-testral-2-aux (list-of-notes node tree)
   ""
   (let
      ;;A parent-id of `nil' means a child of NODE itself.
      ((alist (cons
		 (list nil node '())
		 ;;For each note, make a viewable that we will later
		 ;;insert into pathtree.
		 (mapcar
		    #'(lambda (note)
			 (list 
			    (emt:testral:note->id note)
			    (emt:view:make-note
			       :contents note)
			    '()))
		    list-of-notes))))
      (check-type alist (repeat emtvr:alist-cell-t))
      
      ;;Remove any previous children.  That's only needed for NODE
      ;;itself, the other viewables are new.
      (setf (emtvp:node->children node) nil)
      
      ;;Record each note's viewable with its parent.  This may skip
      ;;nodes that have no parent.
      (dolist (cell (cdr alist))
	 (let*
	    (  (note (emt:view:note->contents (second cell)))
	       (parent-id (emt:testral:note->parent-id note))
	       (parent-cell (assoc parent-id alist)))
	    (when parent-cell
	       (check-type parent-id   emt:testral:id-element)
	       (check-type parent-cell emtvr:alist-cell-t)
	       (push (second cell) (third parent-cell)))))

      (check-type alist (repeat emtvr:alist-cell-t))

      ;;Put them in; pathtree will dirty them etc.
      (dolist (cell alist)
	 (dolist (child (third cell))
	    (let* 
	       ((path
		   (emt:testral:note->prestn-path 
		      (emt:view:note->contents child)))
		  (ancestor (second cell))
		  (parent
		     (if path
			(emtvp:find-node-under-node
			   tree 
			   (emt:testral:map-id->emtvp:name path) 
			   ancestor
			   #'emt:view:make-note-placeholder)
			ancestor)))
	       ;;$$IMPROVE ME If there's a `emt:view:note-placeholder', replace
	       ;;it with this one.
	       (emtvp:add-child
		  tree parent nil child t))))))

;;;_ , TESTRAL suites

;;;_  . emtvr:collect-testral-2
(defun emtvr:collect-testral-2  (node tree)
   "Put NODE's TESTRAL notes or runform-list under it in pathtree TREE."
   (check-type tree emtvp)
   (check-type node emtvp:node)
   (when (emt:view:suite-p node)
      (let
	 ((result
	     (emt:view:suite->result node)))
	 (when (emt:testral:suite-p result)
	    (let
	       ((contents
		   (emt:testral:suite->contents result)))
	       (typecase contents
		  (emt:testral:note-list
		     (emtvr:collect-testral-2-aux 
			(emt:testral:note-list->notes contents)
			node
			tree))
		  (emt:testral:runform-list
		     (emtvr:collect-runform-list
			node tree contents))))))))

;;;_  . emtvr:collect-runform-list
(defun emtvr:collect-runform-list (node tree runform-list)
   "Put viewables for RUNFORM-LIST under NODE in TREE."
   (mapcar
      #'(lambda (runform)
	   (emtvo:receive-cb 
	      (emtt:explorable->prestn-path runform)
	      (emt:view:make-explorable :contents runform)))
      (emt:testral:runform-list->els runform-list)))

;;;_ , emtvr:place-node

(defun emtvr:place-node (tree presentation-path cell)
   "Add CELL to TREE at PRESENTATION-PATH.
Cell must be a emt:view:presentable descendant."
   (check-type cell emt:view:presentable)
   (let
      ((old-node 
	  (emtvp:find-node tree presentation-path
	     #'emt:view:make-presentable)))

      (setf
	 (emtvp:node->name cell)
	 (car (last presentation-path)))
      ;;Adopt suite children but not note children
      (setf
	 (emtvp:node->children cell)
	 (delq nil
	    (mapcar
	       #'(lambda (child)
		    (unless (emt:view:note-p child) child))
	       (emtvp:node->children old-node))))
       
      ;;$$IMPROVE ME if (eq old-node cell) just dirty it for
      ;;resummary/redisplay as `updated', and handle that.
      (emtvp:replace-node
	 tree old-node cell)))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/empathtree)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; empathtree.el ends here
