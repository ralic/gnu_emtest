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

;;;_  . emt:pth:grd:notelist-raw
(defun emt:pth:grd:notelist-raw (note-list)
   "Get a raw notelist's grade"
   (mapcar
      #'emt:testral:note->grade
      (emt:testral:note-list->notes note-list)))
;;;_  . emt:pth:grd:summarize-suite+notes
;;UNUSED but held ready if we change to dynamic reprinting.
(defun emt:pth:grd:summarize-suite+notes (s)
   "Return a summary grade for a suite node and its notes
Intended for suites whose notes haven't been placed on pathtree"
   
   (let
      ((own-grade (emt:testral:suite->grade s))
	 (contents
	    (emt:testral:suite->contents s)))
      (typecase contents
	 (emt:testral:note-list
	    (emt:grd:combine
	       (cons
		  own-grade
		  (emt:pth:grd:notelist-raw contents))))
	 (t own-grade))))

;;;_  . emt:pth:grd:node-proper
(defun emt:pth:grd:node-proper (node)
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


;;;_  . emt:pth:grd:subtree
(defun emt:pth:grd:subtree (node)
   ""
   (check-type node emt:view:presentable)
   (let*
      (
	 (childrens-grade
	    (mapcar
	       #'(lambda (child)
		    (emt:view:presentable->sum-grades
		       child))
	       (pathtree:node->children node)))

	 ;;Accessor
	 (own-grade
	    (emt:pth:grd:node-proper node)))
      (emt:grd:combine
	 (cons
	    own-grade
	    childrens-grade))))

;;;_  . emt:pth:grd:cache-subtree-grade
(defun emt:pth:grd:cache-subtree-grade (node)
   ""
   (check-type node pathtree:node)
   (when (typep node 'emt:view:presentable)
      (setf
	 (emt:view:presentable->sum-grades node)
	 (emt:pth:grd:subtree node))))

;;;_ , Collecting TESTRAL notes
;;;_  . emt:pth:alist-cell
(deftype emt:pth:alist-cell ()
   "Alist items have this type"
   '(list
       emt:testral:id-element
       emt:view:presentable
       (repeat emt:view:presentable)))

;;;_  . emt:pth:collect-testral-2-aux
(defun emt:pth:collect-testral-2-aux (list-of-notes node tree)
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
			       :name (emt:testral:note->id note)
			       :contents note)
			    '()))
		    list-of-notes))))
      (check-type alist (repeat emt:pth:alist-cell))
      
      ;;Remove any previous children.  That's only needed for NODE
      ;;itself, the other viewables are new.
      (setf (pathtree:node->children node) nil)
      
      ;;Record each note's viewable with its parent.  This may skip
      ;;nodes that have no parent.
      (dolist (cell (cdr alist))
	 (let*
	    (  (note (emt:view:note->contents (second cell)))
	       (parent-id (emt:testral:note->parent-id note))
	       (parent-cell (assoc parent-id alist)))
	    (when parent-cell
	       (check-type parent-id   emt:testral:id-element)
	       (check-type parent-cell emt:pth:alist-cell)
	       (push (second cell) (third parent-cell)))))

      (check-type alist (repeat emt:pth:alist-cell))

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
			(pathtree:find-node-under-node
			   tree 
			   (emt:testral:map-id->pathtree:name path) 
			   ancestor
			   #'emt:view:make-note-placeholder)
			ancestor)))
	       ;;$$IMPROVE ME If there's a `emt:view:note-placeholder', replace
	       ;;it with this one.
	       (pathtree:add-child
		  tree parent 
		  (emt:view:presentable->name child )
		  child t))))))

;;;_ , TESTRAL suites

;;;_  . emt:pth:collect-testral-2
(defun emt:pth:collect-testral-2  (node tree)
   "Put NODE's TESTRAL notes or runform-list under it in pathtree TREE."
   (check-type tree pathtree)
   (check-type node pathtree:node)
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
		     (emt:pth:collect-testral-2-aux 
			(emt:testral:note-list->notes contents)
			node
			tree))
		  (emt:testral:runform-list
		     (emt:pth:collect-runform-list
			node tree contents))))))))

;;;_  . emt:pth:collect-runform-list
(defun emt:pth:collect-runform-list (node tree runform-list)
   "Put viewables for RUNFORM-LIST under NODE in TREE."
   (mapcar
      #'(lambda (runform)
	   (emt:vw:og:receive-cb 
	      (emt:run:explorable->prestn-path runform)
	      (emt:view:make-explorable :contents runform)))
      (emt:testral:runform-list->els runform-list)))

;;;_ , emt:pth:place-node

(defun emt:pth:place-node (tree presentation-path cell)
   "Add CELL to TREE at PRESENTATION-PATH.
Cell must be a emt:view:presentable descendant."
   (check-type cell emt:view:presentable)
   (let
      ((old-node 
	  (pathtree:find-node tree presentation-path
	     #'emt:view:make-presentable)))

      (setf
	 (pathtree:node->name cell)
	 (car (last presentation-path)))
      ;;Adopt suite children but not note children
      (setf
	 (pathtree:node->children cell)
	 (delq nil
	    (mapcar
	       #'(lambda (child)
		    (unless (emt:view:note-p child) child))
	       (pathtree:node->children old-node))))
       
      ;;$$IMPROVE ME if (eq old-node cell) just dirty it for
      ;;resummary/redisplay as `updated', and handle that.
      (pathtree:replace-node
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
