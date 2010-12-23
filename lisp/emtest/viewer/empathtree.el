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
(require 'emtest/common/testral-types)
(require 'emtest/viewer/view-types)
(require 'emtest/viewer/sumgrades)

;;;_. Body
;;;_ , Getting summed grades

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
      ;;$$IMPROVE ME Treat this.
      (emt:view:TESTRAL-2 '())
      ;;Only the root will have this type.
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

;;;_ , Collecting TESTRAL notes
'(progn
;;;_  . emtvp:relation-element
;;$$OBSOLESCENT
(deftype emtvp:relation-element ()
   "Relations have this type"
   ;;'(or emtvp->id-element symbol integer)
   'string)
;;;_  . emtvr:relation-group-type
;;$$OBSOLESCENT
(deftype emtvr:relation-group-type ()
   "Relation-groups have this type"
   '(list* emtvp:relation-element (repeat emt:testral:newstyle)))
;;;_  . emtvr:pend-type
;;$$OBSOLESCENT
(deftype emtvr:pend-type ()
   "Pending items have this type"
   '(list emtvp->id-element (repeat emtvp:relation-element)))
;;;_  . emtvr:relation-group->path
'
(defun emtvr:relation-group->path (relation-group prefix)
   ""
   (append 
      prefix 
      (list (car relation-group))))
;;;_  . emtvr:collect-relation-groups
(defun emtvr:collect-relation-groups (id note-list)
   ""
   (check-type note-list emt:testral:note-list)
   (let
      ((relations '()))
      (dolist (note (emt:testral:note-list->notes note-list))
	 (when (emt:testral:id= id (emt:testral:base->parent-id note))
	    ;;$$TRANSITIONAL Later, no typecase needed.
	    (typecase note
	       (emt:testral:newstyle
		  ;;$$TRANSITIONAL Later, don't assertfail, just add a
		  ;;complaint note.
		  (assert 
		     (not 
			(emt:testral:id=
			   (emt:testral:base->parent-id note)
			   (emt:testral:base->id note))))
		  (let*
		     ((relation-name
			 (emt:testral:newstyle->relation note))
			(cell-already
			   (assoc relation-name relations)))
		     (if cell-already
			;;There are already note for this
			;;relation.  Splice this note in.  NB,
			;;this gets them into reverse order.
			(setcdr cell-already 
			   (cons note (cdr cell-already)))
			;;There are no actions on this deffile.  Add a
			;;place for them and include this action.
			(push 
			   (list relation-name note)
			   relations))))
	       ;;$$TRANSITIONAL
	       (t ()))))
      relations))

;;;_  . emtvr:add-relations-to-pathtree
(defun emtvr:add-relations-to-pathtree (relations tree node prefix)
   "Push each group in RELATIONS onto pathtree TREE.
Prefix will be (PREFIX... relation-name)"

   (dolist (relation-group relations)
      (check-type relation-group emtvr:relation-group-type)
      ;;$$TRANSITIONAL - something is making long prefixes.
      (unless (> (length prefix) 10)
	 (emtvp:add/replace-node-recurse
	    tree 
	    node
	    (emtvr:relation-group->path relation-group prefix)
	    (list 'note (cdr relation-group))))))

;;;_  . emtvr:collect-testral-aux3
;;$$FIX ME
(defun emtvr:collect-testral-aux3 (relations prefix)
   "Return a list of each node's (id prefix)"
   
   (apply #'nconc
      (mapcar
	 #'(lambda (relation-group)
	      (check-type relation-group emtvr:relation-group-type)
	      (let 
		 ((new-prefix 
		     (emtvr:relation-group->path relation-group prefix)))
		 (mapcar
		    #'(lambda (item)
			 (let
			    ((new-pend
				(list
				   (emt:testral:base->id item)
				   new-prefix)))
			    (check-type new-pend emtvr:pend-type)
			    new-pend))
		    (cdr relation-group))))
	 relations)))

;;;_  . emtvr:collect-testral
;;Caller should probably call (emtvp:freshen tree)
(defun emtvr:collect-testral (note-list tree node prefix)
   "Collect NOTE-LIST into a pathtree.
NOTE-LIST is a emt:testral:note-list
TREE is an `emtvp'
PREFIX is the path up to this point.
We assume no circularity in NOTE-LIST."

   (check-type note-list emt:testral:note-list)
   (check-type tree emtvp)
   (check-type node emtvp:node)
   (check-type prefix (repeat emtvp->id-element))
   (let
      ;;Initial id is `nil', meaning looking for no parent.
      ((pends (list (list nil prefix))))
      (pending:do-all
	 pends 
	 #'(lambda (el)
	      (check-type el emtvr:pend-type)
	      (let
		 ;;List to build
		 ((relations 
		     (emtvr:collect-relation-groups (car el) note-list)))

		 (emtvr:add-relations-to-pathtree 
		    relations 
		    tree 
		    node 
		    (second el))
	      
		 ;;Each found child's (id prefix), to be further
		 ;;explored.
		 (emtvr:collect-testral-aux3 relations (second el))))
	 ;; args 
	 '() 
	 ;; error-args-f
	 '() 
	 ;;Allow the list to expand
	 t)))

)

;;;_ , Collecting TESTRAL notes
;;;_  . emtvr:alist-cell-t
(deftype emtvr:alist-cell-t ()
   "Alist items have this type"
   '(list
       emtvp->id-element
       emt:view:presentable
       (repeat emt:view:presentable)))

;;;_  . emtvr:collect-testral-2-aux
(defun emtvr:collect-testral-2-aux (list-of-notes node tree)
   ""
   (let
      ;;A parent-id of `nil' means a child of NODE itself.
      ((alist (cons
		 (list nil node '())
		 (mapcar
		    #'(lambda (note)
			 (list 
			    (emt:testral:base->id note)
			    (emt:view:make-TESTRAL-2
			       :contents note)
			    '()))
		    list-of-notes))))
      (check-type alist (repeat emtvr:alist-cell-t))
      
      ;;Remove any previous children.  That's only needed for NODE
      ;;itself, the other viewables are new.
      (setf (emtvp:node->children node) nil)
      
      ;;Record each note's viewable with its parent
      (dolist (cell (cdr alist))
	 (let*
	    (  (note (emt:view:TESTRAL-2->contents (second cell)))
	       (parent-id (emt:testral:base->parent-id note))
	       (parent-cell (assoc parent-id alist)))
	    (check-type parent-id   emtvp->id-element)
	    (check-type parent-cell emtvr:alist-cell-t)
	    (push (second cell) (third parent-cell))))

      (check-type alist (repeat emtvr:alist-cell-t))

      ;;Put them in; pathtree will dirty them etc.
      (dolist (cell alist)
	 (dolist (child (third cell))
	    (emtvp:add-child
	       tree (second cell) (first cell) child t)))))

;;;_  . emtvr:collect-testral-2
(defun emtvr:collect-testral-2  (node tree)
   "Put NODE's TESTRAL notes under it in pathtree TREE."
   (check-type tree emtvp)
   (check-type node emtvp:node)
   (when (emt:view:suite-newstyle-p node)
      (let
	 ((result
	     (emt:view:suite-newstyle->result node)))
	 (when (emt:testral:suite-p result)
	    (let
	       ((contents
		   (emt:testral:suite->contents result)))
	       (when
		  (emt:testral:note-list-p contents)
		  (emtvr:collect-testral-2-aux 
		     (emt:testral:note-list->notes contents)
		     node
		     tree)))))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/empathtree)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; empathtree.el ends here
