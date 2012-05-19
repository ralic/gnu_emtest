;;;_ utility/pathtree/rtest.el --- Tests for pathtree

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
(require 'emtest/main/define)
(require 'utility/pathtree)
(require 'utility/pathtree/testhelp)

(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/match)
(require 'utility/pending)

;;;_. Body
;;;_ , Helper
;;;_ , pathtree:find-node
;;These were tests of a function that's now obsolete.
(emt:deftest-3 pathtree:find-node
   (nil
      (progn
	 (emt:doc "Proves that `pathtree:th:callback:push' terminates.")
	 (pathtree:th:let-usuals nil
	    (assert
	       (pending:terminates-on-examples #'pathtree:th:callback:push nil
		  (pathtree:th:make-derived-node :name "a" :dirty-flags
		     '(new))))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Dirty list has members.")
	 (emt:doc "Operation: Freshen the tree.")
	 (emt:doc "Result: Dirty list is now empty.")
	 (pathtree:th:let-usuals nil
	    (push
	       (pathtree:th:make-derived-node :name "a" :dirty-flags
		  '(new))
	       (pathtree->dirty tree))
	    (pathtree:freshen tree)
	    (assert
	       (null
		  (pathtree->dirty tree))
	       t)
	    (assert
	       (equal
		  (pathtree:th:how-dirty "a")
		  '(new))
	       t)
	    (pathtree:th:assert-name-dirtiness "a"
	       '(new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Empty tree")
	 (emt:doc "Operation: Add one element, path = a")
	 (emt:doc "Response: That element is in the first ply.")
	 (pathtree:th:let-usuals
	    ((cell "cell 1"))
	    (pathtree:th:add/replace-node tree
	       '("a")
	       cell)
	    (pathtree:freshen tree)
	    (assert
	       (emtm
		  (pathtree->root tree)
		  (pathtree:th:make-derived-node :name "" :children
		     (list
			(pathtree:th:make-derived-node :name "a" :children nil :data
			   (eval 'cell)))
		     :data "default-data"))
	       t)
	    (pathtree:th:assert-name-dirtiness "" 'nil)
	    (pathtree:th:assert-name-dirtiness "a"
	       '(new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Empty tree")
	 (emt:doc "Operation: Add one element, path = a/b")
	 (emt:doc "Response: That element is in the second ply.")
	 (pathtree:th:let-usuals
	    ((cell "cell 1"))
	    (pathtree:th:add/replace-node tree
	       '("a" "b")
	       cell)
	    (pathtree:freshen tree)
	    (assert
	       (emtm
		  (pathtree->root tree)
		  (pathtree:th:make-derived-node :name "" :children
		     (list
			(pathtree:th:make-derived-node :name "a" :children
			   (list
			      (pathtree:th:make-derived-node :name "b" :children nil :data
				 (eval 'cell)))
			   :data "default-data"))
		     :data "default-data"))
	       t)
	    (pathtree:th:assert-name-dirtiness "" 'nil)
	    (pathtree:th:assert-name-dirtiness "a"
	       '(new))
	    (pathtree:th:assert-name-dirtiness "b"
	       '(new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Tree with one element, path a")
	 (emt:doc "Operation: Add an element beneath it, path a/b")
	 (emt:doc "Response: Now the tree has both elements in the expected topology.")
	 (pathtree:th:let-usuals
	    ((cell "cell 1")
	       (cell-b "cell 2"))
	    (pathtree:th:add/replace-node tree
	       '("a")
	       cell)
	    (pathtree:freshen tree)
	    (setq *nodes-freshened* 'nil)
	    (pathtree:th:add/replace-node tree
	       '("a" "b")
	       cell-b)
	    (pathtree:freshen tree)
	    (assert
	       (emtm
		  (pathtree->root tree)
		  (pathtree:th:make-derived-node :name "" :children
		     (list
			(pathtree:th:make-derived-node :name "a" :children
			   (list
			      (pathtree:th:make-derived-node :name "b" :children nil :data
				 (eval 'cell-b)))
			   :data
			   (eval 'cell)))
		     :data "default-data"))
	       t)
	    (pathtree:th:assert-name-dirtiness "a" 'nil)
	    (pathtree:th:assert-name-dirtiness "b"
	       '(new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Tree with one element, path a")
	 (emt:doc "Operation: Add a different element, same path")
	 (emt:doc "Response: That element replaces the old element.")
	 (pathtree:th:let-usuals
	    ((cell "cell 1")
	       (cell-a-2 "cell 2"))
	    (pathtree:th:add/replace-node tree
	       '("a")
	       cell)
	    (pathtree:freshen tree)
	    (setq *nodes-freshened* 'nil)
	    (pathtree:th:add/replace-node tree
	       '("a")
	       cell-a-2)
	    (pathtree:freshen tree)
	    (assert
	       (emtm
		  (pathtree->root tree)
		  (pathtree:th:make-derived-node :name "" :children
		     (list
			(pathtree:th:make-derived-node :name "a" :children nil :data
			   (eval 'cell-a-2)))
		     :data "default-data"))
	       t)
	    (pathtree:th:assert-name-dirtiness "" 'nil)
	    (pathtree:th:assert-name-dirtiness "a"
	       (list 'new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Tree with two elements, 
 * path a/b
 * path a/c (sibling)")
	 (emt:doc "Operation: Add a different element, same path as first.")
	 (emt:doc "Response: That element replaces the old element.  
The other element remains.")
	 (pathtree:th:let-usuals
	    ((cell "cell 1")
	       (cell-2 "cell 2")
	       (cell-3 "cell 3"))
	    (pathtree:th:add/replace-node tree
	       '("a" "b")
	       cell)
	    (pathtree:th:add/replace-node tree
	       '("a" "c")
	       cell-3)
	    (pathtree:freshen tree)
	    (setq *nodes-freshened* 'nil)
	    (pathtree:th:add/replace-node tree
	       '("a" "b")
	       cell-2)
	    (pathtree:freshen tree)
	    '(assert
		(emtm
		   (pathtree->root tree)
		   (pathtree:th:make-derived-node :name "" :children
		      (list
			 (pathtree:th:make-derived-node :name "a" :children
			    (list
			       (pathtree:th:make-derived-node :name "b" :children nil :data
				  (eval 'cell-2))
			       (pathtree:th:make-derived-node :name "c" :children nil :data
				  (eval 'cell-3)))
			    :data "default-data"))
		      :data "default-data"))
		t)
	    (pathtree:th:assert-name-dirtiness "a" 'nil)
	    (pathtree:th:assert-name-dirtiness "b"
	       (list 'new))
	    (pathtree:th:assert-name-dirtiness "c" 'nil)
	    t)))
   '(nil
       (progn
	  (emt:doc "Situation: Tree with one element, path a")
	  (emt:doc "Operation: Delete an element, same path.")
	  (emt:doc "Response: The tree is now empty.")
	  (pathtree:th:let-usuals
	     ((cell "cell 1"))
	     (pathtree:th:add/replace-node tree
		'("a")
		cell)
	     (pathtree:remove-node-recurse
		'("a"))
	     (pathtree:freshen tree)
	     '(assert)
	     t)))
   '(nil
       (progn
	  (emt:doc "Situation: Tree with one element, path a/b")
	  (emt:doc "Operation: Delete an element, same path")
	  (emt:doc "Response: The element is now gone but its parent remains.")
	  (pathtree:th:let-usuals
	     ((cell "cell 1"))
	     (pathtree:th:add/replace-node tree
		'("a" "b")
		cell)
	     (pathtree:remove-node-recurse
		'("a" "b"))
	     (pathtree:freshen tree)
	     (assert
		(emtm
		   (pathtree->root tree)
		   (pathtree:th:make-derived-node :name "" :children nil :data "default-data"))
		t)
	     t)))
   '(nil
       (progn
	  (emt:doc "Situation: Tree with two elements
 * path a/b
 * path a/c (sibling)")
	  (emt:doc "Operation: Delete an element, same path as first")
	  (emt:doc "Response: The element is now gone but its sibling and parent remain.")
	  (pathtree:th:let-usuals
	     ((cell "cell 1")
		(cell "cell 3"))
	     (pathtree:th:add/replace-node tree
		'("a" "b")
		cell)
	     (pathtree:th:add/replace-node tree
		'("a" "c")
		cell-3)
	     (pathtree:remove-node-recurse
		'("a" "b"))
	     (pathtree:freshen tree)
	     (assert
		(emtm
		   (pathtree->root tree)
		   (pathtree:th:make-derived-node :name "" :children
		      (list
			 (pathtree:th:make-derived-node :name "a" :children
			    (list
			       (pathtree:th:make-derived-node :name "c" :children nil :data
				  (eval 'cell-3)))
			    :data "default-data"))
		      :data "default-data"))
		t)
	     t)))
   '(nil
       (progn
	  (emt:doc "Situation: Tree with two elements
 * path a/b
 * path a/b/d (Child)")
	  (emt:doc "Operation: Delete an element, same path")
	  (emt:doc "Response: TO BE DECIDED.
Leaning towards
The element is now gone; only its parent remains.")
	  (pathtree:th:let-usuals
	     ((cell "cell 1"))
	     (pathtree:th:add/replace-node tree
		'("a" "b")
		cell)
	     (pathtree:th:add/replace-node tree
		'("a" "b" "d")
		cell)
	     (pathtree:remove-node-recurse
		'("a" "b"))
	     (pathtree:freshen tree)
	     '(assert)
	     t))))

(emt:deftest-3
   ((of 'pathtree:name-lessp))
   (nil
      (emt:assert
	 (eq
	    (pathtree:name-lessp "1" 2)
	    (not (pathtree:name-lessp 2 "1")))))
   (nil
      (emt:assert
	 (eq
	    (pathtree:name-lessp 1 2)
	    (not (pathtree:name-lessp 2 1))))))

;;;_. Footers
;;;_ , Provides

(provide 'utility/pathtree/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/pathtree/rtest.el ends here
