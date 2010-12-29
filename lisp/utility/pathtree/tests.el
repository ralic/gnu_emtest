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
(require 'emtest/runner/define)
(require 'utility/pathtree)
(require 'utility/pathtree/testhelp)

(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/match)
(require 'utility/pending)

;;;_. Body
;;;_ , Helper
;;;_   , emtvp:add/replace-node
'  ;;$$OBSOLETE
(put 'emtvp:add/replace-node 'emt:test-thru 'emtvp:add/replace-node-recurse)


;;;_ , emtvp:add/replace-node-recurse
'  ;;$$OBSOLETE
(emt:deftest-3 emtvp:add/replace-node-recurse
   (nil
      (progn
	 (emt:doc "Proves that `emtvp:th:callback:push' terminates.")
	 (emtvp:th:let-usuals nil
	    (assert
	       (pending:terminates-on-examples #'emtvp:th:callback:push nil
		  (emtvp:th:make-derived-node :name "a" :dirty-flags
		     '(new))))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Dirty list has members.")
	 (emt:doc "Operation: Freshen the tree.")
	 (emt:doc "Result: Dirty list is now empty.")
	 (emtvp:th:let-usuals nil
	    (push
	       (emtvp:th:make-derived-node :name "a" :dirty-flags
		  '(new))
	       (emtvp->dirty tree))
	    (emtvp:freshen tree)
	    (assert
	       (null
		  (emtvp->dirty tree))
	       t)
	    (assert
	       (equal
		  (emtvp:th:how-dirty "a")
		  '(new))
	       t)
	    (emtvp:th:assert-name-dirtiness "a"
	       '(new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Empty tree")
	 (emt:doc "Operation: Add one element, path = a")
	 (emt:doc "Response: That element is in the first ply.")
	 (emtvp:th:let-usuals
	    ((cell "cell 1"))
	    (emtvp:add/replace-node tree
	       '("a")
	       cell)
	    (emtvp:freshen tree)
	    (assert
	       (emtm
		  (emtvp->root tree)
		  (emtvp:th:make-derived-node :name "" :children
		     (list
			(emtvp:th:make-derived-node :name "a" :children nil :data
			   (eval 'cell)))
		     :data "default-data"))
	       t)
	    (emtvp:th:assert-name-dirtiness "" 'nil)
	    (emtvp:th:assert-name-dirtiness "a"
	       '(new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Empty tree")
	 (emt:doc "Operation: Add one element, path = a/b")
	 (emt:doc "Response: That element is in the second ply.")
	 (emtvp:th:let-usuals
	    ((cell "cell 1"))
	    (emtvp:add/replace-node tree
	       '("a" "b")
	       cell)
	    (emtvp:freshen tree)
	    (assert
	       (emtm
		  (emtvp->root tree)
		  (emtvp:th:make-derived-node :name "" :children
		     (list
			(emtvp:th:make-derived-node :name "a" :children
			   (list
			      (emtvp:th:make-derived-node :name "b" :children nil :data
				 (eval 'cell)))
			   :data "default-data"))
		     :data "default-data"))
	       t)
	    (emtvp:th:assert-name-dirtiness "" 'nil)
	    (emtvp:th:assert-name-dirtiness "a"
	       '(new))
	    (emtvp:th:assert-name-dirtiness "b"
	       '(new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Tree with one element, path a")
	 (emt:doc "Operation: Add an element beneath it, path a/b")
	 (emt:doc "Response: Now the tree has both elements in the expected topology.")
	 (emtvp:th:let-usuals
	    ((cell "cell 1")
	       (cell-b "cell 2"))
	    (emtvp:add/replace-node tree
	       '("a")
	       cell)
	    (emtvp:freshen tree)
	    (setq *nodes-freshened* 'nil)
	    (emtvp:add/replace-node tree
	       '("a" "b")
	       cell-b)
	    (emtvp:freshen tree)
	    (assert
	       (emtm
		  (emtvp->root tree)
		  (emtvp:th:make-derived-node :name "" :children
		     (list
			(emtvp:th:make-derived-node :name "a" :children
			   (list
			      (emtvp:th:make-derived-node :name "b" :children nil :data
				 (eval 'cell-b)))
			   :data
			   (eval 'cell)))
		     :data "default-data"))
	       t)
	    (emtvp:th:assert-name-dirtiness "a" 'nil)
	    (emtvp:th:assert-name-dirtiness "b"
	       '(new))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Tree with one element, path a")
	 (emt:doc "Operation: Add a different element, same path")
	 (emt:doc "Response: That element replaces the old element.")
	 (emtvp:th:let-usuals
	    ((cell "cell 1")
	       (cell-a-2 "cell 2"))
	    (emtvp:add/replace-node tree
	       '("a")
	       cell)
	    (emtvp:freshen tree)
	    (setq *nodes-freshened* 'nil)
	    (emtvp:add/replace-node tree
	       '("a")
	       cell-a-2)
	    (emtvp:freshen tree)
	    (assert
	       (emtm
		  (emtvp->root tree)
		  (emtvp:th:make-derived-node :name "" :children
		     (list
			(emtvp:th:make-derived-node :name "a" :children nil :data
			   (eval 'cell-a-2)))
		     :data "default-data"))
	       t)
	    (emtvp:th:assert-name-dirtiness "" 'nil)
	    (emtvp:th:assert-name-dirtiness "a"
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
	 (emtvp:th:let-usuals
	    ((cell "cell 1")
	       (cell-2 "cell 2")
	       (cell-3 "cell 3"))
	    (emtvp:add/replace-node tree
	       '("a" "b")
	       cell)
	    (emtvp:add/replace-node tree
	       '("a" "c")
	       cell-3)
	    (emtvp:freshen tree)
	    (setq *nodes-freshened* 'nil)
	    (emtvp:add/replace-node tree
	       '("a" "b")
	       cell-2)
	    (emtvp:freshen tree)
	    '(assert
		(emtm
		   (emtvp->root tree)
		   (emtvp:th:make-derived-node :name "" :children
		      (list
			 (emtvp:th:make-derived-node :name "a" :children
			    (list
			       (emtvp:th:make-derived-node :name "b" :children nil :data
				  (eval 'cell-2))
			       (emtvp:th:make-derived-node :name "c" :children nil :data
				  (eval 'cell-3)))
			    :data "default-data"))
		      :data "default-data"))
		t)
	    (emtvp:th:assert-name-dirtiness "a" 'nil)
	    (emtvp:th:assert-name-dirtiness "b"
	       (list 'new))
	    (emtvp:th:assert-name-dirtiness "c" 'nil)
	    t)))
   '(nil
       (progn
	  (emt:doc "Situation: Tree with one element, path a")
	  (emt:doc "Operation: Delete an element, same path.")
	  (emt:doc "Response: The tree is now empty.")
	  (emtvp:th:let-usuals
	     ((cell "cell 1"))
	     (emtvp:add/replace-node tree
		'("a")
		cell)
	     (emtvp:remove-node-recurse
		'("a"))
	     (emtvp:freshen tree)
	     '(assert)
	     t)))
   '(nil
       (progn
	  (emt:doc "Situation: Tree with one element, path a/b")
	  (emt:doc "Operation: Delete an element, same path")
	  (emt:doc "Response: The element is now gone but its parent remains.")
	  (emtvp:th:let-usuals
	     ((cell "cell 1"))
	     (emtvp:add/replace-node tree
		'("a" "b")
		cell)
	     (emtvp:remove-node-recurse
		'("a" "b"))
	     (emtvp:freshen tree)
	     (assert
		(emtm
		   (emtvp->root tree)
		   (emtvp:th:make-derived-node :name "" :children nil :data "default-data"))
		t)
	     t)))
   '(nil
       (progn
	  (emt:doc "Situation: Tree with two elements
 * path a/b
 * path a/c (sibling)")
	  (emt:doc "Operation: Delete an element, same path as first")
	  (emt:doc "Response: The element is now gone but its sibling and parent remain.")
	  (emtvp:th:let-usuals
	     ((cell "cell 1")
		(cell "cell 3"))
	     (emtvp:add/replace-node tree
		'("a" "b")
		cell)
	     (emtvp:add/replace-node tree
		'("a" "c")
		cell-3)
	     (emtvp:remove-node-recurse
		'("a" "b"))
	     (emtvp:freshen tree)
	     (assert
		(emtm
		   (emtvp->root tree)
		   (emtvp:th:make-derived-node :name "" :children
		      (list
			 (emtvp:th:make-derived-node :name "a" :children
			    (list
			       (emtvp:th:make-derived-node :name "c" :children nil :data
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
	  (emtvp:th:let-usuals
	     ((cell "cell 1"))
	     (emtvp:add/replace-node tree
		'("a" "b")
		cell)
	     (emtvp:add/replace-node tree
		'("a" "b" "d")
		cell)
	     (emtvp:remove-node-recurse
		'("a" "b"))
	     (emtvp:freshen tree)
	     '(assert)
	     t))))

;;;_. Footers
;;;_ , Provides

(provide 'utility/pathtree/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/pathtree/rtest.el ends here
