;;;_ pathtree/tests.el --- Tests for pathtree

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(require 'pathtree/testhelp)
;;;_. Body
;;;_ , Helper

;;;_  . emt:pathtree:th:callback:push
(defun emt:pathtree:th:callback:push (x)
   "Callback for testing pathtree.
Store data about X on the list `*nodes-freshened*'.  Then clean up
   like a normal callback would must."
   (check-type x emt:view:pathtree-node)
   (push  
      (list
	 (emt:view:pathtree-node-name x)
	 (emt:view:pathtree-node-data x)
	 (emt:view:pathtree-node-dirty-flags x))
      *nodes-freshened*)
   ;;Wipe out previous dirty-flags in case we are interested in later
   ;;operations. 
   (setf (emt:view:pathtree-node-dirty-flags x) '())

   ;;Return the empty list, indicating that there are no forther
   ;;operations to perform.
   ())
;;;_  . emt:pathtree:th:how-dirty
;;Usage: (assert (equal (emt:pathtree:th:how-dirty Name) Expected) t)
;;Usage: (assert (emt:match (emt:pathtree:th:how-dirty Name) Expected) t)

;;But this can't be easily used when matching patterns.  Maybe if we
;;sort flags. 
(defun emt:pathtree:th:how-dirty (name)
   ""
   
   ;;Mapcar, giving applicable dirty-flags.  Empty list for other names.
   ;;No need to delete the empty lists.
   ;;Union the dirty-flags of `name'.  Ie, reduce them by union.
   (let*
      ((total-flags
	  (mapcar
	     #'(lambda (node)
		  (if
		     (string= name (car node))
		     (third node)
		     '()))

	     *nodes-freshened*)))
      (if total-flags
	 (reduce #'union total-flags)
	 '())))

;;;_   , Tests (of this helper)
;;See below
;;;_  . emt:pathtree:th:assert-name-dirtiness
(defmacro emt:pathtree:th:assert-name-dirtiness (name pattern)
   ""
   
   `(let
       ((how-dirty
	   (emt:pathtree:th:how-dirty ,name)))
       (assert 
	  (emt:match how-dirty ,pattern)
	  nil "Mismatch %S has %s" ,name how-dirty)))


;;;_  . emt:pathtree:th:let*-usuals
(defmacro emt:pathtree:th:let*-usuals (other-lets &rest body)
   ""
   
   `(let*
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    ,@other-lets)
       ,@body))


;;;_ , Tests

(rtest:deftest emt:pathtree:add/replace-node-recurse
   
   
   (  "Proves that `emt:pathtree:th:callback:push' terminates."
      (emt:pathtree:th:let*-usuals ()
	 (assert
	    (pending:terminates-on-examples
	       #'emt:pathtree:th:callback:push
	       ()
	       (make-emt:view:pathtree-node
		  :name "a"
		  :dirty-flags '(new))))
	 t))
   
   ;;Really a test of `emt:pathtree:freshen'
   (  "Situation: Dirty list has members.  
Operation: Freshen the tree.
Result: Dirty list is now empty."
      (emt:pathtree:th:let*-usuals ()
	 
	 ;;How to guarantee it has members while still obeying its
	 ;;interface?
	 (push
	    (make-emt:view:pathtree-node
	       :name "a"
	       :dirty-flags '(new))
	    (emt:view:pathtree-dirty tree))
	 (emt:pathtree:freshen tree)
	 ;;The dirty list is now empty
	 (assert
	    (null (emt:view:pathtree-dirty tree))
	    t)

	 ;;Gives the expected list of dirty flags.
	 (assert (equal (emt:pathtree:th:how-dirty "a") '(new)) 
	    t)

	 (emt:pathtree:th:assert-name-dirtiness "a" '(new))
	 
	 t))
   

   ;;Testing strategy:
   ;;Add with known paths.  
   ;;Tree structure should match what's expected.
   ;;Callback does nothing.
   ;;Matches do not look for full-id because that might go away.

   ;;Check that callbacks are as expected (Now it's check that dirty
   ;;nodes are as expected).  We sort the list for unvarying
   ;;comparison.


   (  "Situation: Empty tree
Operation: Add one element, path = a
Response: That element is in the first ply."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1"))
	 (emt:pathtree:add/replace-node
	    tree '("a") cell)
	 
	 (emt:pathtree:freshen tree)
	 ;;Matches expected tree
	 (assert
	    (emt:match
	       (emt:view:pathtree-root tree)
	       (make-emt:view:pathtree-node
		  :name ""
		  :children 
		  (list
		     (make-emt:view:pathtree-node
			:name "a"
			:children ()
			:data (eval 'cell)))
		  :data "default-data"))
	    t)
	 
	 ;;Root "" has new child
	 (emt:pathtree:th:assert-name-dirtiness ""
	    '())
	 ;;Node "a" is new
	 (emt:pathtree:th:assert-name-dirtiness "a" 
	    '(new))
	 t))
   
   ;;Add an element 2 plies down.
   (  "Situation: Empty tree
Operation: Add one element, path = a/b
Response: That element is in the second ply."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1"))
	 (emt:pathtree:add/replace-node
	    tree '("a" "b") cell)

	 (emt:pathtree:freshen tree)
	 ;;Matches expected tree
	 (assert
	    (emt:match
	       (emt:view:pathtree-root tree)
	       (make-emt:view:pathtree-node
		  :name ""
		  :children 
		  (list
		     (make-emt:view:pathtree-node
			:name "a"
			:children 
			(list
			   (make-emt:view:pathtree-node
			      :name "b"
			      :children ()
			      :data (eval 'cell)))
			
			:data "default-data"))
		  :data "default-data"))
	    t)

	 ;;Root (grandparent) has updated child
 	 (emt:pathtree:th:assert-name-dirtiness "" '())
	 
	 (emt:pathtree:th:assert-name-dirtiness "a"
	    '(new))
	 (emt:pathtree:th:assert-name-dirtiness "b"
	    '(new))

	 t))

   ;;Add an element below another element
   (  "Situation: Tree with one element, path a
Operation: Add an element beneath it, path a/b
Response: Now the tree has both elements in the expected topology."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1")
	    (cell-b "cell 2"))
	 (emt:pathtree:add/replace-node
	    tree '("a") cell)

	 (emt:pathtree:freshen tree)
	 (setq *nodes-freshened* '())
	 (emt:pathtree:add/replace-node
	    tree '("a" "b") cell-b)

	 (emt:pathtree:freshen tree)
	 ;;Matches expected tree
	 (assert
	    (emt:match
	       (emt:view:pathtree-root tree)
	       (make-emt:view:pathtree-node
		  :name ""
		  :children 
		  (list
		     (make-emt:view:pathtree-node
			:name "a"
			:children 
			(list
			   (make-emt:view:pathtree-node
			      :name "b"
			      :children ()
			      :data (eval 'cell-b)))
			:data (eval 'cell)))
		  :data "default-data"))
	    t)
	 
	 ;;The element "a" has a new child
	 (emt:pathtree:th:assert-name-dirtiness "a"
	    '())
	 ;;Node "b" is new
	 (emt:pathtree:th:assert-name-dirtiness "b"
	    '(new))
	 t))


   ;;Replace an element.
   (  "Situation: Tree with one element, path a
Operation: Add a different element, same path
Response: That element replaces the old element."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1")
	    (cell-a-2 "cell 2"))
	 (emt:pathtree:add/replace-node
	    tree '("a") cell)

	 (emt:pathtree:freshen tree)
	 (setq *nodes-freshened* '())
	 (emt:pathtree:add/replace-node
	    tree '("a") cell-a-2)

	 (emt:pathtree:freshen tree)
	 ;;Matches expected tree
	 (assert
	    (emt:match
	       (emt:view:pathtree-root tree)
	       (make-emt:view:pathtree-node
		  :name ""
		  :children 
		  (list
		     (make-emt:view:pathtree-node
			:name "a"
			:children ()
			:data (eval 'cell-a-2)))
		  :data "default-data"))
	    t)

	 ;;Root element "" has updated child
 	 (emt:pathtree:th:assert-name-dirtiness "" '())
	 ;;Node "a" has replaced its data
	 (emt:pathtree:th:assert-name-dirtiness "a"
	    (list
	       (list 'replaced x y)))

	 t))

   (  "Situation: Tree with two elements, 
 * path a/b
 * path a/c (sibling)
Operation: Add a different element, same path as first.
Response: That element replaces the old element.  
The other element remains."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1")
	    (cell-2 "cell 2")
	    (cell-3 "cell 3"))
	 (emt:pathtree:add/replace-node
	    tree '("a" "b") cell)
	 (emt:pathtree:add/replace-node
	    tree '("a" "c") cell-3)

	 (emt:pathtree:freshen tree)
	 (setq *nodes-freshened* '())
	 (emt:pathtree:add/replace-node
	    tree '("a" "b") cell-2)

	 (emt:pathtree:freshen tree)
	 ;;Matches expected tree, including cell-3 being in place.
	 (assert
	    (emt:match
	       (emt:view:pathtree-root tree)
	       (make-emt:view:pathtree-node
		  :name ""
		  :children 
		  (list
		     (make-emt:view:pathtree-node
			:name "a"
			:children 
			;;$$ADD SUPPORT This requires set matching
			;;(ie, unordered), not list matching.
			(list
			   (make-emt:view:pathtree-node
			      :name "b"
			      :children ()
			      :data (eval 'cell-2))
			   (make-emt:view:pathtree-node
			      :name "c"
			      :children ()
			      :data (eval 'cell-3))
			   )
			:data "default-data"))
		  :data "default-data"))
	    t)

	 ;;The inner node "a" has a new child.
 	 (emt:pathtree:th:assert-name-dirtiness "a" '())
	 ;;Node "b"'s data has been replaced
	 (emt:pathtree:th:assert-name-dirtiness "b"
	    (list
	       ;;$$SUPPORTME would be nice to have anonymous
	       ;;variables in emt-match
	       (list 'replaced y z)))
 	 (emt:pathtree:th:assert-name-dirtiness "c" '())

	 t))
   
   ;;Delete an element
   ;;Punt for now.  Not needed until we're much further along.
   '
   (  "Situation: Tree with one element, path a
Operation: Delete an element, same path.
Response: The tree is now empty."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1"))
	 (emt:pathtree:add/replace-node
	    tree '("a") cell)
	 (emt:pathtree:remove-node-recurse '("a"))
	 (emt:pathtree:freshen tree)
	 ;;Matches expected tree - empty
	 '(assert)
	 t))
   
   '
   (  "Situation: Tree with one element, path a/b
Operation: Delete an element, same path
Response: The element is now gone but its parent remains."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1"))
	 (emt:pathtree:add/replace-node
	    tree '("a" "b") cell)

	 (emt:pathtree:remove-node-recurse '("a" "b"))
	 (emt:pathtree:freshen tree)
	 (assert
	    (emt:match
	       (emt:view:pathtree-root tree)
	       (make-emt:view:pathtree-node
		  :name ""
		  :children ()
		  :data "default-data"))
	    t)
	 t))
   
   
   ;;Sibling
   '
   (  "Situation: Tree with two elements
 * path a/b
 * path a/c (sibling)
Operation: Delete an element, same path as first
Response: The element is now gone but its sibling and parent remain."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1")
	    (cell "cell 3"))
	 (emt:pathtree:add/replace-node
	    tree '("a" "b") cell)
	 (emt:pathtree:add/replace-node
	    tree '("a" "c") cell-3)

	 (emt:pathtree:remove-node-recurse '("a" "b"))
	 (emt:pathtree:freshen tree)

	 ;;Matches expected tree
	 (assert
	    (emt:match
	       (emt:view:pathtree-root tree)
	       (make-emt:view:pathtree-node
		  :name ""
		  :children 
		  (list
		     (make-emt:view:pathtree-node
			:name "a"
			:children 
			(list
			   (make-emt:view:pathtree-node
			      :name "c"
			      :children ()
			      :data (eval 'cell-3)))
			
			:data "default-data"))
		  :data "default-data"))
	    t)
	 t))

   ;;Descendants - Correct behavior is TBD.
   '
   (  "Situation: Tree with two elements
 * path a/b
 * path a/b/d (Child)
Operation: Delete an element, same path
Response: TO BE DECIDED.
Leaning towards
The element is now gone; only its parent remains."
      (let* 
	 (  (*nodes-freshened* '())
	    (tree 
	       (emt:pathtree:make-empty-tree-newstyle
		  #'emt:pathtree:th:callback:push
		  #'(lambda() "default-data") 
		  'string))
	    (cell "cell 1"))
	 (emt:pathtree:add/replace-node
	    tree '("a" "b") cell)
	 (emt:pathtree:add/replace-node
	    tree '("a" "b" "d") cell)
	 (emt:pathtree:remove-node-recurse '("a" "b"))

	 (emt:pathtree:freshen tree)
	 ;;Matches expected tree
	 '(assert)
	 t))


   ;;PROBABLY NOT these tests:
   
   ;;Tests with alists.
   ;;Build on the alist tests, but now check trees and alist, and that
   ;;trees point to entries on alist as expected.
   ;;Alist gets callbacks that deals with trees and does nothing else.


   ;;Check display, using simple formatting and controlled buffers.  And
   ;;a simpler data type?

   )
;;;_. Footers
;;;_ , Provides

(provide 'pathtree/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; pathtree/tests.el ends here
