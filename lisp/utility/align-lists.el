;;;_ utility/align-lists.el --- Align two lists for minimum edit distance

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp

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

(require 'heap)

;;;_. Body
;;;_ , Type
(defstruct (align-lists:candidate
	      (:conc-name align-lists:candidate->)
	      (:constructor align-lists:make-candidate))
   
   "A candidate for best alignment"
   edit-distance
   edits
   a
   b)


;;;_ , Comparing priority
(defun align-lists:closer (a-el b-el)
   ""
   (< 
      (align-lists:candidate->edit-distance a-el)
      (align-lists:candidate->edit-distance b-el)))

;;;_ , align-lists:new-candidate

(defun align-lists:adapt-field (accessor oobj form)
   ""
   (subst
      (list accessor oobj)
      '-old-
      (copy-tree form)))

(defmacro* align-lists:adapt-candidate (oobj &key edit-distance edits a b)
   "Construct an align-lists:candidate object adapted from CAND.
Syntax is almost that of a ctor, but in each form, the symbol `-old-'
is replaced by the value of the respective field of OOBJ."

   `(align-lists:make-candidate
       :edit-distance 
       ,(align-lists:adapt-field 
	   'align-lists:candidate->edit-distance 
	   oobj
	   edit-distance)
       :edits
       ,(align-lists:adapt-field
	   'align-lists:candidate->edits 
	   oobj
	   edits)
       :a
       ,(align-lists:adapt-field
	   'align-lists:candidate->a 
	   oobj
	   a)
       :b 
       ,(align-lists:adapt-field
	   'align-lists:candidate->b 
	   oobj
	   b)))


;;;_ , align-lists

(defun align-lists (a b eq-f)
   "Return the best alignment of two lists

EQ-F should be a predicate that takes two arguments and returns
whether they match.

The elements of the returned list are all of the form:
 * (a ELEMENT)
 * (b ELEMENT)
 * (both ELEMENT-OF-A ELEMENT-OF-B)"

   (let
      ((align-lists:heap 
	  (heap-create #'align-lists:closer)))
      (heap-add
	 align-lists:heap
	 (align-lists:make-candidate
	    :edit-distance 0
	    :edits '()
	    :a a
	    :b b))
      
      (catch 'align-lists:answer
	 (while t
	    (let*
	       (  (cand
		     (heap-delete-root align-lists:heap))
		  (a (align-lists:candidate->a cand))
		  (b (align-lists:candidate->b cand)))

	       ;;If no edits can be made, we're done.  This is because
	       ;;we always operate on the most promising candidate, so
	       ;;we know there is no better solution (Though there may
	       ;;be an equally good one).
	       (unless (or a b)
		  (throw 'align-lists:answer 
		     (nreverse
			(align-lists:candidate->edits cand))))

	       ;;If they match, accept from both lists
	       (if
		  (and a b (funcall eq-f (car a)(car b)))
		  (heap-add
		     align-lists:heap
		     (align-lists:adapt-candidate cand
			:edit-distance 
			-old-
			:edits
			(cons (list 'both (car a)(car b)) -old-)
			:a
			(cdr -old-)
			:b
			(cdr -old-)))

		  ;;Otherwise make multiple candidates, which accept
		  ;;from one or from the other.
		  (progn
		     (when a
			;;Don't proceed if most recent edit is `b',
			;;because '((b..)(a..)) means the same as
			;;'((a..)(b..)), which we generate by another
			;;path.  Ie, we generate all the a's before
			;;any b's that they would commute with.
			(unless
			   (eq
			      (car (car (align-lists:candidate->edits cand)))
			      'b)
			   (heap-add
			      align-lists:heap
			      (align-lists:adapt-candidate cand
				 :edit-distance 
				 (+ -old- 1)
				 :edits
				 (cons (list 'a (car a)) -old-)
				 :a
				 (cdr -old-)
				 :b
				 -old-))))

		     (when b
			(heap-add
			   align-lists:heap
			   (align-lists:adapt-candidate cand
			      :edit-distance 
			      (+ -old- 1)
			      :edits
			      (cons (list 'b (car b)) -old-)
			      :a
			      -old-
			      :b
			      (cdr -old-))))))
	       )))))



;;;_. Footers
;;;_ , Provides

(provide 'utility/align-lists)

;;;_ * Local emacs vars.

;;Do not remove `no-byte-compile', it avoids a byte-compiler bug.

;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + no-byte-compile: t
;;;_  + End:

;;;_ , End
;;; utility/align-lists.el ends here
