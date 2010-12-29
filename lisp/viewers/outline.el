;;;_ viewers/outline.el --- Outline formatting

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp,outlines

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



;;;_. Body
;;;_  . Outlining
;;$$MOVE ME later when we have dynamic variable registration sorted out.
;;;_   , Special variables
(declare (special emtvf:*outline-depth* emtvf:*folded*))
(eval-after-load 'utility/dynvars
   '(progn
      (utidyv:register-var 'emtvf:*outline-depth* 0)
      (utidyv:register-var 'emtvf:*folded* nil)))

;;;_   , emtvf:outline-item-f
(defun emtvf:outline-item-f (depth face headtext contents &optional fold)
   "Make an outline item of DEPTH."
   `(
       (sep 3)
       (w/face ,(make-string depth ?*) ,face)
       " " 
       ,headtext
       ;;The heading terminator is made part of contents in order to
       ;;accord with outline-cycle's understanding of folded items.
       ,(cond
	   ((null contents) nil)
	   (fold
	      `(overlay (invisible outline) (sep 2) ,contents))
	   (t
	      `((sep 2) ,contents)))
       (sep 2)))

(defmacro emtvf:outline-item (headtext contents &optional face fold)
   "Make an outline item.
HEADTEXT gives the heading and CONTENTS as contents.
FACE is the face to display the heading in.
If FOLD is non-nil, fold that contents."
   (let
      (  (contents-sym  (make-symbol "contents"))
	 (fold-now      (make-symbol "fold-now"))
	 (new-depth     (make-symbol "new-depth")))
      
      `(let*
	  (  (,new-depth (1+ emtvf:*outline-depth*))
	     ;;Don't re-fold if this item is inside a folded item.
	     (,fold-now (and ,fold (not emtvf:*folded*)))
	     (,contents-sym
		(let
		   (  (emtvf:*outline-depth* ,new-depth)
		      (emtvf:*folded* (or emtvf:*folded* ,fold-now)))
		   (declare (special emtvf:*outline-depth* emtvf:*folded*))
		   ,contents)))
	  (emtvf:outline-item-f ,new-depth ,face ,headtext
	     ,contents-sym ,fold-now))))


;;;_. Footers
;;;_ , Provides

(provide 'viewers/outline)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/outline.el ends here
