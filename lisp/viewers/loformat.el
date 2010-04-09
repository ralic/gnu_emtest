;;;_ viewers/loformat.el --- Low-level structured inserting

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
(eval-when-compile
   (require 'cl))
;;;_. Body
;;;_  . Types

;;$$RENAME ME
;;Was formatter:stage3
(deftype formatter:stage3 ()
     '(or
	 string
	 ;;First is property-list
	 (list* (member overlay) (repeat *) (repeat *))
	 (list (member nl-if-none))
	 (repeat formatter:stage3)))



;;;_  . emt:plain-viewer:insert
;;$$RENAME ME
;;$$USE ME
(defun emt:plain-viewer:insert (tree)
   "Print a format-tree into the current buffer.
TREE must be a format-tree.
The insert function is always `insert'."
   
   (cond
      ((consp tree)
	 (if (symbolp (car tree))
	    (case (car tree)
	       (overlay
		  (destructuring-bind (properties &rest r) (cdr tree)
		     (let
			(  ;;Remember where point was
			   (beg (set-marker (make-marker) (point))))
			
			;;Recurse
			(mapcar #'emt:plain-viewer:insert r)

			;;Now we know the text the overlay must cover,
			;;so we can create it.  Couldn't create it
			;;earlier and let it advance, because
			;;rear-advance must be nil.
			(let
			   ((ov
			       (make-overlay beg (point) nil nil nil)))
			   (dolist (prop properties)
			      (destructuring-bind (name value) prop
				 (overlay-put ov name value)))
			   ;;Now don't need the marker any more.
			   (set-marker beg nil)))))
	       
	       (nl-if-none
		  (unless (bolp) (insert "\n"))))
	    
	    (mapcar #'emt:plain-viewer:insert tree)))
      (t (insert tree))))

;;;_  . loformat functions
;;;_   , loformat:print
;;You can tell it's loformat because it inserts, rather than make a
;;format string.
(defun loformat:print (x)
   ""
   (typecase x
      (cons
	 (mapcar #'loformat:print x))
      (string
	 (insert x))))

;;;_. Footers
;;;_ , Provides

(provide 'viewers/loformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/loformat.el ends here
