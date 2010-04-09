;;;_ viewers/outformat.el --- Outline-style formatting

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, outlines, internal

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

;; Outline formatting support

;;;_ , Requires
(eval-when-compile
   (require 'cl))

;;;_. Body

;;;_  . Type
;;Using this requires deep-type-checker.el for `list' with args.
(deftype formatter:stage2 ()
   '(or
       string
       (list (member sep) integer)

       ;;(repeat formatter:stage2) ;;List, replacing `sequence'
       (list* (member sequence) (repeat *))
       ;;First is property-list suitable for next stage.
       (list* (member section-x) (repeat *) (repeat *))
       (list (member start-headline) integer (satisfies facep))
       (list (member end-headline))))

;;;_  . emt:plain-viewer:stage2->stage3
(defun emt:plain-viewer:stage2->stage3 (tree)
   ""
   
   (cond
      ((consp tree)
	 (case (car tree)
	    ;;$$OBSOLETE I've since redesigned this - lists that don't
	    ;;start with a symbol are just recursed on.
	    (sequence
	       (mapcar #'emt:plain-viewer:stage2->stage3
		  (cdr tree)))
	    (section-x
	       `(overlay ,(cadr tree)
		   ,(mapcar #'emt:plain-viewer:stage2->stage3
		       (cddr tree))))
	    (sep
	       (case (cadr tree)
		  (0 " ")
		  (1 " - ")
		  (2 '(nl-if-none))
		  (3 "\n")
		  (4 '((nl-if-none) "\n"))
		  (5 (list '(nl-if-none) (make-string 20 ?-)"\n"))
		  (6 (list '(nl-if-none) "\n" (make-string 40 ?*)"\n\n"))))
	    (start-headline
	       (list
		  (apply
		     #'(lambda (depth &optional face)
			  (list "\n" (make-string depth ?*)))
		     (cdr tree))
		  " "))
	    (end-headline "\n")))
      
      (t tree)))


;;;_. Footers
;;;_ , Provides

(provide 'viewers/outformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/outformat.el ends here
