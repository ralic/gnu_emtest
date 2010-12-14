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

;;;_ , Inserters
;;;_  . outformat:insert:start-headline
(defun outformat:insert:start-headline (recurse-f depth &optional face)
   "Insert an outline headline prefix for DEPTH"
   (funcall recurse-f '(nl-if-none))
   (insert (make-string depth ?*) " "))

;;;_  . outformat:insert:end-headline
(defun outformat:insert:end-headline (recurse-f)
   "Insert an outline headline suffix"
   
   (insert "\n"))

;;;_  . outformat:insert:w/headline
(defun outformat:insert:w/headline (recurse-f args headline &rest body)
   "Insert an outline item, possibly containing other items.
ARGS format is undecided.
HEADLINE is a string and should not contain newline.
BODY is the rest of the item."
   (mapcar recurse-f
      (list
	 ;;$$FIXME Punt depth for now.
	 `(start-headline 1)
	 headline
	 '(end-headline)
	 '(sep 3)))
   ;;$$FIXME Must increase depth by 1 here
   (mapcar recurse-f body))

;;;_. Footers
;;;_ , Provides

(provide 'viewers/outformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/outformat.el ends here
