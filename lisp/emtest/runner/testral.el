;;;_ emtest/runner/testral.el --- Testral functions for emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp, maint

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

;;;_ , TESTRAL functions
;;;_  . emt:testral:create
(defsubst emt:testral:create ()
   ""
   ())

;;;_  . emt:testral:add-note
;;Must "note" be a `emt:testral:base'?
(defun emt:testral:add-note (note &optional tags arglist)
   ""
   (when
      (boundp 'emt:testral:*events-seen*)
      ;;Later, tags will inform a report-manager, which also checks
      ;;whether to add notes.


      ;;Later, for "call" tags, arglist will be processed wrt objects
      ;;whose origin is known.  This relates to the
      ;;`emt:result:diag:call' type, but the design may change.
      (push
	 note
	 emt:testral:*events-seen*
	 )))

;;;_  . emt:testral:set-object-origin
(defun emt:testral:set-object-origin (object origin)
   ""

   ;;Punt for now.  Later, store its identity on some sort of alist.
   (let*
      ()
      
      ))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/testral)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/testral.el ends here
