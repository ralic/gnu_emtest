;;;_ rtest.el --- Tests for empathtree

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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

(require 'emtest/viewer/empathtree)

;;;_. Body

;;;_ , emtvr:sum-node-badnesses
(rtest:deftest emtvr:sum-node-badnesses

   ;;Given a tree, compare result to expected example.
   ;;Summary of the one-error example gets propagated upwards.

   ;;Even if the TESTRAL notes haven't been expanded yet.

   ;;Looking at a tree with two errors, from a corresponding report.
   ;;Summary combines them at the appropriate spot.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   
   )
;;;_ , emtvr:conform-stages
(rtest:deftest emtvr:conform-stages
   ;;Given the note-list, transform it to the conformed version
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   
   )
;;;_ , emtvr:expand-testral
(rtest:deftest emtvr:expand-testral
   ;;Given a view testral node of one level, expand it.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )

   ;;(Later) Different function: Recursively to the leaves
   ;;(Later, or YAGNI) As far as an expansion policy says to.
   
   )



;;;_. Footers
;;;_ , Provides

(provide 'rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; rtest.el ends here
