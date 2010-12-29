;;;_ emtest/viewer/view-types.el --- Viewable object types for Emtest

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
(require 'emtest/common/run-types)
(require 'utility/pathtree)
(eval-when-compile
   (require 'cl))
;;;_. Body
;;;_ , emtvr:suite-newstyle->id
(deftype emtvr:suite-newstyle->id ()
   "The ID of a reported suite.  In fact a how-to-run object"
   'emthow)


;;;_ , Presentables
;;$$RENAME US Use prefix emtvr:
;;;_  . Base 

;;Emviewer uses this as the content element in pathtree nodes.
(defstruct (emt:view:presentable
	      (:constructor emt:view:make-presentable)
	      (:conc-name emt:view:presentable->)
	      (:include emtvp:node))
   ""
   ;;Summarized badnesses from all subtrees.  They are summarized
   ;;treewise, including any badnesses from this node.
   (sum-grades () :type (repeat emt:testral:grade))
   ;;This relates to a display
   (list ()))

;;;_  . Suite in tree (as by emviewer)
(defstruct (emt:view:suite-newstyle
	      (:constructor emt:view:make-suite-newstyle)
	      (:conc-name emt:view:suite-newstyle->)
	      (:include emt:view:presentable))
   ""
   ;;Just for suite nodes.
   (id () :type emtvr:suite-newstyle->id
      :doc "The \"official\" id.")

   (how-to-run ():type emtt:explorable
      :doc "How to run this as a test.")

   ;;Where it lives in the pathtree, so that we can put new results in
   ;;the same place.
   (presentation-path ()   
      :type emt:testral:suite-id
      :doc "The path to the corresponding node in the pathtree.")

   ;;This is becoming more of a datestamp/circumstance info.
   (testrun-id ()
      :type emt:testral:testrun-id
      :doc "The ID of the testrun that created this result.")
   (result ()     
      :type (or null 
	       emt:testral:suite 
	       emt:testral:test-runner-info)
      :doc "The result data itself"))


;;;_  . Notes in viewable form.
(defstruct (emt:view:note
	    (:constructor emt:view:make-note)
	    (:conc-name emt:view:note->)
	      (:include emt:view:presentable))
   ""
   (contents () :type emt:testral:newstyle))

;;;_  . Note placeholder
(defstruct (emt:view:note-placeholder
	    (:constructor emt:view:make-note-placeholder)
	    (:conc-name emt:view:note-placeholder->)
	      (:include emt:view:presentable))
   "A blank note-like placeholder, not associated with a TESTRAL note")


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/view-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/view-types.el ends here
