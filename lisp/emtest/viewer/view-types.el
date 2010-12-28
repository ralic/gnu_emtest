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
(require 'emtest/common/result-types)
(require 'utility/pathtree)
(eval-when-compile
   (require 'cl))
;;;_. Body
;;;_ , emtvr:suite-newstyle->id
;;$$OBSOLESCENT Being rethought
(deftype emtvr:suite-newstyle->id ()
   "Can be a UUID string or a how-to-run object"
   '(or string emthow))


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
   (sum-badnesses () :type (repeat emt:testral:grade))
   ;;This relates to a display
   (list ()))

;;;_  . Suite in tree (as by emviewer)
(defstruct (emt:view:suite-newstyle
	      (:constructor emt:view:make-suite-newstyle)
	      (:conc-name emt:view:suite-newstyle->)
	      (:include emt:view:presentable))
   ""
   ;;Just for suite nodes.
   ;;$$REMOVE ME later
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


;;;_  . TESTRAL in viewable form.
;;$$OBSOLETE
(defstruct (emt:view:TESTRAL
	    (:constructor nil)
	    (:conc-name emt:view:TESTRAL->)
	      (:include emt:view:presentable))
   ""
   (content () :type (repeat emt:testral:newstyle)))
(defun emt:view:make-TESTRAL (&rest args)
   (error "Obsolete emt:view:make-TESTRAL"))
;;;_  . TESTRAL in viewable form.
(defstruct (emt:view:TESTRAL-2
	    (:constructor emt:view:make-TESTRAL-2)
	    (:conc-name emt:view:TESTRAL-2->)
	      (:include emt:view:presentable))
   ""
   (contents () :type emt:testral:newstyle))

;;;_  . Note placeholder
;;$$RENAME ME emt:view:TESTRAL-placeholder
(defstruct (emt:view:no-note
	    (:constructor emt:view:make-no-note)
	    (:conc-name emt:view:no-note->)
	      (:include emt:view:presentable))
   "A blank note-like placeholder, not associated with a TESTRAL note")

;;;_  . TESTRAL unexpanded leaf
'  ;;$$OBSOLETE
(defstruct (emt:view:TESTRAL-unexpanded
	    (:constructor emt:view:make-TESTRAL-unexpanded)
	    (:conc-name emt:view:TESTRAL-unexpanded->)
	      (:include emt:view:presentable))
   ""
   ;;The list of applicable nodes (generally in another list)
   (start () :type (repeat emt:testral:base))
   ;;The inapplicable tail of that list.
   (past-end ()  :type (repeat emt:testral:base)))




;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/view-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/view-types.el ends here
