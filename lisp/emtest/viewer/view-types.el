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
(eval-when-compile
   (require 'cl))
;;;_. Body
;;;_ , emtvr:suite-newstyle-id

(deftype emtvr:suite-newstyle-id ()
   "Can be a UUID string or a how-to-run object"
   '(or string emt:test-ID:e-n))

;;;_ , emtvr:suite-newstyle

;;This is cted by receive.
;;Note that changes will affect basically just `emtvr:one-newstyle'.

(defstruct emtvr:suite-newstyle
   "Report element in receive tree"
   (id () :type emtvr:suite-newstyle-id
      :doc "The \"official\" id.")

   ;;Was a emt:test-ID:e-n
   ;;$$CHANGED to a emtt:explorable
   ;;Could even be a `emtt:method'
   (how-to-run ():type emtt:explorable
      :doc "How to run this as a test.")

   ;;The full presentation path, including any prefix from report.
   (presentation-path ()   
      :type emt:testral:suite-id
      :doc "The path to the corresponding node in the pathtree.")

   ;;This is becoming more of a datestamp/circumstance info.
   (testrun-id ()
      :type emt:testral:testrun-id
      :doc "The ID of the testrun that created this result.")
   ;;$$RENAME ME  "result"
   (suite ()     
      :type (or null 
	       emt:testral:suite 
	       emt:testral:test-runner-info)
      :doc "The result data itself"))


;;;_ , Presentables

;;;_  . Base 

;;Emviewer uses this as the content element in pathtree nodes.
(defstruct emt:view:presentable
   ""
   ;;Summarized badnesses from all subtrees.  They are summarized
   ;;treewise, including any badnesses from this node.
   (sum-badnesses () :type (repeat emt:result-badness))
   (list () :type chewie:2:list))



;;;_  . Suite in tree (as by emviewer)
(defstruct (emt:view:suite-newstyle 
	      (:include emt:view:presentable))
   ""
   ;;Just for suite nodes.
   (cell () :type emtvr:suite-newstyle))

;;;_  . TESTRAL finished
(defstruct (emt:view:TESTRAL
	      (:include emt:view:presentable))
   ""

   (main () :type (or emt:testral:alone emt:testral:push))
   (end  () :type (or null emt:testral:pop))
   ;;May GO AWAY and be represented by child nodes having mains of
   ;;type `emt:testral:separate'.
   (args () :type (repeat emt:testral:separate)))

;;;_  . TESTRAL unexpanded leaf
(defstruct (emt:view:TESTRAL-unexpanded
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
