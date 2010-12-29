;;;_ emtest/viewer/emviewer.el --- Chewie-based viewer for Emtest results

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

;; This file coordinates the use of several complementary formatting
;; and display modules to give emtest a dynamic viewer.

;;;_ , Requires

(require 'emtest/viewer/receive)
(require 'emtest/viewer/empathtree)
(require 'emtest/viewer/emformat)
(require 'emtest/viewer/view-types)
(require 'viewers/chewie)
(require 'viewers/loformat)
(require 'utility/pathtree)

;;;_. Body
;;;_ , Config

(defconst emtve:report-buffer-name 
   "*Emtest Report (emviewer)*")
;;;_ , Types
;;$$USEME in place of the many globals
(defstruct (emtve
	    (:constructor emtve:make)
	    (:conc-name emtve->)
	    (:copier nil))
   "An emviewer object"
   (report-buffer () :type (satisfies bufferp))
   (result-root   () :type emtvp)
   (wookie        () :type endor:endor)
   (receiver      () :type emtvr:data))

;;;_ , Globals
;;Other than tests, everything uses this emviewer object.
;;NOT IN USE YET
(defvar emtve:*viewer* 
   nil
   "Global Emviewer object.
An `emtve' or `nil'." )
(defvar emtve:report-buffer nil 
   "" )
(defvar emtve:chewie nil 
   "" )
;;;_ , Pathtree callback functions 
;;;_  . emtve:vp-node->dlist
(defun emtve:vp-node->dlist (obj)
   ""
   (error "Obsolete, pathtree nodes no longer accept data")
   (emt:view:presentable->list
      (emtvp:node->data obj)))

;;;_  . emtest:viewer:pathtree-cb
(defun emtest:viewer:pathtree-cb (obj tree)
   "Callback to handle dirty flags, that `pathree' gets."
   (error "Obsolete, pathtree nodes no longer accept data")
   (emtvp:util:handle-dirty obj
      (cond
	 ((or
	     (member 'new dirty-flags)
	     (emtvp:util:member-as-car 
		'replaced 
		dirty-flags))
	    (undirty 'new)
	    (undirty-car 'replaced)

	    ;;Dirty display is implied by dirty summary.
	    (new-dirty 'summary)
	    (let
	       ((parent (emtvp:node->parent obj)))
	       (when parent
		  ;;Parent's summary may be dirty now.  Parent's
		  ;;display definitely is, but that's implied by
		  ;;summary being dirty.
		  (new-dirty-node 'summary parent))))
	 
	 ((member 'summary dirty-flags)
	    ;;If any children have yet to be summarized, can't do
	    ;;anything yet.
	    (unless 
	       (some
		  #'(lambda (child)
		       (member 'summary 
			  (emtvp:node->dirty-flags child)))
		  (emtvp:node->children obj))
	       ;;Do summarization
	       (emtvr:cache-subtree-grade obj)
	       (undirty 'summary)
	       (new-dirty 'display) ;;Now we can display it
	       ;;Parent (if any) now needs to be resummarized.
	       (let
		  ((parent (emtvp:node->parent obj)))
		  (when parent
		     (new-dirty-node 'summary parent)))))
	 
	 ((member 'display dirty-flags)
	    ;;Shouldn't have dirty summary because that would have
	    ;;been caught by the previous clause.
	    (assert (not (member 'summary dirty-flags)) t)
	    (undirty 'display)
	    (wookie:redisplay 
	       emtve:chewie 
	       (emtve:vp-node->dlist obj)))
	 ;;Other dirty flags are just cleared
	 (dirty-flags
	    (setq dirty-flags '())))))

;;;_ , Setup emtest:viewer:setup-if-needed
(defun emtest:viewer:setup-if-needed ()
   ""
   (error "Obsolete, pathtree nodes no longer accept data")
   (unless 
      (and 
	 emtve:chewie
	 (buffer-live-p emtve:report-buffer))
      (unless (buffer-live-p emtve:report-buffer)
	 (setq 
	    emtve:report-buffer 
	    (generate-new-buffer
	       emtve:report-buffer-name)))

      ;;If there was no buffer, we always need a new chewie
      (with-current-buffer emtve:report-buffer
	 (erase-buffer)
	 (setq emtve:chewie
	    (chewie:make-chewie
	       (emtvo:get-root)
	       '()
	       #'emtvf:top
	       #'loformat:insert
	       #'emtve:vp-node->dlist))
	 ;;May be replaced at some point
	 (outline-mode)))

   (emtvo:setup-if-needed
      #'emtest:viewer:pathtree-cb
      #'wookie:make-dlist))



;;;_ , emtve:tester-cb
(defun emtve:tester-cb (report)
   "The Emviewer callback for emtest to use"
   (error "Obsolete, pathtree nodes no longer accept data")
   (check-type report emt:testral:report)
   (emtest:viewer:setup-if-needed)
   (emtvo:receive report)
   (pop-to-buffer emtve:report-buffer))

;;;_ , emt:relaunch-all
;;Very dependent on emviewer, and may belong in it or associated to it
;;or to receive.
;;;###autoload
(defun emt:relaunch-all ()
   ""
   
   (interactive)
   (let*
      ()
      
      ))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emviewer)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + no-byte-compile: t
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer.el ends here
