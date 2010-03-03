;;;_ viewers/emviewer.el --- Chewie-based viewer for Emtest results

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

(require 'viewers/chewie)  ;;May be replaced by 'viewers/wookie
(require 'viewers/receive)
(require 'viewers/pathtree)
(require 'viewers/emformat)

;;;_. Body
;;;_ , Config

(defconst emtve:report-buffer-name 
   "*Emtest Report (emviewer)*")
;;;_ , Types
;;$$USEME in place of the many globals
(defstruct emtve
   "An emviewer object"
   (report-buffer () :type (satisfies bufferp))
   (result-root   () :type emtvp)
   (wookie        () :type wookie:wookie)
   (receiver      () :type emtvr:data))

;;;_ , Globals
;;Other than tests, everything uses this emviewer object.
;;NOT IN USE YET
(defconst emtve:*viewer* 
   nil
   "Global Emviewer object" )
(defconst emtve:report-buffer nil 
   "" )
;;result object from receive.  Now it should live here.
;;$$RENAME ME emtve:pathtree
(defconst emtve:result-root nil 
   "" )
;;$$RENAME ME maybe.  To *:wookie
(defconst emtve:chewie nil 
   "" )
(defconst emtve:receiver 
   nil ;;Should be made by setup.  Of type `emtvr:data'
   "" )
;;;_ , Pathtree callback functions 
;;;_  . emtest:viewer:receive-cb
(defun emtest:viewer:receive-cb (presentation-path cell)
   "Emviewer callback that `receive' gets.
It just tells a pathtree to add this node."
   ;;This interface really does need its own callback.
   (emtvp:add/replace-node
      ;;The pathtree root
      emtve:result-root 
      ;;The path
      presentation-path
      ;;The data
      (make-emt:view:suite-newstyle 
	 :list (chewie:2:make-list)
	 :cell cell)))
;;;_  . emtest:viewer:pathtree-cb
(defun emtest:viewer:pathtree-cb (obj)
   "Callback to handle dirty flags, that `pathree' gets."
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
	       ((parent (emtvp-node-parent obj)))
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
			  (emtvp-node-dirty-flags child)))
		  (emtvp-node-children obj))
	       ;;Do summarization
	       (emtvr:sum-node-badnesses obj)
	       (undirty 'summary)
	       (new-dirty 'display) ;;Now we can display it
	       ;;Parent (if any) now needs to be resummarized.
	       (let
		  ((parent (emtvp-node-parent obj)))
		  (when parent
		     (new-dirty-node 'summary parent)))))
	 
	 ((member 'display dirty-flags)
	    ;;Shouldn't have dirty summary because that would have
	    ;;been caught by the previous clause.
	    (assert (not (member 'summary dirty-flags)) t)
	    (undirty 'display)
	    (chewie:redisplay emtve:chewie obj)))))

;;;_ , Setup emtest:viewer:setup-if-needed
(defun emtest:viewer:setup-if-needed ()
   ""
   (unless emtve:result-root
      (setq emtve:result-root
	 (emtvp:make-empty-tree-newstyle
	    #'emtest:viewer:pathtree-cb
	    ;;Default makes the base type.
	    #'(lambda ()
		 (make-emt:view:presentable
		    :list (chewie:2:make-list)))
	    'emt:view:suite-newstyle)))
   
   (unless emtve:chewie
      (unless emtve:report-buffer
	 (setq 
	    emtve:report-buffer 
	    (generate-new-buffer
	       emtve:report-buffer-name)))
	 
      (with-current-buffer emtve:report-buffer
	 (erase-buffer)
	 (setq emtve:chewie
	    (chewie:create-wookie
	       #'emtvf:top
	       ;;Pass instead the root node of this.
	       ;;emtve:result-root ;;Chewie makes a dynamic object.
	       (emtvp-root emtve:result-root)
	       #'(lambda (obj)
		    (emt:view:presentable-list
		       (emtvp-node-data obj)))
	       :buf
	       emtve:report-buffer
	       :handlers (list chewie:handler-alist)
	       ))
	 ;;For now, anyways.
	 (outline-mode)))
   (unless 
      emtve:receiver
      (setq emtve:receiver
	 (make-emtvr:data
	    :alist ()
	    :tree-insert-cb #'emtest:viewer:receive-cb
	    ;;:tree-remove-cb Not yet
	    ))))


;;;_ , emtest:viewer:receive

;;This is the callback for emtest to use.  For now, this is always the
;;one.  Later, customization could control what is used.
(defun emtest:viewer:receive (report)
   ""
   (check-type report emt:testral:report)
   (emtest:viewer:setup-if-needed)
   (emtvr:newstyle emtve:receiver report)
   (emtvp:freshen emtve:result-root)
   (pop-to-buffer emtve:report-buffer))


;;;_. Footers
;;;_ , Provides

(provide 'viewers/emviewer)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/emviewer.el ends here
