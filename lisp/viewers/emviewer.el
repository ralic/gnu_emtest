;;;_ emviewer.el --- Chewie-based viewer for Emtest results

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

(require 'chewie)  ;;May be replaced by 'wookie
(require 'receive)
(require 'pathtree)
(require 'emformat)

;;;_. Body
;;;_ , Config

(defconst emtest:viewer:emviewer:report-buffer-name 
   "*Emtest Report (emviewer)*")
;;;_ , Types
;;$$USEME in place of the many globals
(defstruct emtest:viewer:emviewer
   "An emviewer object"
   (report-buffer () :type (satisfies bufferp))
   result-root
   chewie  ;;May be replaced by wookie
   receiver)

;;;_ , Globals
;;Other than tests, everything uses this emviewer object.
;;NOT IN USE YET
(defconst emtest:viewer:*emviewer* 
   nil
   "Global Emviewer object" )
(defconst emtest:viewer:emviewer:report-buffer nil 
   "" )
;;result object from receive.  Now it should live here.
;;$$RENAME ME emtest:viewer:emviewer:pathtree
(defconst emtest:viewer:emviewer:result-root nil 
   "" )
(defconst emtest:viewer:emviewer:chewie nil 
   "" )
(defconst emtest:viewer:emviewer:receiver 
   nil ;;Should be made by setup.  Of type `emt:receive:data'
   "" )
;;;_ , Pathtree callback functions 
;;;_  . emtest:viewer:receive-cb
(defun emtest:viewer:receive-cb (presentation-path cell)
   "Emviewer callback that `receive' gets.
It just tells a pathtree to add this node."
   ;;This interface really does need its own callback.
   (emt:pathtree:add/replace-node
      ;;The pathtree root
      emtest:viewer:emviewer:result-root 
      ;;The path
      presentation-path
      ;;The data
      (make-emt:view:suite-newstyle :cell cell)))
;;;_  . emtest:viewer:pathtree-cb

;;Except for summarizing, this isn't very specific to emviewer.
;;$$SIMPLIFYME This could be a general framework, except for the
;;`cond' statement.  That general framework would belong in pathtree.
;;Expand how much we farm off to pathtree utils.
(defun emtest:viewer:pathtree-cb (obj)
   ""
   (check-type obj emt:view:pathtree-node)

   (let* 
      ((dirty-flags (emt:view:pathtree-node-dirtinesses obj))
	 (new-dirty-nodes '()))
      (flet
	 (  (undirty (flag)
	       (setq dirty-flags
		  (delete* flag dirty-flags)))
	    (undirty-car (flag)
	       (setq dirty-flags
		  (delete* 
		     flag
		     dirty-flags 
		     :test #'emt:view:pathtree:util:match-as-car)))
	    (new-dirty (flag)
	       (push flag dirty-flags))
	    (new-dirty-node (flag node)
	       (push flag (emt:view:pathtree-node-dirtinesses node))
	       (push node new-dirty-nodes)))
	 
	 
	 
	 
      ;;Clauses generally delete their flag, but not always.  Each
      ;;clause returns a list of nodes to continue on.
      ;;Maybe encap this as a clause-building cond relative.
      ;;Maybe let each clause push or append-in the nodes it
      ;;wants to be rerun.
      (cond
	 ((null dirty-flags) '())
	 ((or
	     (member 'new dirty-flags)
	     ;;Treated the same, for now anyways.
	     (emt:view:pathtree:util:member-as-car 
		'replaced 
		dirty-flags))
	    (undirty 'new)
	    '(setq dirty-flags
	       (delete* 'new dirty-flags))
	    (undirty-car 'replaced)
	    '(setq dirty-flags
	       (delete* 
		  'replaced 
		  dirty-flags 
		  :test #'emt:view:pathtree:util:match-as-car))

	    ;;Dirty display is implied by dirty summary.
	    (new-dirty 'summary)
	    '(push 'summary dirty-flags)
	    (let
	       ((parent (emt:view:pathtree-node-parent obj)))
	       (when parent
		  ;;Parent's summary or display may be dirty now.
		  ;;And parent's display is dirty, but that's implied
		  ;;by summary being dirty.
		  (new-dirty-node 'summary parent)
		  '(push 'summary 
		     (emt:view:pathtree-node-dirtinesses
			parent))
		  '(push parent new-dirty-nodes))))
	 
	 ((member 'summary dirty-flags)
	    (if 
	       ;;If any children have yet to be summarized,
	       ;;delay
	       (some
		  #'(lambda (child)
		       (member 'summary 
			  (emt:view:pathtree-node-dirtinesses child)))
		  (emt:view:pathtree-node-children obj))
	       (list obj)
	       (let
		  ((parent (emt:view:pathtree-node-parent obj)))
		  (undirty 'summary)
		  '(setq dirty-flags
		     (delete* 'summary dirty-flags))
		  ;;Do summarization
		  (emt:receive:sum-node-badnesses obj)
		  (when parent
		     ;;Mark the parent (if any) as needing to be
		     ;;summarized.
		     (new-dirty-node 'summary parent)
		     '(push 'summary 
			(emt:view:pathtree-node-dirtinesses
			   parent))
		     '(push parent new-dirty-nodes))
		  ;;Now we'll display it
		  (new-dirty 'display)
		  '(push 'display dirty-flags))))
	 
	 ((member 'display dirty-flags)
	    ;;Shouldn't have dirty summary because that would have
	    ;;been caught by the previous clause.
	    (assert (not (member 'summary dirty-flags)) t)
	    (undirty 'display)
	    '(setq dirty-flags
	       (delete* 'display dirty-flags))
	    (chewie:freshen-obj
	       emtest:viewer:emviewer:chewie 
	       obj))))
      
      (setf 
	 (emt:view:pathtree-node-dirtinesses obj) dirty-flags)
      

      ;;Return the nodes we newly know are dirty.  If dirty-flags is
      ;;non-nil, that includes this node.
      (if dirty-flags
	 (cons obj new-dirty-nodes)
	 new-dirty-nodes)))


;;;_ , Setup emtest:viewer:setup-if-needed
(defun emtest:viewer:setup-if-needed ()
   ""
   (unless emtest:viewer:emviewer:result-root
      (setq emtest:viewer:emviewer:result-root
	 (emt:pathtree:make-empty-tree-newstyle
	    #'emtest:viewer:pathtree-cb
	    ;;Default makes the base type
	    #'(lambda ()
		 (make-emt:view:presentable))
	    'emt:view:suite-newstyle)))
   
   (unless emtest:viewer:emviewer:chewie
      (unless emtest:viewer:emviewer:report-buffer
	 (setq 
	    emtest:viewer:emviewer:report-buffer 
	    (generate-new-buffer
	       emtest:viewer:emviewer:report-buffer-name)))
	 
      (with-current-buffer emtest:viewer:emviewer:report-buffer
	 (erase-buffer)
	 (setq emtest:viewer:emviewer:chewie
	    (chewie:setup-root
	       emtest:viewer:emviewer:result-root
	       '()
	       #'emtest:viewer:fmt:top
	       emtest:viewer:emviewer:report-buffer))
	 ;;For now, anyways.
	 (outline-mode)))
   (unless 
      emtest:viewer:emviewer:receiver
      (setq emtest:viewer:emviewer:receiver
	 (make-emt:receive:data
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
   (emt:receive:newstyle emtest:viewer:emviewer:receiver report)
   (emt:pathtree:freshen emtest:viewer:emviewer:result-root)
   (pop-to-buffer emtest:viewer:emviewer:report-buffer))


;;;_. Footers
;;;_ , Provides

(provide 'emviewer)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emviewer.el ends here
