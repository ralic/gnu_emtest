;;;_ emtest/viewer/organize.el --- Code to organize results before presenting them

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp

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

;; The relevant code will be moved in here from emviewer and emviewer2


;;;_ , Requires

(require 'utility/pathtree)
(require 'emtest/viewer/receive)

;;;_. Body

;;;_ , Variables
(defvar emtvo:pathtree nil 
   "Pathtree object of type emtvp" )
(defvar emtvo:receiver 
   nil
   "Receiver object, of type `emtvr:data'" )
;;;_ , Callbacks
;;;_  . emtvo:receive-cb

;;$$PARAMETERIZE MY on make-display-data and tree object.
(defun emtvo:receive-cb (presentation-path cell)
   "Emviewer callback that `receive' gets.
It just tells a pathtree to add this node."
   '
   (emtvp:add/replace-node
      ;;The pathtree root
      emtvo:pathtree 
      ;;The path
      presentation-path
      ;;The data
      (list 'suite cell))
   (let
      ((old-node (emtvp:find-node emtvo:pathtree presentation-path)))
      ;;$$MAKE ME ACCURATE
      ;;$$PUNT Call the make-display-data callback.

      (setf
	 (emtvp:node->name cell)
	 (car (last presentation-path)))
      ;;Adopt suite children but not note children
      (setf
	 (emtvp:node->children cell)
	 (delq nil
	    (mapcar
	       #'(lambda (child)
		    (unless (emt:view:TESTRAL-2-p child) child))
	       (emtvp:node->children old-node))))
       
      ;;$$IMPROVE ME if (eq old-node cell)
      ;;just dirty it for resummary/redisplay.  As `new'?  `updated'?
      (emtvp:replace-node
	 emtvo:pathtree old-node cell)))
;;;_  . emtvo:pathtree-cb-aux
(defun emtvo:pathtree-cb-aux (old-version arg display-data)
   "Worker for the pathtree callback.
Make a `emt:view:presentable' or its descendant."
   (cond
      ((null arg)
	 ;;Just enough to show the name.
	 (emt:view:make-presentable
	    :list display-data))
      ((not (consp arg)) (error "Should be a cons"))
      ((eq (car arg) 'suite)
	 (let
	    ((suite (second arg)))
	    (setf
	       (emt:view:presentable->list suite)
	       display-data)
	    ;;Remove any note children from previous iterations.
	    (setf
	       (emtvp:node->children suite)
	       (delq nil
		  (mapcar
		     #'(lambda (child)
			  (unless (emt:view:TESTRAL-2-p child) child))
		     (emtvp:node->children suite))))
	    suite))
      ((eq (car arg) 'note-2)
	 (check-type (second arg) emt:view:presentable)
	 ;;Adopt all the old version's children
	 (when old-version
	    (setf
	       (emtvp:node->children (second arg))
	       (emtvp:node->children old-version)))
	 (setf
	    (emt:view:presentable->list (second arg))
	    display-data)
	 (second arg))
      ;;$$OBSOLETE
      ((eq (car arg) 'note)
	 (error "Don't call with arg `note'")
	 (check-type (second arg) (repeat emt:testral:newstyle))
	 ;;Aside from content, it's all set in pathtree or pathtree's
	 ;;dirty-handler callback.  `:children' is mixed-initiative,
	 ;;but we don't set TESTRAL children.
	 (emt:view:make-TESTRAL
	    :content (second arg)))))


;;;_ , Setup
;;;_  . emtvo:setup-if-needed
(defun emtvo:setup-if-needed (pathtree-cb make-display-data)
   ""
   (unless emtvo:pathtree
      (setq emtvo:pathtree
	 (emtvp:make-pathtree
	    pathtree-cb
	    `(lambda (old-version arg)
		(emtvo:pathtree-cb-aux old-version arg (,make-display-data)))
	    'emt:view:presentable)))

   (unless 
      emtvo:receiver
      (setq emtvo:receiver
	 (emtvr:make-data
	    :alist ()
	    :tree-insert-cb #'emtvo:receive-cb
	    ;;:tree-remove-cb Not yet
	    ;;:node-replace-cb Not yet
	    ))))

;;;_ , Function entry points
;;;_  . emtvo:get-root
(defun emtvo:get-root ()
   "Return the root of the pathtree"
   (emtvp->root emtvo:pathtree))

;;;_  . emtvo:receive
(defun emtvo:receive (report)
   "Receive REPORT"
   (emtvr:newstyle emtvo:receiver report)
   (emtvp:freshen emtvo:pathtree))

;;;_ , Command entry points
;;;_  . emtv2:tests-outstanding hollow. 
(defvar emtv2:tests-outstanding)
;;;_  . emtvo:reset
;;;###autoload
(defun emtvo:reset ()
   ""
   
   (interactive)
   (setq emtv2:tests-outstanding 0)
   (setq emtvo:pathtree nil)
   (setq emtvo:receiver nil))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/organize)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/organize.el ends here
