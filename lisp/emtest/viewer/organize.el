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
(defvar emt:vw:og:pathtree nil 
   "Pathtree object of type pathtree" )
(defvar emt:vw:og:receiver 
   nil
   "Receiver object, of type `emt:r:data'" )
;;;_ , Callbacks
;;;_  . emt:vw:og:receive-cb

;;$$PARAMETERIZE MY on make-display-data
(defun emt:vw:og:receive-cb (presentation-path cell)
   "Emviewer callback that `receive' gets.
It just tells a pathtree to add this node."
   (emt:pth:place-node
      emt:vw:og:pathtree presentation-path cell))

;;;_  . emt:vw:og:pathtree-cb-aux
;;$$OBSOLESCENT - Only called to make root.  We might as well just
;;pass root.  Waiting on changes to `pathtree:make-pathtree'.
;;$$RETHINK ME When we set up display-data.  Perhaps better in the
;;freshener, getting foreign data stored in the pathtree.
(defun emt:vw:og:pathtree-cb-aux (old-version arg display-data)
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
	       (pathtree:node->children suite)
	       (delq nil
		  (mapcar
		     #'(lambda (child)
			  (unless (emt:view:note-p child) child))
		     (pathtree:node->children suite))))
	    suite))
      ((eq (car arg) 'note-2)
	 (check-type (second arg) emt:view:presentable)
	 ;;Adopt all the old version's children
	 (when old-version
	    (setf
	       (pathtree:node->children (second arg))
	       (pathtree:node->children old-version)))
	 (setf
	    (emt:view:presentable->list (second arg))
	    display-data)
	 (second arg))))


;;;_ , Setup
;;;_  . emt:vw:og:setup-if-needed
(defun emt:vw:og:setup-if-needed (pathtree-cb make-display-data)
   ""
   (unless emt:vw:og:pathtree
      (setq emt:vw:og:pathtree
	 (pathtree:make-pathtree
	    pathtree-cb
	    `(lambda (old-version arg)
		(emt:vw:og:pathtree-cb-aux old-version arg (,make-display-data)))
	    'emt:view:presentable)))

   (unless 
      emt:vw:og:receiver
      (setq emt:vw:og:receiver
	 (emt:r:make-data
	    :alist ()
	    :tree-insert-cb #'emt:vw:og:receive-cb
	    ;;:tree-remove-cb Not yet
	    ;;:node-replace-cb Not yet
	    ))))

;;;_ , Function entry points
;;;_  . emt:vw:og:get-root
(defun emt:vw:og:get-root ()
   "Return the root of the pathtree"
   (pathtree->root emt:vw:og:pathtree))

;;;_  . emt:vw:og:receive
(defun emt:vw:og:receive (report)
   "Receive REPORT"
   (emt:r:receive emt:vw:og:receiver report)
   (pathtree:freshen emt:vw:og:pathtree))

;;;_ , Command entry points
;;;_  . emt:vw:top:tests-outstanding hollow. 
(defvar emt:vw:top:tests-outstanding)
;;;_  . emtest:reset
;;;###autoload
(defun emtest:reset ()
   ""
   
   (interactive)
   (setq emt:vw:top:tests-outstanding 0)
   (setq emt:vw:og:pathtree nil)
   (setq emt:vw:og:receiver nil))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/organize)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/organize.el ends here
