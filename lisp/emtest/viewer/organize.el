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
(defvar emtvo:make-display-data #'ignore
   "Callback to make a display object" )
;;;_ , Callbacks
;;;_  . emtvo:receive-cb

(defun emtvo:receive-cb (presentation-path cell)
   "Emviewer callback that `receive' gets.
It just tells a pathtree to add this node."
   (emtvp:add/replace-node
      ;;The pathtree root
      emtvo:pathtree 
      ;;The path
      presentation-path
      ;;The data
      cell))
;;;_  . emtvo:pathtree-cb
(defun emtvo:pathtree-cb (old-node data)
   "Callback to make a pathtree node."
   (if data 
      data
      (emt:view:make-presentable
	 :list (funcall emtvo:make-display-data))))

;;;_ , Setup
;;;_  . emtvo:setup-if-needed
(defun emtvo:setup-if-needed (node-dirtied-cb make-display-data)
   ""
   (setq emtvo:make-display-data make-display-data)
   (unless emtvo:pathtree
      (setq emtvo:pathtree
	 (emtvp:make-pathtree
	    node-dirtied-cb
	    #'emtvo:pathtree-cb
	    'emt:view:presentable)))

   (unless 
      emtvo:receiver
      (setq emtvo:receiver
	 (emtvr:make-data
	    :alist ()
	    :insert-cb #'emtvo:receive-cb
	    ;;:remove-cb Not yet
	    ;;:update-cb Not yet.  
	    ))))

;;;_ , Function entry points
;;;_  . emtvo:get-root
(defun emtvo:get-root ()
   "Return the root of the pathtree"
   (emtvp->root emtvo:pathtree))

;;;_  . emtvo:receive
(defun emtvo:receive (report)
   "Receive REPORT"
   (emtvr:receive-report emtvo:receiver report)
   (emtvp:freshen emtvo:pathtree))

;;;_ , Command entry points
;;;_  . emtvo:reset
;;;###autoload
(defun emtvo:reset ()
   ""
   
   (interactive)
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
