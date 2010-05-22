;;;_ emviewer2.el --- Plain, static viewer for emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, outlines

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

;; Borrowed from emviewer, only needs differentiate a few things.
;; They ought to share code.


;;;_ , Requires



;;;_. Body
;;;_ , emtv2:report-buffer-name
(defconst emtv2:report-buffer-name 
   "*Emtest Report (Plain viewer)*")
;;;_ , Vars
(defvar emtv2:report-buffer nil 
   "Buffer that we write reports in" )

(defvar emtv2:pathtree nil 
   "Result object from receive" )
'
(defvar emtv2:chewie nil 
   "Not used here" )
(defvar emtv2:receiver 
   nil
   "Receiver object, of type `emtvr:data'" )

;;;_ , emtv2:receive-cb
'  ;;Doesn't need to differentiate yet - may never.
(defun emtv2:receive-cb (presentation-path cell)
   "Emviewer callback that `receive' gets.
It just tells a pathtree to add this node."
   (emtvp:add/replace-node
      ;;The pathtree root
      emtv2:pathtree 
      ;;The path
      presentation-path
      ;;The data
      (make-emt:view:suite-newstyle 
	 :list (wookie:make-dlist)
	 :cell cell)))

;;;_ , emtv2:setup-if-needed
;;$$ADAPT ME
(defun emtv2:setup-if-needed ()
   ""
   (unless emtv2:pathtree
      (setq emtv2:pathtree
	 (emtvp:make-empty-tree-newstyle
	    #'emtest:viewer:pathtree-cb
	    ;;Default makes the base type.
	    #'(lambda ()
		 (make-emt:view:presentable
		    :list (wookie:make-dlist)))
	    'emt:view:suite-newstyle)))

   (unless 
      (and 
	 emtv2:chewie
	 (buffer-live-p emtv2:report-buffer))
      (unless (buffer-live-p emtv2:report-buffer)
	 (setq 
	    emtv2:report-buffer 
	    (generate-new-buffer
	       emtv2:report-buffer-name)))

      ;;If there was no buffer, we always need a new chewie
      (with-current-buffer emtv2:report-buffer
	 (erase-buffer)
	 (setq emtv2:chewie
	    (chewie:make-chewie
	       (emtvp-root emtv2:pathtree)
	       '()
	       #'emtvf:top
	       #'emt:plain-viewer:insert
	       ;;#'loformat:print
	       #'emtv2:vp-node->dlist))
	 ;;May be replaced at some point
	 (outline-mode)))
   (unless 
      emtv2:receiver
      (setq emtv2:receiver
	 (make-emtvr:data
	    :alist ()
	    :tree-insert-cb #'emtest:viewer:receive-cb
	    ;;:tree-remove-cb Not yet
	    ))))

;;;_ , emtv2:tester-cb

;;This is the callback for emtest to use.  For now, this is always the
;;one.  Later, customization could control what is used.
(defun emtv2:tester-cb (report)
   ""
   (check-type report emt:testral:report)
   (emtest:viewer:setup-if-needed)
   (emtvr:newstyle emtv2:receiver report)
   (emtvp:freshen emtv2:pathtree)
   (pop-to-buffer emtv2:report-buffer))

;;;_. Footers
;;;_ , Provides

(provide 'emviewer2)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emviewer2.el ends here
