;;;_ emtest/viewer/emviewer2.el --- Plain, static viewer for emtest

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

;; A static version of the original emviewer.

;;;_ , Requires

(require 'emtest/types/testral-types)
(require 'emtest/viewer/emformat)
(require 'emtest/viewer/empathtree)
(require 'emtest/viewer/mode)
(require 'emtest/viewer/organize)
(require 'emtest/viewer/view-types)
(require 'formatter/loformat)
(require 'formatter/pseudodynamic)
(require 'utility/dynvars)
(require 'utility/pathtree)

;;;_. Body
;;;_ , Constants
;;;_  . emt:vw:top:report-buffer-name
(defconst emt:vw:top:report-buffer-name 
   "*Emtest Report*")
;;;_ , Vars
(defvar emt:vw:top:report-buffer nil 
   "Buffer that we write reports in" )

;;;_ , Glue functions
;;;_  . emt:vw:top:pathtree-cb
;;Different from `emtest:viewer:pathtree-cb' in that it does not try
;;to print objects, thus does not deal with `dirty' flag.
(defun emt:vw:top:pathtree-cb (obj tree)
   "Callback to handle dirty flags, that `pathree' gets."
   (check-type obj emt:view:presentable)
   (check-type tree emtvp)

   (emtvp:util:handle-dirty obj
      (cond
	 (
	    (or
	       (member 'new dirty-flags)
	       (emtvp:util:member-as-car 
		  'replaced 
		  dirty-flags))
	    (undirty 'new)
	    (undirty-car 'replaced)
	    (new-dirty 'summary)
	    (new-dirty 'notes)
	    (let
	       ((parent (emtvp:node->parent obj)))
	       (when parent
		  ;;Parent's summary may be dirty now.
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
	       (emt:pth:grd:cache-subtree-grade obj)
	       ;;Record this property
	       (when (emt:view:suite-p obj)
		  (emt:ind:set-prop
		     (emt:run:how->contents
			(emt:run:explorable->how-to-run
			   (emt:view:suite->explorable obj)))
		     'grade-says-rerun
		     (case
			(emt:view:grade-summary->worst
			   (emt:view:presentable->sum-grades obj))
			(fail t)
			((ok test-case) nil)
			;;Generally rerunning blowouts is just
			;;troublesome
			(blowout nil)
			;;Some ungradeds merely used `assert' instead of
			;;`emt:assert'
			(ungraded t)
			;;Dormants asked not to be run, so don't try hard
			;;to run them
			(dormant nil)
			;;Anything else, no effect.
			(t t))))
	       (undirty 'summary)
	       ;;Parent (if any) now needs to be resummarized.
	       (let
		  ((parent (emtvp:node->parent obj)))
		  (when parent
		     (new-dirty-node 'summary parent)))))

	 ((member 'notes dirty-flags)
	    (emt:pth:collect-testral-2 obj tree)
	    (undirty 'notes))
	 ;;Other dirty flags are just cleared
	 (dirty-flags
	    (setq dirty-flags '())))))



;;;_  . emt:vw:top:setup-if-needed
(defun emt:vw:top:setup-if-needed ()
   ""
   (unless (buffer-live-p emt:vw:top:report-buffer)
      (setq 
	 emt:vw:top:report-buffer 
	 (generate-new-buffer
	    emt:vw:top:report-buffer-name)))
   (emtvo:setup-if-needed #'emt:vw:top:pathtree-cb #'ignore))

;;;_ , Static printing functions
;;;_  . emt:vw:top:print-all
(defun emt:vw:top:print-all (top-node)
   "Format and print the whole report."
   (emtvp:freshen emtvo:pathtree)
   (with-current-buffer emt:vw:top:report-buffer
      (let
	 ((inhibit-read-only t))
	 (erase-buffer))
      (emtest/viewer/mode)
      (emt:vw:top:insert top-node)))
;;;_  . emt:vw:top:format-alist
(defconst emt:vw:top:format-alist 
   (append
      '((dynamic emt:fmt:dyn:insert))
      loformat:default-alist)
   
   "List of formatters that emformat uses.")

;;;_  . emt:vw:top:insert
(defun emt:vw:top:insert (top-node)
   "Insert TOP-NODE via loformat"
   (let*
      ((tree (emt:fmt:top top-node)))
      (loformat:insert
	 tree
	 emt:vw:top:format-alist)))

;;;_ , Overall callback
;;;_  . emt:vw:top:tests-outstanding
(defvar emt:vw:top:tests-outstanding 0 
   "Number of tests currently enqueued that we haven't received
   reports from." )
;;;_  . emt:vw:top:tester-cb
;;;###autoload
(defun emt:vw:top:tester-cb (report)
   ""
   (check-type report emt:testral:report)
   (emt:vw:top:setup-if-needed)
   (emtvo:receive report)
   (incf emt:vw:top:tests-outstanding
      (-
	 (emt:testral:report->newly-pending report)
	 (length
	    (emt:testral:report->suites report))))
   
   (when
      (equal emt:vw:top:tests-outstanding 0)
      (emt:vw:top:print-all (emtvo:get-root))
      (pop-to-buffer emt:vw:top:report-buffer)))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emviewer2)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer2.el ends here
