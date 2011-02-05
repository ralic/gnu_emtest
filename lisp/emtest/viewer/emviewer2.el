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
;;;_  . emtv2:report-buffer-name
(defconst emtv2:report-buffer-name 
   "*Emtest Report*")
;;;_ , Vars
(defvar emtv2:report-buffer nil 
   "Buffer that we write reports in" )

;;;_ , Glue functions
;;;_  . emtv2:pathtree-cb
;;Different from `emtest:viewer:pathtree-cb' in that it does not try
;;to print objects, thus does not deal with `dirty' flag.
(defun emtv2:pathtree-cb (obj tree)
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
	       (emtvr:cache-subtree-grade obj)
	       ;;Record a suitable score component
	       (when (emt:view:suite-p obj)
		  (emt:ind:set-score-component
		     (emtt:explorable->how-to-run
			(emt:view:suite->how-to-run obj))
		     'passfail
		     (case
			(emt:grade:summary->worst
			   (emt:view:presentable->sum-grades obj))
			(fail 10)
			;;Usually don't need to rerun passes
			((ok test-case) -10)
			;;Generally rerunning blowouts is just
			;;troublesome
			(blowout -100)
			;;Some ungradeds merely used `assert' instead of
			;;`emt:assert'
			(ungraded 0)
			;;Dormants asked not to be run, so don't try hard
			;;to run them
			(dormant -5)
			;;Anything else, no effect.
			(t 0))))
	       (undirty 'summary)
	       ;;Parent (if any) now needs to be resummarized.
	       (let
		  ((parent (emtvp:node->parent obj)))
		  (when parent
		     (new-dirty-node 'summary parent)))))

	 ((member 'notes dirty-flags)
	    (emtvr:collect-testral-2 obj tree)
	    (undirty 'notes))
	 ;;Other dirty flags are just cleared
	 (dirty-flags
	    (setq dirty-flags '())))))



;;;_  . emtv2:setup-if-needed
(defun emtv2:setup-if-needed ()
   ""
   (unless (buffer-live-p emtv2:report-buffer)
      (setq 
	 emtv2:report-buffer 
	 (generate-new-buffer
	    emtv2:report-buffer-name)))
   (emtvo:setup-if-needed #'emtv2:pathtree-cb #'ignore))

;;;_ , Static printing functions
;;;_  . emtv2:print-all
(defun emtv2:print-all (top-node)
   "Format and print the whole report."
   (emtvp:freshen emtvo:pathtree)
   (with-current-buffer emtv2:report-buffer
      (let
	 ((inhibit-read-only t))
	 (erase-buffer))
      (emtest/viewer/mode)
      (emtv2:insert top-node)))
;;;_  . emtv2:format-alist
(defconst emtv2:format-alist 
   (append
      '((dynamic emtv2:insert:dynamic))
      loformat:default-alist)
   
   "List of formatters that emformat uses.")

;;;_  . emtv2:insert
(defun emtv2:insert (top-node)
   "Insert TOP-NODE via loformat"
   (let*
      ((tree (emtvf:top top-node)))
      (loformat:insert
	 tree
	 emtv2:format-alist)))

;;;_ , Overall callback
;;;_  . emtv2:tests-outstanding
(defvar emtv2:tests-outstanding 0 
   "Number of tests currently enqueued that we haven't received
   reports from." )
;;;_  . emtv2:tester-cb
;;;###autoload
(defun emtv2:tester-cb (report)
   ""
   (check-type report emt:testral:report)
   (emtv2:setup-if-needed)
   (emtvo:receive report)
   (incf emtv2:tests-outstanding
      (-
	 (emt:testral:report->newly-pending report)
	 (length
	    (emt:testral:report->suites report))))
   
   (when
      (equal emtv2:tests-outstanding 0)
      (emtv2:print-all (emtvo:get-root))
      (pop-to-buffer emtv2:report-buffer)))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emviewer2)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer2.el ends here
