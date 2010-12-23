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

;; Borrowed from emviewer, only needs differentiate a few things.
;; They ought to share code.


;;;_ , Requires


(require 'emtest/common/testral-types)
(require 'emtest/viewer/emformat)
(require 'emtest/viewer/organize)
(require 'emtest/viewer/empathtree)
(require 'emtest/viewer/view-types)
(require 'viewers/loformat)
(require 'emtest/viewer/mode)

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
	       (emtvr:cache-subtree-badnesses obj)
	       (undirty 'summary)
	       ;;Parent (if any) now needs to be resummarized.
	       (let
		  ((parent (emtvp:node->parent obj)))
		  (when parent
		     (new-dirty-node 'summary parent)))))

	 ((member 'notes dirty-flags)
	    (when (emt:view:suite-newstyle-p obj)
	       (let
		  ((result
		      (emt:view:suite-newstyle->result obj)))
		  (when (emt:testral:suite-p result)
		     (let
			((contents
			    (emt:testral:suite->contents result)))
			(when
			   (emt:testral:note-list-p contents)
			   (emtvr:collect-testral contents tree obj '()))))))
	    (undirty 'notes)))))


;;;_  . emtv2:setup-if-needed
(defun emtv2:setup-if-needed ()
   ""
   (unless (buffer-live-p emtv2:report-buffer)
      (setq 
	 emtv2:report-buffer 
	 (generate-new-buffer
	    emtv2:report-buffer-name)))
   (emtvo:setup-if-needed #'emtv2:pathtree-cb #'ignore))

;;;_ , Pseudo-dynamic
;;$$MOVE ME later into viewer package, after the vars are
;;a parameter, not a constant.
;;;_  . emtv2:dynamic:vars
(defconst emtv2:dynamic:vars 
   '(emtvf:*outline-depth* emtvf:*folded*)
   "Special variables that the formatters use.
These variables propagate thru `dynamic' bindings." )
;;;_  . emtv2:dynamic:init-forms
(defconst emtv2:dynamic:init-forms 
   '(0 nil)
   "Init forms for the special variables." )
;;;_  . emtv2:dynamic-register-var
(defun emtv2:dynamic-register-var (sym init-form)
   "PUNTED.  Register SYM as a special variable for `dynamic'.
If it's already register, just change its init form."

   (let*
      ()
      
      ))
;;;_  . emtv2:dynamic:with
(defmacro emtv2:dynamic:with-vars (body)
   "UNTESTED.  Even BODY with the special variables bound."

   `(progv emtv2:dynamic-vars (list ,@emtv2:dynamic:init-forms)
       ,@body))

;;;_  . emtv2:dynamic-capture-vars
(defun emtv2:dynamic-capture-vars ()
   "Capture the values of the formatter special variables."

   ;;Capture the special variables.
   (eval `(list ,@emtv2:dynamic-vars)))
;;;_  . emtv2:insert-dynamic
(defun emtv2:insert-dynamic (recurse-f obj loal func &optional data)
   "Insert (statically) the result of a dynamic spec"
   (let*
      ((fmt-list 
	  (progv emtv2:dynamic-vars data
	     (funcall func obj loal))))
      (funcall recurse-f fmt-list)))

;;;_ , Static printing functions
;;;_  . emtv2:print-all
(defun emtv2:print-all (top-node)
   ""
   (with-current-buffer emtv2:report-buffer
      (let
	 ((inhibit-read-only t))
	 (erase-buffer))
      (emtest/viewer/mode)
      (emtvf:insert top-node '() '((dynamic emtv2:insert-dynamic)))))


;;;_ , Overall callback
;;;_  . emtv2:tests-outstanding
(defvar emtv2:tests-outstanding 0 
   "Number of tests currently enqueued that we haven't received
   reports from." )
;;;_  . emtv2:tester-cb

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
      (or
	 (emt:testral:report->run-done-p report)
	 (equal emtv2:tests-outstanding 0))
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
