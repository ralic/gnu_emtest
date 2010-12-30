;;;_ utility/pending.el --- Pending: Manage pending operations

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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

;; Manage pending operations on a list of elements.  Allow operations
;; to dynamically be delayed, so that for instance inter-element
;; dependencies work.


;;;_ , Requires



;;;_. Body

;;;_ , pending:do-all-f
(defun pending:do-all-f (init func args error-args-f &optional expand-p)
   "INIT is the initial list of pending objects to process.  
You probably want to set INIT to the empy list after this call returns.

FUNC is a function that:
 * takes a member of the list
 * takes ARGS as rest
 * returns a list of elements that should be (re)tried later.

ERROR-ARGS-F is a function that:
 * takes the final list of unprocessed elements
 * takes ARGS
 * returns data suitable for the second arg of `signal'."
   (let*
      (  (delayed ())
	 (pending-list init)
	 (last-num-left (length init)))

      (while pending-list
	 ;;Process pending elements
	 (dolist (v pending-list)
	    (let
	       ((new-els
		   (apply func v args)))
	       (when new-els
		  (setf delayed
		     (append new-els delayed)))))
	 
	 (let
	    ((num-left (length delayed)))
	    (cond
	       ;;All done.
	       ((= num-left 0)
		  (setq pending-list ()))
	       ;;Not done but making progress, or we allow the pending
	       ;;list to expand.  Set up to try again.
	       ((or expand-p (< num-left last-num-left))
		  (setq last-num-left num-left)
		  (setq pending-list delayed)
		  (setq delayed ()))
	       ;;Not even making progress, and not allowed to expand.
	       (t
		  (signal 'error
		     (apply error-args-f delayed args))))))))
;;;_ , pending:do-all
(defmacro pending:do-all (init func args error-args-f &optional expand-p)
   "Forwards to pending:do-all-f.
Clears the pending list when done."
   
   `(progn
       (pending:do-all-f
	  ,init ,func ,args ,error-args-f ,expand-p)
       ;;Empty the list for the caller.
       (setf ,init '())))

;;;_  . Test support
;;;_   , pending:terminates-on-examples

(defun pending:terminates-on-examples (func args &rest examples)
   ""
   ;;Shall we trap errors so we always return a boolean?  Or shall the
   ;;error be allowed to carry the information?
   (pending:do-all-f examples func args 
      #'(lambda (unprocessed &rest args)
	   (format "Didn't terminate.  Remains: %S" unprocessed)))
   t)

;;;_. Footers
;;;_ , Provides

(provide 'utility/pending)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/pending.el ends here
