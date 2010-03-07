;;;_ tester/emt-funcall.el --- Provide emt:funcall

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

;; 


;;;_ , Requires
(unless (fboundp 'emt:deftest)
   (defmacro emt:deftest (&rest dummy))
   (defmacro emt:if-avail (&rest dummy)))

'(eval-when-compile
   (require 'cl))
(require 'common/result-types)
(emt:if-avail
   (require 'tester/testhelp/eg))

;;;_. Body

;;;_ , Variables
(defvar emt:trace:stored-diag ()
   "Global value for stored diagnostics.
Each element should be a `emt:result:diag:bool'" )


;;;_ , Validation support
(defun emt:trace:stored-diag:check-type ()
   ""
   ;;Can't require it at eval time or there'd be mutual dependence.
   (require 'tester/testhelp/deep-type-checker)
   (emty:check
      emt:trace:stored-diag
      (list
	 emt:result:diag:bool)))

;;;_ , Error type emt:already-handled
(put 'emt:already-handled 'error-conditions
   '(error emt:already-handled))
(put 'emt:already-handled 'error-message
   "This error has already been recorded in the diagnostic trace")


;;;_ , emt:trace:protect
(defmacro emt:trace:protect (main &rest protected)
   "Like `unwind-protect' but also groups diagnostic info and
info-about for the enclosed scope.

Runs MAIN, then even if main throws or signals, runs each clause in
PROTECTED.

PROTECTED has access to these variables:
 * `diags' which is the diagnostics in the order they were added.
 * `info-about', which is the info-about list.

Generally both should be placed into some sort of report structure."
   
   `(let (  (emt:trace:stored-diag ())
	    (emt:trace:info-about ()))
       (unwind-protect
	  ,main
	  (let
	     ((diags (reverse emt:trace:stored-diag))
		(info-about emt:trace:info-about))
	     ,@protected))))

;;;_ , emt:trace:add-to-stored-diag
;;These could check type and if type doesn't match
;;`emt:result:diag:bool', store a report that it doesn't match.
;;This could just use `ring'
(defun emt:trace:add-to-stored-diag (report)
   "Add reports to stored diagnostics, global version.
Trim the list to some maximum size."
   (let
      ((len (length emt:trace:stored-diag)))
   (if
      (> len 5)
      (setq emt:trace:stored-diag
	 (cons report
	    (nbutlast emt:trace:stored-diag (- len 5))))
      
      (push report emt:trace:stored-diag))))

;;;_ , emt:trace:add-to-stored-diag-keep-all
(defun emt:trace:add-to-stored-diag-keep-all (report)
   "Add reports to stored diagnostics, tester version.
Not called by name, it is flet by tester."
   (push report emt:trace:stored-diag))


;;;_ , emt:funcall-handle-persistence

(defun emt:funcall-handle-persistence (args)
   ""
   ;;If persist is not loaded, there can be no placeholder to even
   ;;find.
   (if (featurep 'emt-persist)
      (emt:funcall-handle-persistence-x args)
      (list 'correct-answer args)))

;;;_ , emt:funcall-x
(defun emt:funcall-x (func args)
   ""

   (destructuring-bind (relation new-args)
      (emt:funcall-handle-persistence args)
      (case relation
	 ((correct-answer)
	    (apply func new-args))
	 ((wrong-answer)
	    (not
	       (apply func new-args))))))

;;;_ , emt:funcall

(defun emt:funcall (func &rest args)
   ""

   (let
      ((report 
	  (make-emt:result:diag:call
	     :call-sexp (list* func args)))
	 (emt:trace:info-about ())
	 (emt:trace:tried ()))
      
      (unwind-protect
	 (condition-case err
	    (let
	       (
		  (retval
		     (emt:funcall-x func args)))
	       
	       (setf (emt:result:diag:call-status report) 
		  (if retval t nil))
	       retval)
	    (error
	       (setf (emt:result:diag:call-status report) 'error)
	       (signal (car err)(cdr err))))
	 
	 (setf (emt:result:diag:call-info-about report) 
	    emt:trace:info-about)
	 (setf (emt:result:diag:call-tried report) 
	    emt:trace:tried)
	 (emt:trace:add-to-stored-diag report))))


;;;_. Footers
;;;_ , Provides

(provide 'tester/emt-funcall)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/emt-funcall.el ends here
