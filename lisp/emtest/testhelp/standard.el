;;;_ emtest/testhelp/standard.el --- Standard testhelp for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
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

(require 'emtest/common/testral-types)
(require 'emtest/common/result-types)
(require 'emtest/runner/testral)

;;;_ , Customization

(defgroup emtest/testhelp ()
   "Customization for Emtest testhelp"
   :group 'emtest)


;;;_. Body
;;;_ , Error types
;;;_  . Error type emt:already-handled
(put 'emt:already-handled 'error-conditions
   '(error emt:already-handled))
(put 'emt:already-handled 'error-message
   "This error has already been recorded in the diagnostic trace")
;;;_ , File location

;;;_  . emt:expand-filename-here
;;;###autoload
(defun emt:expand-filename-here (filename)
   ""
   (expand-file-name filename
      (if load-file-name
	 (file-name-directory 
	    (file-truename load-file-name)))))
;;For backward compatibility.
;;;###autoload
(defalias 'emtb:expand-filename-by-load-file 'emt:expand-filename-here)

;;;_ , Explicit notes
;;;_  . emt:doc
;;;###autoload
(defun emt:doc (str &rest r)
   ""
   (emtt:testral:add-note "doc" nil 'doc str))

;;;_  . emtt:testral:report-false
;;$$IMPROVE ME take args, str can be a format string
(defun emtt:testral:report-false (str)
   "Report that a compare leaf was false.
STR should be a string"
   (when (emtt:testral:p)
      (emtt:testral:add-note "trace" nil 'failed str)))

;;;_  . emt:stage
;;$$WRITE ME
(defmacro emt:stage (stage-args &rest body)
   "Run BODY in an Emtest stage"
   ;;Make a `scope' note and run body in 
   `(emtt:testral:with-parent-id id ,@body))

;;;_ , Error / retry management
;;;_  . emth:abortscope
;;$$IMPROVE ME  Encap emtt:*abort-p* in an uninterned symbol
(defmacro emth:abortscope (var body after)
   "Eval BODY, which may call `emth:trap-errors'
Then eval AFTER in a scope where VAR is bound to the boolean whether
there was any error inside a `emth:trap-errors'."
   
   `(let
       ((emtt:*abort-p* nil))
       ,body
       (let
	  ((,var emtt:*abort-p*))
	  ,after)))
;;;_  . emth:abortscope-other
;;Different logic: Set VAR just if there was an error not handled by
;;`emth:trap-errors'.  Not sure if this is better or worse.
'''' ;;$$TEST ME
(defmacro emth:abortscope-other (var body after)
   "Eval BODY, which may call `emth:trap-errors'
Then eval AFTER in a scope where VAR is bound to the boolean whether
there was any error inside a `emth:trap-errors'."
   
   `(let
       ((,var t))
       (unwind-protect
	  (setq  ,var
	     (condition-case nil
		(let
		   ((emtt:*abort-p* nil))
		   ,body
		   nil)
		('emt:already-handled nil)
		(error t)))
	  ,after)))


;;;_  . emth:trap-errors
(defmacro emth:trap-errors (&rest body)
   ""
   `(progn
       (declare (special emtt:*abort-p*))
       (condition-case err
	  (progn ,@body)
	  ('emt:already-handled
	     (setq emtt:*abort-p* t))
	  ;;$$ADD ME an error case for dormancy pseudo-errors.  It
	  ;;should push a dormancy note (here, not lower down, which
	  ;;may be somehow wrong?)
	  (error
	     (emtt:testral:add-note
		"problem" 
		(emt:testral:make-grade:ungraded
		   :contents 
		   "An error escaped to `emth:trap-errors'")
		'error-raised
		err)
	     (setq emtt:*abort-p* t)))))


;;;_  . emth:try-all
;;For now, it only watches for errors.  Doesn't try to logically
;;conjoin branches.
;;Early, I want it to map over the branches, so we can wrap map forms
;;in this.
;;$$TEST ME
;;$$USE ME  In particular, emtg:map should use emth:map&trap
(defmacro emth:try-all (&rest forms)
   "Error if any branch errors, but try all branches"
   (let
      ((form (make-symbol "form")))
      `(let
	  ((emtt:*abort-p* nil))
	  (declare (special emtt:*abort-p*))
	  (dolist (,form ',forms)
	     (emth:trap-errors ,form))
	  (when emtt:*abort-p*
	     (signal emt:already-handled ())))))

;;;_  . emth:map&trap
;;$$TEST ME
;;$$USE ME  In particular, emtg:map should use emth:map&trap
(defun emth:map&trap (func list)
   ""
   (declare (special emtt:*abort-p*))
   (let
      ((emtt:*abort-p* nil)
	 (results
	    (mapcar
	       #'(lambda (el)
		    (emth:trap-errors (funcall func el)))
	       list)))
      
      (if emtt:*abort-p*
	 (signal 'emt:already-handled ())
	 results)))

;;Usage: On forms, just (emth:map&trap #'eval form-list)
;;;_ , Assertions
;;;_  . emt:assert-f
;;`emt:assert-f' doesn't use `emth:trap-errors'.  If an error would
;;escape, the assert wasn't going to be meaningful anyways.
(defun emt:assert-f (form)
   "Worker function for macro `emt:assert'"
   
   (if 
      (emtt:testral:p)
      (let*
	 (  (id (emtt:testral:new-id))
	    (retval 
	       (emtt:testral:with-parent-id id
		  (eval form))))
	 (if retval
	    (emtt:testral:add-note-w/id
	       id
	       "trace"
	       nil
	       'succeeded
	       form)
	    (emtt:testral:add-note-w/id
	       id
	       "trace"
	       (emt:testral:make-grade:fail)
	       'failed
	       form))
	 retval)
      (eval `(assert ,form t))))

;;;_  . emt:assert
;;;###autoload
(defmacro emt:assert (form &rest r)
   ""
   `(emt:assert-f ',form))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/standard)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;;  emtest/testhelp/standard.el ends here

