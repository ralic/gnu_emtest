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

(require 'emtest/types/testral-types)
(require 'emtest/types/run-types)
(require 'emtest/main/notes)

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
;;;###autoload
(defun emtt:testral:report-false (str)
   "Report that a compare leaf was false.
STR should be a string"
   (when (emtt:testral:p)
      (emtt:testral:add-note "trace" nil 'matched str)))

;;;_  . emt:stage
;;;###autoload
(defmacro emt:stage (stage-args &rest body)
   "Run BODY in an Emtest stage"
   ;;$$IMPROVE ME  Use `emt:testral:with-parent-note'
   (let
      ((id (make-symbol "id")))
      `(let ((,id (emtt:testral:new-id)))
	  (emtt:testral:add-note-w/id id "trace" nil 'scope)
	  (emtt:testral:with-parent-id ,id ,@body))))

;;;_ , Error / retry management
;;;_  . emth:protect&trap
(defmacro emth:protect&trap (var body after)
   "Eval BODY, then eval AFTER.

Eval AFTER with VAR bound to the boolean whether an error escaped
BODY, other than an error of type `emt:already-handled'."
   
   (let ((err (make-symbol "err")))
      `(let
	  ;;Initialize VAR to `escaped-by-throw' because the only way
	  ;;it can escape being set by the setq below is if code
	  ;;throws out of the setq call.
	  ((,var 'escaped-by-throw))
	  (unwind-protect
	     (setq  ,var
		(condition-case ,err
		   (progn
		      ,body
		      nil)
		   ('emt:already-handled nil)
		   (error ,err)))
	     ,after))))

;;;_  . emth:trap-errors-aux
(defmacro emth:trap-errors-aux (var body form)
   "Eval BODY, trapping errors.
If an error is seen, eval FORM with VAR bound to the error object.
If the error is `emt:already-handled', just return `nil'."
   `(progn
       (condition-case ,var
	  ,body
	  ('emt:already-handled nil)
	  ;;$$IMPROVE ME an error case for dormancy pseudo-errors.  It
	  ;;should push a dormancy note (here, not lower down, which
	  ;;may be somehow wrong?)
	  (error
	     ,form))))

;;;_  . emth:trap-errors
;;;###autoload
(defmacro emth:trap-errors (&rest body)
   "Eval BODY, trapping errors.
If an error is seen, make a TESTRAL note of it.
If the error is `emt:already-handled', just return `nil'."
   `(emth:trap-errors-aux err 
       (progn ,@body)
       (emtt:testral:add-note
	  "problem" 
	  'ungraded
	  'error-raised
	  err)))



;;;_  . emth:try-all
;;$$TEST ME
;;;###autoload
(defmacro emth:try-all (&rest forms)
   "Eval each form.
When done, if any branch erred, raise a single error"
   (let
      ((form (make-symbol "form"))
	 (aborted-p (make-symbol "aborted-p")))
      `(let
	  ((,aborted-p nil))
	  (dolist (,form ',forms)
	     (emth:trap-errors-aux 
		nil 
		,form
		(setq ,aborted-p t)))
	  (when ,aborted-p
	     (signal 'emt:already-handled ())))))

;;;_  . emth:map&trap
;;;###autoload
(defun emth:map&trap (func list)
   "Map FUNC of LIST, returning a list of the results.
If errors are seen, raise a single error instead."
   (let
      ((emtt:*aborted-p* nil)
	 (results
	    (mapcar
	       #'(lambda (el)
		    (emth:trap-errors-aux 
		       nil
		       (funcall func el)
		       (setq emtt:*aborted-p* t)))
	       list)))
      
      (if emtt:*aborted-p*
	 (signal 'emt:already-handled ())
	 results)))

;;Usage: On forms, just (emth:map&trap #'eval form-list)
;;;_ , Assertions
;;;_  . emt:assert-react-to-error
(defun emt:assert-report-error (id form)
   "React to an error in assert"

   (emtt:testral:add-note-w/id
      id
      "trace"
      'ungraded
      ;;$$IMPROVE ME Make something specific for this.
      'failed
      form)
   
   (signal 'emt:already-handled ()))
;;;_  . emt:analytic-eval
(defun emt:analytic-eval (form)
   "Evaluate FORM and record analytic notes.
Only make notes if form calls a true function, not a macro or subr."
   ;;$$IMPROVE ME Don't capture any variables, including `form',
   ;;`arg', and `val'.  This may not be reasonably possible.
   (if
      ;;If it's a true function call, analyze it.
      (and
	 (listp form)
	 (functionp (car form))
	 (not (subrp (car form))))
      (apply (car form)
	 (mapcar
	    #'(lambda (arg)
		 (let
		    ((val (eval arg)))
		    ;;$$IMPROVE ME Recurse if expression is a call.
		    (unless
		       ;;Don't make notes for constant args, they are
		       ;;not useful.
		       (or
			  (eq val arg)
			  (and
			     (consp arg)
			     (memq (car arg)
				'(quote function function*))))
		       (emtt:testral:add-note
			  "param" nil 'parameter
			  arg val))
		    val))
	    (cdr form)))
      ;;Otherwise just eval it.
      (eval form)))

;;;_  . emt:assert-f
;;`emt:assert-f' doesn't use `emth:trap-errors' nor
;;`emth:protect&trap' because they want to handle control differently
;;than we need.
(defun emt:assert-f (form)
   "Worker function for macro `emt:assert'"
   
   (if 
      (emtt:testral:p)
      (let
	 ;;In every case, make a note with this id so that notes
	 ;;inside our scope are not orphaned.
	 ((id (emtt:testral:new-id)))
	 (condition-case err
	    (let*
	       (  
		  (retval 
		     (emtt:testral:with-parent-id id
			(emt:analytic-eval form))))
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
		     'fail
		     'failed
		     form))
	       retval)
	    ('emt:already-handled
	       (emt:assert-report-error id form))
	    (error
	       (emtt:testral:with-parent-id id
		  (emtt:testral:add-note
		     "problem" 
		     'ungraded
		     'error-raised
		     err))
	       (emt:assert-report-error id form))))
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

