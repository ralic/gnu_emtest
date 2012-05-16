;;;_ emtest/testhelp/testpoint.el --- Creating transparent test-points in code

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: 

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

;;Instead writing a normal `require', use command
;;`emtest:insert' and select `emt:insert--require-tp'.  It will insert
;;this special code:

;; (eval-when-compile
;;    (require 'emtest/testhelp/testpoint/requirer))
;; (emtp:require)

;;That means that if testpoint is not available, there is no error and
;;a testpoint will just eval the body forms.

;;;_ , Requires

(require 'utility/accumulator)

;;;_. Body

;;;_ , Globals
(defconst emtp:enable nil 
   "The global value is always `nil'." )

;;;_ , Non-global special variables
;;$$RETHINK ME Might be made a global again if we want nested
;;testpoints to be progressive.
'(defvar emtp:*handlers* nil "" )
;;;_ , Declarations
(declare (special emtp:*collected* emtp:*handlers* emtp:enable))

;;;_ , emtp
;;;###autoload
(defmacro emtp (id args &rest body)
   "A testpoint.
When not enabled, this simply runs BODY as if in a progn.
When enabled and there is a handler that matches ID, calls that
handler with arglist ARGS.
That handler can either fall thru to BODY or throw a return value to
`emtp:tag-return'."
   (declare 
      (debug (symbolp (&rest form) body)))
   `(catch 'emtp:tag-return
       (progn 
	  (declare (special emtp:*handlers*))
	  (when emtp:enable
	     ;;If `emtp:handle-call' uses a handler, it throws and
	     ;;does not return. 
	     (emtp:handle-call
		emtp:*handlers* ',id (list ,@args)))
	  ,@body)))

;;;_ , emtp:handle-call
(defun emtp:handle-call (handlers id args)
   ""
   (let
      ((cell (assoc id handlers)))
      (if cell
	 (let
	    ((func (second cell)))
	    (apply func args)))))

;;;_ , Error types
;;;_  . emtp:error (The general emtp error)

(put 'emtp:error 'error-conditions
   '(error emt:error emtp:error))
(put 'emtp:miscount 'error-message
   "emtp detected something wrong.")

;;;_  . emtp:miscount
(put 'emtp:miscount 'error-conditions
   '(error emt:error emtp:error emtp:miscount))
(put 'emtp:miscount 'error-message
   "emtp detected that something was not called exactly as
many times as expected")

;;;_ , Make clause forms
;;;_  . Accumulator type

(utiacc:define
   (emtp:clause-formdata 
      (:type list)
      (:copier nil)
      (:conc-name emtp:clause-formdata->)
      (:constructor emtp:make-clause-formdata))
   
   "The type that a clause-maker returns"
   ;;Elements as (symbol lambda-form)
   handler 
   mock-func
   (reached-counter () :type (list symbol integer))
   final-form ;;A lambda
   ;;Assumes we collect into a list.  For now we always do.
   collector-num-slot
   return-finally
   finally-data)


;;;_  . Worker
;;;_   , emtp:make-clause-form-x
(defun emtp:make-clause-form-x 
   (id args body &optional fallthru count bindings mock)
   ""
   (let
      ((counter
	  (when count (emtp:make-counter count id))))

      (emtp:make-clause-formdata
	 ;;Choose which field we put this into by choosing keyword.
	 (if mock :mock-func-SINGLE :handler-SINGLE)
	 (list  
	    id
	    (eval
	       `#'(lambda (,@args)
		     (let ,bindings
			,(when counter 
			    (emtp:counter:incf-form counter))
			,(if fallthru
			    `(progn ,@body)
			    `(throw 'emtp:tag-return
				(progn ,@body)))))))
	       
	 :reached-counter-LIST
	 (if counter (list counter) ()))))


;;;_  . Handle specific governors

;;;_   , emtp:make-clause-form-tp*
(defun* emtp:make-clause-form-tp*
   ((&key id fallthru (count 1) bindings) args &rest body)
   ""
   (emtp:make-clause-form-x id args body fallthru count bindings))

;;;_   , emtp:make-clause-form-tp
(defun emtp:make-clause-form-tp (id args &rest body)
   ""
   (emtp:make-clause-form-x id args body nil 1))

;;;_   , emtp:make-clause-form-tp-reached
(defun emtp:make-clause-form-tp-reached (id count)
   ""
   (emtp:make-clause-form-x id '(&rest dummy) nil t count))
;;;_   , emtp:make-clause-form-mock*
(defun* emtp:make-clause-form-mock* 
   ((&key symbol (count 1)) &optional args-dummy &rest body)
   ""
   (emtp:make-clause-form-x 
       symbol '(&rest rest) body t count () t))

;;;_   , emtp:make-clause-form-finally
(defun* emtp:make-clause-form-finally 
   ((&key bindings return) arglist &rest form)
   ""
   (emtp:make-clause-formdata
      :finally-data-SINGLE 
      (list
	 ;;Form
	 (eval
	    `#'(lambda (,@arglist) (let ,bindings ,@form)))
	 ;;number of slots
	 (length arglist)
	 ;;Whether to return the `finally' value
	 return)
      :return-finally-SINGLE 
      return
      :final-form-SINGLE
      (eval
	 `#'(lambda (,@arglist) (let ,bindings ,@form)))
      :collector-num-slot-SINGLE  
      (length arglist)))


;;;_  . Dispatch clause form maker
;;;_   , emtp:make-clause-form
(defun emtp:make-clause-form (governor &rest r)
   ""

   (case governor

      (tp*
	 (apply #'emtp:make-clause-form-tp* r))

      (tp
	 (apply #'emtp:make-clause-form-tp r))
      (tp-reached
	 (apply #'emtp:make-clause-form-tp-reached r))
      (mock*
	 (apply #'emtp:make-clause-form-mock* r))
      (finally
	 (apply #'emtp:make-clause-form-finally r))
      (t
	 (error "No such testpoint clause governor: %s" governor))))



;;;_  . emtp:get-combined-clause-forms
(defun emtp:get-combined-clause-forms (testpoint-forms)
   ""

   (utiacc:collect 
      #'emtp:make-clause-form
      testpoint-forms
      'emtp:clause-formdata))


;;;_ , Counters
;;;_  . Structure (Implied)
;;;_  . Ctor emtp:make-counter
(defun emtp:make-counter (reached id)
   ""
   (list (gensym) reached id))

;;;_  . To increment form emtp:counter:incf-form
(defun emtp:counter:incf-form (counter)
   ""
   `(incf ,(car counter)))

;;;_  . To binding-form emtp:counter:binding-form
(defun emtp:counter:binding-form (counter)
   ""
   `(,(car counter) 0))

;;;_  . To check form emtp:counter:check-form
(defun emtp:counter:check-form (counter)
   ""
   `(unless (equal ,(car counter) ,(cadr counter))
       (signal 'emtp:miscount
	  (list
	     (format
		"Testpoint %s was reached %d times (expected %d)"
		',(caddr counter)
		,(car counter)
		,(cadr counter))))))



;;;_ , emtp:eval
;;;###autoload
(defmacro emtp:eval (form &rest testpoint-forms)
   ""
   (let*
      ((all-formdata
	  (emtp:get-combined-clause-forms testpoint-forms))
	 (counter-data
	    (emtp:clause-formdata->reached-counter-LIST
	       all-formdata))
	 (finally-data-list
	    (emtp:clause-formdata->finally-data-LIST
	       all-formdata))
	 (old-funcs-sym (gensym "emtp"))
	 (seen-err (gensym "emtp")))

      ;;Sanity checks
      (when (> (length finally-data-list) 1)
	 (error "Only one final form is allowed"))

      ;;The form we make.
      `(let (  (emtp:enable t)
	       (emtp:*collected* ())
	       (,seen-err nil)
	       (emtp:*handlers*
		  ',(emtp:clause-formdata->handler-LIST
		       all-formdata))
	       
	       ;;Rebind functions.
	       (,old-funcs-sym 
		  (emtp:bind-funcs
		     ',(emtp:clause-formdata->mock-func-LIST
			  all-formdata)))
	       
	       ;;Bind counters to initial values
	       ,@(mapcar
		    #'emtp:counter:binding-form
		    counter-data))

	  (unwind-protect
	     (condition-case err
		,(if finally-data-list
		    (destructuring-bind 
		       (final-form collector-num-slots return-finally)
		       (car finally-data-list)
		       (check-type collector-num-slots integer)
		       (let
			  ((finally-form
			      `(apply 
				  ,final-form
				  (utiacc:transpose 
				     emtp:*collected*
				     ,collector-num-slots
				     #'nth))))
			  (if return-finally 
			     `(progn
				 ,form
				 ,finally-form)
			     `(prog1
				 ,form
				 ,finally-form))))
		    form)
		(error
		   (setq ,seen-err t)
		   ;;Re-raise the signal
		   (signal (car err)(cdr err))))

	     (emtp:bind-funcs ,old-funcs-sym)
	     (unless ,seen-err
		,@(mapcar
		     #'emtp:counter:check-form
		     counter-data))))))

;;;_  . Helper emtp:bind-funcs
(defun emtp:bind-funcs (data-list)
   "Rebind the functions in DATA-LIST.
Each element of DATA-LIST has the form (SYMBOL DEFINITION).
Return a list of the old bindings in that form."

   (mapcar
      #'(lambda (datum)
	   (destructuring-bind (symbol definition) datum
	      (prog1
		 (list 
		    symbol 
		    (if (fboundp symbol)
		       (symbol-function symbol)
		       nil))
		 (when definition
		    (ad-safe-fset symbol definition)))))
      data-list))



;;;_ , emt:tp:collect
;;;###autoload
(defun emt:tp:collect (&rest args)
   "Only meaningful inside `emtp:eval'"
   (callf append emtp:*collected* (list (mapcar #'list args))))
;;;_ , emtp:insulate
(defmacro emtp:insulate (funcs &rest body)
   "Arrange that calling FUNCS during BODY has no effect or a
   controlled effect.
FUNCS is a list whose elements are each:
 * a symbol, and the func just returns `nil'
 * a list, which is interpreted as if governed by `mock*'."
   
   `(emtp:eval
       (progn ,@body)
       ,@(mapcar
	    #'(lambda (f)
		 (if (symbolp f)
		    `(mock* (:symbol ,f))
		    `(mock* ,@f)))
	    funcs)))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/testpoint)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/testpoint.el ends here
