;;;_ emtest/runners/expect.el --- Expect-like testing external programs

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint,processes

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

(require 'tq)
(require 'emtest/types/testral-types)
(require 'emtest/testhelp/standard)
(require 'emtest/main/notes)
(progn
   (eval-when-compile
      (require 'emtest/testhelp/testpoint/requirer))
   (emtp:require))

;;;_. Body
;;;_ , Structures
;;Other data may be added here, such as for variables that cases
;;carry around.
(defstruct (emtr:expect-data
	      (:conc-name emtr:expect-data->)
	      (:constructor emtr:make-expect-data))
   "Data telling us how to run the interaction sequence"
   tq
   report-f
   timer
   pending
   prompt
   testral-obj
   (interaction-id () :doc
      "The parent note id of the current interaction")
   append-newline)

;;;_  . emtr:interact-predata
(defstruct (emtr:interact-predata
	      (:conc-name emtr:interact-predata->)
	      (:constructor emtr:make-interact-predata))
   "Data telling us how to run one interaction"
   question
   form
   timeout)
;;;_ , Configuration
(defconst emtr:expect-max-running 3 
   "Maximum number of expect interactions allowed to run at once" )
;;;_ , Variables

(defvar emtr:expect-num-running 0 
   "Number of expect interactions currently running" )
(defvar emtr:expect-queue '()
   "Queue of expect interactions that are waiting till something finishes" )

;;;_ , Utility
;;;_  . emtr:with-testral
(defmacro emtr:with-testral (obj &rest body)
   "Evaluate BODY in a testral context defined by OBJ.
OBJ must evaluate to an `emtr:expect-data'."
   
   `(emtt:testral:continued-with
      (emtr:expect-data->testral-obj ,obj)
      ,@body))

;;;_ , Support
;;;_  . emtr:expect-cb
(defun emtr:expect-cb (data raw-answer)
   "Callback for tq-enqueue"
   (when (first data)
      (emtt:testral:continued-with (second data)
	 (emth:protect&trap
	    aborted-p
	    (emtt:testral:with-parent-id
	       (emtr:expect-data->interaction-id (third data))
	       (let*
		  (  (endpos
			(progn
			   (string-match 
			      (concat 
				 (regexp-quote 
				    (emtr:expect-data->prompt 
				       (third data)))
				 "$")
			      raw-answer)
			   (match-beginning 0)))
		     (answer
			(substring raw-answer 0 endpos )))
		  (eval (first data))))
	    (when aborted-p 
	       (emtt:testral:add-note 
		  "problem"
		  'ungraded 
		  'failed (first data))))))

   (ignore-errors
      (cancel-timer
	 (emtr:expect-data->timer (third data))))
   (emtr:expect-start-next (third data)))

;;;_  . emtr:expect-timer-cb
(defun emtr:expect-timer-cb (data question)
   ""
   (ignore-errors
      (emtr:with-testral data
	 (emtt:testral:add-note
	    "problem"
	    'ungraded
	    'error-raised
	    '(timeout))))
   
   ;;Pop tq
   (tq-queue-pop (emtr:expect-data->tq data))
   ;;Start another.
   (emtr:expect-start-next data))


;;;_  . emtr:expect-start-next
(defun emtr:expect-start-next (data)
   ""
   
   (if
      (emtr:expect-data->pending data)
      (let
	 ((next
	     (pop (emtr:expect-data->pending data)))
	    timer)
	 (ignore-errors
	    (emtr:with-testral data
	       (let* 
		  ((id (emtt:testral:new-id)))
		  ;;Make a scope note for the next interaction
		  (emtt:testral:add-note-w/id id "trace" nil 'scope 
		     ;;$$IMPROVE ME  Make a nicer name, perhaps indexed.
		     "Interaction")
		  ;;Make a note of what we will send
		  (emtt:testral:with-parent-id id
		     (emtt:testral:add-note "param" nil
			'parameter
			'question
			(emtr:interact-predata->question next)))
		  ;;Store its parent note id
		  (setf (emtr:expect-data->interaction-id data) id))))
	 
	 ;;$$ENCAP ME.
	 (setf
	    (emtr:expect-data->timer data)
	    (run-at-time 
	       (emtr:interact-predata->timeout next)
	       nil
	       #'emtr:expect-timer-cb
	       data
	       (emtr:interact-predata->question next)))

	 (tq-enqueue (emtr:expect-data->tq data)
	    (let
	       ((ques (emtr:interact-predata->question next)))
	       (if (emtr:expect-data->append-newline data)
		  (concat ques "\n")
		  ques))
	    (emtr:expect-data->prompt data)
	    (list 
	       (emtr:interact-predata->form next) 
	       (emtr:expect-data->testral-obj data)
	       data)
	    #'emtr:expect-cb
	    nil))
      
      ;;Otherwise we're done.
      (progn
	 (decf emtr:expect-num-running)
	 ;;Report results
	 (funcall (emtr:expect-data->report-f data)
	    (emt:testral:make-suite
	       :contents
	       (emtr:with-testral data
		  (emtt:testral:note-list))
	       :grade 'test-case))
	 ;;Close tq
	 (tq-close (emtr:expect-data->tq data))
	 (when emtr:expect-queue
	    (let
	       ((args (pop emtr:expect-queue)))
	       (apply #'emtr:expect args))))))




;;;_  . emtr:expect-form->predata
(defun emtr:expect-form->predata (form)
   "Return an `emtr:interact-predata' made from FORM.
If impossible, return nil instead"
   (declare (special timeout))
   (case (car form)
      ((t)
	 (emtr:make-interact-predata
	    :question (second form)
	    :form 
	    (let* 
	       ((forms (cddr form)))
	       (if forms `(progn ,@forms) nil))
	    ;;$$IMPROVE ME Make `timeout' a special variable, rename
	    ;;it.  Get it from FORM if available.
	    :timeout timeout))
      ;;Dormant tests
      (quote
	 ;;Record what we did
	 (emtt:testral:add-note "problem" 
	    'dormant
	    'doc
	    "Dormant test")
	 ;;No interaction for it.
	 nil)

      (t
	 (emtt:testral:add-note "problem" 
	    'ungraded
	    'error-raised
	    `(unknown-governor ,(car form)))
	 nil)))



;;;_ , Entry point (for clause explorer)
;;;_  . emtr:expect
;;;###autoload
(defun emtr:expect (props form report-f)
   "Run a test-case on external program and report the result."
   (if
      (>= emtr:expect-num-running emtr:expect-max-running)
      (push (list props form report-f) emtr:expect-queue)
      (let
	 ((con
	     (emtt:testral:make-continuing props)))
	 ;;Testpoint to check what we receive.
	 (emth:protect&trap err
	    (emtp tp:96304f8f-2edc-4ac9-8ecb-9c6ad9ce0415 (form)
	       (let*
		  (  (form-parms (car form))
		     (exec+args
			(eval
			   (second (assq 'exec+args form-parms))))
		     (dummy
			(when (null exec+args)
			   (error "emtr:expect: no exec+args given")))
		     (dummy
			(when (not (stringp (car exec+args)))
			   (error "emtr:expect: exec is not a string")))
		     (dummy
			(when (not (file-name-absolute-p (car exec+args)))
			   (error "emtr:expect: path to exec is not absolute")))
		     (prompt
			(eval
			   (second (assq 'prompt    form-parms))))
		     (dummy
			(when (null prompt)
			   (error "emtr:expect: no prompt set")))
		     (shell
			;;Defaults to nil
			(eval
			   (second (assq 'shell     form-parms))))
		     (timeout
			;;Defaults to 30
			(or
			   (eval
			      (second (assq 'timeout form-parms)))
			   30))
		     (proc
			(apply 
			   (if shell
			      #'start-process-shell-command
			      #'start-process)
			   "expect" nil exec+args))
		     (dummy
			(unless
			   ;;0 status means a live process
			   (equal
			      (process-exit-status proc)
			      0)
			   (error
			      "emtr:expect: No live process")))
		     (tq
			(tq-create proc))
		     
		     (pending
			(emtt:testral:continued-with con
			   (delq nil
			      (mapcar
				 #'emtr:expect-form->predata
				 (cdr form)))))
		     (data
			(emtr:make-expect-data
			   :tq tq
			   :report-f report-f
			   ;;Timer is not set now, it will be set when we
			   ;;start
			   :timer nil 
			   ;;Will be set when we start.
			   :interaction-id nil 
			   :pending pending
			   :prompt prompt
			   :testral-obj con
			   :append-newline 
			   (second (assq 'append-newline form-parms)))))
		  
	       
		  ;;Start the testing
		  (incf emtr:expect-num-running)
		  (emtr:expect-start-next data)))
	 
	    (when err
	       (funcall report-f
		  (emt:testral:make-suite
		     :contents
		     (emtt:testral:continued-with con
			;;Make a note about the error
			(emtt:testral:add-note
			   "problem"
			   'ungraded
			   'error-raised
			   err)
			;;Then give all the notes.
			(emtt:testral:note-list))
		     :grade
		     'ungraded)))))))

;;;_ , Register it
;;;###autoload (eval-after-load 'emtest/main/all-runners
;;;###autoload '(emt:runner:add 'expect #'emtr:expect
;;;###autoload   "Expect script runner"))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runners/expect)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runners/expect.el ends here
