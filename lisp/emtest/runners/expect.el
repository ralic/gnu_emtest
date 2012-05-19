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
(defstruct (emt:xp:expect:data
	      (:conc-name emt:xp:expect:data->)
	      (:constructor emt:xp:expect:make-expect-data))
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

;;;_  . emt:xp:expect:interact-predata
(defstruct (emt:xp:expect:interact-predata
	      (:conc-name emt:xp:expect:interact-predata->)
	      (:constructor emt:xp:expect:make-interact-predata))
   "Data telling us how to run one interaction"
   question
   form
   timeout)
;;;_ , Configuration
(defconst emt:xp:expect:max-running 1
   "Maximum number of expect interactions allowed to run at once" )
;;;_ , Variables

(defvar emt:xp:expect:num-running 0 
   "Number of expect interactions currently running" )
(defvar emt:xp:expect:queue '()
   "Queue of expect interactions that are waiting till something finishes" )

;;;_ , Utility
;;;_  . emt:xp:expect:with-testral
(defmacro emt:xp:expect:with-testral (obj &rest body)
   "Evaluate BODY in a testral context defined by OBJ.
OBJ must evaluate to an `emt:xp:expect:data'."
   
   `(emtt:testral:continued-with
      (emt:xp:expect:data->testral-obj ,obj)
      ,@body))

;;;_ , Support
;;;_  . emt:xp:expect:cb
(defun emt:xp:expect:cb (data raw-answer)
   "Callback for tq-enqueue"
   (when (first data)
      (emtt:testral:continued-with (second data)
	 (emth:protect&trap
	    aborted-p
	    (emtt:testral:with-parent-id
	       (emt:xp:expect:data->interaction-id (third data))
	       (let*
		  (  (endpos
			(progn
			   (string-match 
			      (concat 
				 (regexp-quote 
				    (emt:xp:expect:data->prompt 
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
	 (emt:xp:expect:data->timer (third data))))
   (emt:xp:expect:start-next (third data)))

;;;_  . emt:xp:expect:timer-cb
(defun emt:xp:expect:timer-cb (data question)
   ""
   (ignore-errors
      (emt:xp:expect:with-testral data
	 (emtt:testral:add-note
	    "problem"
	    'ungraded
	    'error-raised
	    '(timeout))))
   
   ;;Pop tq
   (tq-queue-pop (emt:xp:expect:data->tq data))

   ;;$$IMPROVE ME  Optionally start another. `emt:xp:expect:start-next'
   (emt:xp:expect:done data))

;;;_  . emt:xp:expect:done
(defun emt:xp:expect:done (data)
   "Finish an expect session"
   
   (decf emt:xp:expect:num-running)
   ;;Report results
   (funcall (emt:xp:expect:data->report-f data)
      (emt:testral:make-suite
	 :contents
	 (emt:xp:expect:with-testral data
	    (emtt:testral:note-list))
	 :grade 'test-case))
   ;;Close tq
   (tq-close (emt:xp:expect:data->tq data))
   (when emt:xp:expect:queue
      (let
	 ((args (pop emt:xp:expect:queue)))
	 (apply #'emt:xp:expect:expect args))))

;;;_  . emt:xp:expect:start-next
(defun emt:xp:expect:start-next (data)
   ""
   
   (if
      (emt:xp:expect:data->pending data)
      (let
	 ((next
	     (pop (emt:xp:expect:data->pending data)))
	    timer)
	 (ignore-errors
	    (emt:xp:expect:with-testral data
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
			(emt:xp:expect:interact-predata->question next)))
		  ;;Store its parent note id
		  (setf (emt:xp:expect:data->interaction-id data) id))))
	 
	 ;;$$ENCAP ME.
	 (setf
	    (emt:xp:expect:data->timer data)
	    (run-at-time 
	       (emt:xp:expect:interact-predata->timeout next)
	       nil
	       #'emt:xp:expect:timer-cb
	       data
	       (emt:xp:expect:interact-predata->question next)))

	 (tq-enqueue (emt:xp:expect:data->tq data)
	    (let
	       ((ques (emt:xp:expect:interact-predata->question next)))
	       (if (emt:xp:expect:data->append-newline data)
		  (concat ques "\n")
		  ques))
	    (emt:xp:expect:data->prompt data)
	    (list 
	       (emt:xp:expect:interact-predata->form next) 
	       (emt:xp:expect:data->testral-obj data)
	       data)
	    #'emt:xp:expect:cb
	    nil))
      
      ;;Otherwise we're done.
      (emt:xp:expect:done data)))




;;;_  . emt:xp:expect:form->predata
(defun emt:xp:expect:form->predata (form)
   "Return an `emt:xp:expect:interact-predata' made from FORM.
If impossible, return nil instead"
   (declare (special timeout))
   (case (car form)
      ((t)
	 (emt:xp:expect:make-interact-predata
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
;;;_  . emt:xp:expect:expect
;;;###autoload
(defun emt:xp:expect:expect (props form report-f)
   "Run a test-case on external program and report the result."
   (if
      (>= emt:xp:expect:num-running emt:xp:expect:max-running)
      (push (list props form report-f) emt:xp:expect:queue)
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
			   (error "emt:xp:expect:expect: no exec+args given")))
		     (dummy
			(when (not (stringp (car exec+args)))
			   (error "emt:xp:expect:expect: exec is not a string")))
		     (dummy
			(when (not (file-name-absolute-p (car exec+args)))
			   (error "emt:xp:expect:expect: path to exec is not absolute")))
		     (prompt
			(eval
			   (second (assq 'prompt    form-parms))))
		     (dummy
			(when (null prompt)
			   (error "emt:xp:expect:expect: no prompt set")))
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
			      "emt:xp:expect:expect: No live process")))
		     (tq
			(tq-create proc))
		     
		     (pending
			(emtt:testral:continued-with con
			   (delq nil
			      (mapcar
				 #'emt:xp:expect:form->predata
				 (cdr form)))))
		     (data
			(emt:xp:expect:make-expect-data
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
		  (incf emt:xp:expect:num-running)
		  (emt:xp:expect:start-next data)))
	 
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
;;;###autoload '(emt:runner:add 'expect #'emt:xp:expect:expect
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
