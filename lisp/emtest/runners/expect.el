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
      "The parent note id of the current interaction"))

;;;_  . emtr:interact-predata
(defstruct (emtr:interact-predata
	      (:conc-name emtr:interact-predata->)
	      (:constructor emtr:make-interact-predata))
   "Data telling us how to run one interaction"
   question
   form
   timeout)

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
(defun emtr:expect-cb (data answer)
   "Callback for tq-enqueue"
   (when (first data)
      (emtt:testral:continued-with (second data)
	 (emth:protect&trap
	    aborted-p
	    (emtt:testral:with-parent-id
	       (emtr:expect-data->interaction-id (third data))
	       (eval (first data)))
	    (when aborted-p 
	       (emtt:testral:add-note 
		  "problem"
		  (emt:testral:make-grade:ungraded
		     :contents
		     "Interaction had an error") 
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
	    (emt:testral:make-grade:ungraded
	       :contents
	       "Interaction timed out")
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
		  (emtt:testral:add-note-w/id id "trace" nil 'scope)
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
	    (emtr:interact-predata->question next)
	    (emtr:expect-data->prompt data)
	    (list 
	       (emtr:interact-predata->form next) 
	       (emtr:expect-data->testral-obj data)
	       data)
	    #'emtr:expect-cb
	    nil))
      
      ;;Otherwise we're done.
      (progn
	 ;;Report results
	 (funcall (emtr:expect-data->report-f data)
	    (emt:testral:make-suite
	       :contents
	       (emtr:with-testral data
		  (emtt:testral:note-list))
	       :grade (emt:testral:make-grade:test-case)))
	 ;;Close tq
	 (tq-close (emtr:expect-data->tq data)))))


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
	    ;;$$PUNT Take a timeout nicely.
	    :timeout timeout))
      ;;Dormant tests
      (quote
	 ;;Record what we did
	 (emtt:testral:add-note "problem" 
	    (emt:testral:make-grade:dormant)
	    'doc
	    "Dormant test")
	 ;;No interaction for it.
	 nil)

      (t
	 (emtt:testral:add-note "problem" 
	    (emt:testral:make-grade:ungraded)
	    'error-raised
	    `(unknown-governor ,(car form)))
	 nil)))



;;;_ , Entry point (for clause explorer)
;;;_  . emtr:expect
;;;###autoload
(defun emtr:expect (props form report-f)
   "Run a test-case on external program and report the result."

   ;;$$IMPROVE ME  Catch errors and make a suite report anyways.

   ;;Testpoint to check what we receive.
   (emtp tp:96304f8f-2edc-4ac9-8ecb-9c6ad9ce0415
      (form)

      (let*
	 (  (form-parms (car form))
	    (exec+args
	       (second (assq 'exec+args form-parms)))
	    (prompt
	       (second (assq 'prompt    form-parms)))
	    (shell
	       (second (assq 'shell     form-parms)))
	    (timeout
	       (or
		  (second (assq 'timeout   form-parms))
		  30))
	    (emt:trace:properties props))
	 (declare (special emt:trace:properties))
	 (if
	    (or
	       (null exec+args)
	       (null prompt))
	    ;;Report bad test and why
	    (funcall report-f
	       (emt:testral:make-suite
		  :contents '() ;;$$PUNT:  No notes yet
		  :grade 
		  (emt:testral:make-grade:ungraded
		     :contents
		     (list
			"emtr:expect: no exec+args or no prompt"))))
	    ;;Do test
	    (let* 
	       ((con
		   (emtt:testral:make-continuing))
		  (proc
		     (apply 
			(if shell
			   #'start-process-shell-command
			   #'start-process)
			 "expect" nil exec+args))
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
			:testral-obj con)))

	       (if
		  ;;Sanity-check: We have a live process.
		  (equal
		     (process-exit-status (tq-process tq))
		     0)
		  ;;Start it
		  (emtr:expect-start-next data)
		  ;;Otherwise report on it.
		  ;;$$IMPROVE ME Make a sensible note
		  (funcall (emtr:expect-data->report-f data)
		     (emt:testral:make-suite
			:contents
			(emtr:with-testral data
			   (emtt:testral:note-list))
			:grade
			(emt:testral:make-grade:ungraded
			   :contents
			   (list
			      "emtr:expect: Don't have a live process")
			   )))))))))


;;;_ , Register it
;;$$TRANSITIONAL - belongs at the top of the file
;;;###autoload (unless (fboundp 'emtt:add-runner)
;;;###autoload   (error "A certain unwritten file must be loaded"))
;;;_  . Registration itself
;;;###autoload (emtt:add-runner 'expect #'emtr:expect
;;;###autoload   "Expect script runner") 
;;;_  . Provision
;;$$TRANSITIONAL
;;;###autoload (provide 'emtest/runners/registrations)


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runners/expect)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runners/expect.el ends here
