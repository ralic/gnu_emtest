;;;_ emtest/runner/runners/external.el --- Expect-like testing external programs

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



;;;_. Body
;;;_ , Structures
;;Other data may be added here, such as for variables that cases
;;carry around.
(defstruct (emtr:external-data
	      (:conc-name emtr:external-data->)
	      (:constructor emtr:make-external-data))
   "Data telling us how to run the interaction sequence"
   tq
   result-f
   timer
   pending
   prompt
   testral-obj)
;;;_  . emtr:interact-predata
(defstruct (emtr:interact-predata
	      (:conc-name emtr:interact-predata->)
	      (:constructor emtr:make-interact-predata))
   "Data telling us how to run one interaction"
   question
   form
   timeout)

;;;_ , Utility
;;;_  . emtr:external-1note
(defun emtr:external-1note (text data)
   ""
   ;;$$PUNT For now, as a doc note
   (emtt:testral:continued-with
      (emtr:external-data->testral-obj data)
      (emt:doc text)))

;;;_ , Support
;;;_  . emtr:external-cb
(defun emtr:external-cb (data answer)
   "Callback for tq-enqueue"
   (when (first data)
      (emtt:testral:continued-with (second data)
	 (emth:abortscope
	    aborted-p
	    (emth:trap-errors 
	       (eval (first data)))
	    t)))
   ;;$$TRANSITIONAL The `when' test is just for development.
   (when (third data)
      (ignore-errors
	 (cancel-timer
	    (emtr:external-data->timer (third data))))
      (emtr:external-start-next (third data))))

;;;_  . emtr:external-timer-cb
(defun emtr:external-timer-cb (data question)
   ""
   (emtr:external-1note 
      (concat 
	 "An interaction failed: " 
	 question)
      data)
   ;;Pop tq
   (tq-queue-pop (emtr:external-data->tq data))
   ;;Start another.
   (emtr:external-start-next data))


;;;_  . emtr:external-start-next
(defun emtr:external-start-next (data)
   ""
   
   (if
      (emtr:external-data->pending data)
      (let
	 ((next
	     (pop (emtr:external-data->pending data)))
	    timer)
	 ;;$$REPLACE WITH ME The `emtt:testral:continued-with' section
	 ;;isn't needed for most of this.
	 '(emtr:external-1note 
	     (emtr:interact-predata->question next)
	     data)
	 
	 (emtt:testral:continued-with 
	    (emtr:external-data->testral-obj data)
	    ;;Make a note of what the next thing is expected to send.
	    ;;$$PUNT For now, as a doc note.
	    (emt:doc (emtr:interact-predata->question next))
	    ;;$$ENCAP ME.
	    (setf
	       (emtr:external-data->timer data)
	       (run-at-time 
		  (emtr:interact-predata->timeout next)
		  nil
		  #'emtr:external-timer-cb
		  data
		  (emtr:interact-predata->question next)))

	    (tq-enqueue (emtr:external-data->tq data)
	       (emtr:interact-predata->question next)
	       (emtr:external-data->prompt data)
	       (list 
		  (emtr:interact-predata->form next) 
		  (emtr:external-data->testral-obj data)
		  data)
	       #'emtr:external-cb
	       nil)))
      
      ;;Untested
      (progn
	 ;;Otherwise report results
	 (funcall report-f
	    (emt:testral:make-suite
	       :contents
	       (emtt:testral:continued-with 
		  (emtr:external-data->testral-obj data)
		  (emtt:testral:get-notes))
	       :badnesses '()
	       :info '()))
	 ;;, close tq, and we're done.
	 (tq-close (emtr:external-data->tq data)))))

;;;_  . emtr:external-form->predata Form to predata
(defun emtr:external-form->predata (form)
   "Make a `emtr:interact-predata' from FORM."
   (declare (special timeout))
   (case (car form)
      ((t)
	 (emtr:make-interact-predata
	    :question (second form)
	    :form (cddr form)
	    ;;$$PUNT Take a timeout nicely.
	    :timeout timeout))
      ;;It will complain about the unknown governor when it's run.
      (t
	 (emtr:make-interact-predata
	    :question ""
	    ;;$$PUNT  Show what the governor is.
	    :form     '(emt:doc "unknown governor")
	    :timeout 0.0000001))))


;;;_ , Entry points (for clause explorer)
;;;_  . emtr:external
(defun emtr:external (props form report-f)
   "Run a test-case on external program and report the result."

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
		  30)))
      
	 (if
	    (or
	       (null exec+args)
	       (null prompt))
	    ;;Report bad test and why
	    (funcall report-f
	       (emt:testral:make-suite
		  :contents '() ;;$$PUNT:  No notes yet
		  :badnesses '(ungraded)
		  :info '()))
	    ;;Do test
	    (let* 
	       ((con
		   (emtt:testral:make-continuing))
		  (proc
		     (apply 
			(if shell
			   #'start-process
			   #'start-process-shell-command)
			 "external" nil exec+args))
		  (tq
		     (tq-create proc))
		  (pending
		     (mapcar
			#'emtr:external-form->predata
			(cdr form)))
		  (data
		     (emtr:make-external-data
			:tq tq
			:result-f result-f
			;;Timer is not set now, it will be set when we
			;;start
			:timer nil 
			:pending pending
			:prompt prompt
			:testral-obj con)))

	       ;;Start it all.
	       (emtr:external-start-next data))))))


;;;_  . Scratch area
;;Could also use start-process-shell-command but wildcards etc seem
;;unneeded. 
;;Make a fresh buffer
'
(setq my-buf nil
   ;(generate-new-buffer "external")
   )

'
(setq my-prog+args
   '("/bin/sh" "-i"))

'
(setq my-con
   (emtt:testral:make-continuing))

'
(setq my-proc
   (apply #'start-process "external" my-buf my-prog+args))

'
(setq my-tq
   (tq-create my-proc))

'
(setq my-data
   (emtr:make-external-data
      :tq my-tq
      :result-f 'no-result-f-yet
      :timer 'no-timer-yet
      :pending 
      (list
	 (emtr:make-interact-predata
	    :question 
	    "echo hello\n"
	    :form
	    '(progn
	       (emt:doc "Another note")
	       (assert (equal answer 5) t))
	    :timeout 
	    10)
	 (emtr:make-interact-predata
	    :question 
	    "echo hello again\n"
	    :form
	    '(progn
		(emt:doc "No test here"))
	    :timeout 
	    10))
      
      
      :prompt "% "
      :testral-obj my-con))

'
(emtr:external-start-next
   my-data)

'
(tq-enqueue my-tq 
    "PS1='% '\n"
    "% "
    (list '(emt:doc "This note should go OK") my-con)
    #'(lambda (data answer)
	 (emtt:testral:continued-with (second data)
	    (emt:doc "This note should go OK")))
   
    t)
'
my-con

'
(tq-enqueue my-tq 
   "echo hello\n"
   "% "
   (list 
      '(progn
	  (emt:doc "Another note")
	  (assert (equal answer 5) t))
      my-con )
   #'emtr:external-cb
   t)
'
my-con

' ;;No test.
(tq-enqueue my-tq 
   "echo hello\n"
   "% "
   (list nil my-con)
   #'emtr:external-cb
   t)

'  ;;Freeze up
(tq-enqueue my-tq 
    "x"
    "% "
    (list '(emt:doc "This note should not go thru") my-con)
   #'emtr:external-cb
    t)
'
(tq-queue-pop my-tq)

'
(setq my-freezing-data
   (emtr:make-external-data
      :tq my-tq
      :result-f 'no-result-f-yet
      :timer 'no-timer-yet
      :pending 
      (list
	 (emtr:make-interact-predata
	    :question 
	    "ec"
	    :form
	    '(progn
	       (emt:doc "Another note"))
	    :timeout 
	    10)
	 (emtr:make-interact-predata
	    :question 
	    "ho hello again\n"
	    :form
	    '(progn
		(emt:doc "We got here"))
	    :timeout 
	    1))
      
      
      :prompt "% "
      :testral-obj my-con))

'
(emtr:external-start-next
   my-freezing-data)

'
(run-at-time 
   1
   nil
   #'emtr:external-timer-cb
   my-freezing-data
   "This 302 string")

'
(emtt:testral:continued-with my-con
    (emtt:testral:get-notes))

;;Cleanup: 
'
(tq-close my-tq)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/runners/external)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/runners/external.el ends here
