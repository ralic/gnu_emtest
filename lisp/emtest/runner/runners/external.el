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
      (emtr:external-start-next (third data))))


;;;_  . emtr:external-start-next
(defun emtr:external-start-next (data)
   ""
   
   (if
      (emtr:external-data->pending data)
      (let
	 ((next
	     (pop (emtr:external-data->pending data)))
	    timer)
	 (emtt:testral:continued-with 
	    (emtr:external-data->testral-obj data)
	    ;;Make a note of what the next thing is expected to send.
	    ;;$$PUNT For now, as a doc note.
	    (emt:doc (emtr:interact-predata->question next))
	    (setf
	       (emtr:external-data->timer data)
	       ;;$$PUNT 
	       '(run-at-time 
		   (emtr:interact-predata->timeout next)
		   nil
		   #'(lambda (data)
			;;Note a fail
			;;Pop tq
			;;Start another.
			(emtr:external-start-next data))
		   data))

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
      '(progn
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



;;;_ , Entry points (for clause explorer)
;;;_  . emtr:external
(defun emtr:external (props form report-f)
   "Run a test-case on external program and report the result."

   ;;$$PUNT for now
   (let*
      ()
      
      ))
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
