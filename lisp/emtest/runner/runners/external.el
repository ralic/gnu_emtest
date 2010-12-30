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

(require 'tq)
(require 'emtest/common/testral-types)
(require 'emtest/testhelp/standard)
(require 'emtest/runner/testral)
(progn
   (eval-when-compile
      (require 'emtest/testhelp/testpoint/requirer))
   (emtp:require))

;;;_. Body
;;;_ , Structures
;;Other data may be added here, such as for variables that cases
;;carry around.
(defstruct (emtr:external-data
	      (:conc-name emtr:external-data->)
	      (:constructor emtr:make-external-data))
   "Data telling us how to run the interaction sequence"
   tq
   report-f
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

   (ignore-errors
      (cancel-timer
	 (emtr:external-data->timer (third data))))
   (emtr:external-start-next (third data)))

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
      
      ;;Otherwise report results
      (progn
	 (funcall (emtr:external-data->report-f data)
	    (emt:testral:make-suite
	       :contents
	       (emtt:testral:continued-with 
		  (emtr:external-data->testral-obj data)
		  (emtt:testral:note-list))
	       :grade (emt:testral:make-grade:test-case)))
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
	    :form 
	    (let* 
	       ((forms (cddr form)))
	       (if forms `(progn ,@forms) nil))
	    ;;$$PUNT Take a timeout nicely.
	    :timeout timeout))
      ;;It will complain about the unknown governor when it's run.
      (t
	 (emtr:make-interact-predata
	    :question ""
	    ;;$$PUNT  Show what the governor is.
	    :form     '(emt:doc "unknown governor")
	    :timeout 0.0000001))))


;;;_ , Entry point (for clause explorer)
;;;_  . emtr:external
;;;###autoload
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
			"emtr:external: no exec+args or no prompt"))))
	    ;;Do test
	    (let* 
	       ((con
		   (emtt:testral:make-continuing))
		  (proc
		     (apply 
			(if shell
			   #'start-process-shell-command
			   #'start-process)
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
			:report-f report-f
			;;Timer is not set now, it will be set when we
			;;start
			:timer nil 
			:pending pending
			:prompt prompt
			:testral-obj con)))

	       ;;Start it all.
	       (emtr:external-start-next data))))))

;;;_ , Register it
;;$$TRANSITIONAL - belongs at the top of the file
;;;###autoload (unless (fboundp 'emtt:add-runner)
;;;###autoload   (error "A certain unwritten file must be loaded"))
;;;_  . Registration itself
;;;###autoload (emtt:add-runner 'external #'emtr:external
;;;###autoload   "External runner") 
;;;_  . Provision
;;$$TRANSITIONAL
;;;###autoload (provide 'emtest/runner/runners/def-runners)


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/runners/external)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/runners/external.el ends here
