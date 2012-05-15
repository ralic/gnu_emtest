;;;_ emtest/explorers/foreign.el --- Explorer that serves test-frameworks on other programs

;;;_. Headers
;;;_ , License
;; Copyright (C) 2012  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: tools

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


;;;_. Body
;;;_ , Customizations

(defcustom emt:foreign:launchables
   '()
   
   "List of executables suitable for this and config data for them."
   :type 
   '(repeat
       (list
	  (string
	     :value "Unnamed"
	     :tag "name"
	     :help-echo "Your nickname for this entry")
	  ;; And its arguments.
	  (cons
	     (file
		:value ""
		:tag "executable"
		:help-echo "The executable's filename")
	     (repeat
		(string  
		   :value ""
		   :tag "arg"
		   :help-echo "An argument when invoking the executable")))
	  (string
	     :value "EndOfAnswer"
	     :tag "terminating regex"
	     :help-echo "Regular expression indicating the end of an answer string")
	  (boolean
	     :value t
	     :tag "Use shell"
	     :help-echo "Whether to go thru shell when launching the executable")
	  (file    
	     :value ""
	     :tag "database-file"
	     :help-echo "Optional: database file for persistent test data.  Need not already exist.")
	  ;; Should be properties for it.
	  (boolean 
	     :value nil
	     :tag "Run all immediately"
	     :help-echo "Whether to immediately run all tests")
	  (string  
	     :value "2 min"
	     :tag "Timeout"
	     :help-echo "How soon after the last test to shut down the executable, like \"2 min\"")))
   
   :group 'emtest)


;;;_ , Alist of foreign testers currently running
(defvar emt:foreign:current-testers
   '()
   ;; For now, timer is not used.
   "Alist of current foreign processes.  

Each element is of the form \(name tq timer launchable\)" )

;;;_ , emt:foreign:make-tq
;; Could share this with expect.el
(defun emt:foreign:make-tq (exec+args proc-base-name)
   ""
   (let* 
      ((proc 
	  (apply 
	     ;; Take a shell argument
	     (if t;;shell
		#'start-process-shell-command
		#'start-process)
	     proc-base-name nil exec+args)))
      (unless
	 ;;0 status means a live process
	 (equal
	    (process-exit-status proc)
	    0)
	 (error
	    "No live process"))
      (tq-create proc)))
;;;_ , emt:foreign:launchable->tq
(defun emt:foreign:launchable->tq (launchable)
   "Make a transaction queue appropriate to LAUNCHABLE"
   
   (emt:foreign:make-tq (second launchable) "foreign"))

;;;_ , emt:foreign:revive-tester
(defun emt:foreign:revive-tester (tester)
   "Make sure the tq of TESTER is alive.

TESTER should be an element of emt:foreign:current-testers."
   
   (destructuring-bind (name tq timer launchable)
      tester
      ;; Cancel any timer that was running wrt it.  (Even though we
      ;; don't make timers yet)
      (when (timerp timer) (cancel-timer timer))

      (unless
	 (buffer-live-p (tq-buffer tq))
	 (setf (second tester) 
	    (emt:foreign:launchable->tq launchable)))))

;;;_ , emt:foreign:get-tester

(defun emt:foreign:get-tester (name)
   "Get the tester for name.

NAME should be the nickname of some launchable"
   
   (let*
      (
	 (launchable
	    (assoc name emt:foreign:launchables))
	 (tester (assoc name emt:foreign:current-testers)))
      (if tester
	 (emt:foreign:revive-tester tester)
	 ;; If it doesn't exist, make it.
	 (progn
	    (setq tester 
	       (list 
		  name
		  (emt:foreign:launchable->tq launchable)
		  ;; No timer yet
		  nil
		  launchable))
	    (push tester emt:foreign:current-testers)))
      tester))

;;;_ , Related to stopping it a certain time after last Q.

;; time = "2 min"
;;(run-at-time time repeat function &rest args)
;;(cancel-timer timer)
;;(tq-close queue)

;;;_ , emt:foreign-encode-TESTRAL
(defun emt:foreign-encode-TESTRAL (raw-question)
   ""
   ;; Punt for now
   "()")
;;;_ , emt:foreign-decode-to-TESTRAL
(defun emt:foreign-decode-to-TESTRAL (text)
   "Convert answer to csexp and thence to object."
   ;; Transitional
   (assert (string= text "()"))
   (let*
      ()

      (emt:testral:make-suite
	 :contents '()
	 :grade 'ok)
      ))

;;;_ , emt:foreign-report-results
;; This will get a bundled object
(defun emt:foreign-report-results (passed-object answer)
   "Report the results when we get an answer"
   
   (destructuring-bind
      (report-f tester) passed-object
      ;; Stop the timer?  Just if tq is not empty?  Otherwise restart
      ;; it?  Maybe timer should just check tq every so often, and
      ;; check that it got some answer in the meantime.

      (funcall report-f
	 (emt:foreign-decode-to-TESTRAL answer)

	 ;; Could schedule any tests a suite returns.
	 '())))

;;;_ , emtt:explore-foreign
;;;###autoload
(defun emtt:explore-foreign (test-id props-unused path report-f)
   ""
   (if (cdr test-id)
      (let* 
	 (
	    (launchable-name (second test-id))
	    (raw-question (third test-id))
	    (tester (emt:foreign:get-tester launchable-name))
	    (tq (second tester))
	    (terminating-regex (third tester)))
	 
	 (tq-enqueue tq 
	    (emt:foreign-encode-TESTRAL raw-question)
	    terminating-regex
	    (list report-f tester)
	    #'emt:foreign-report-results t))
      

      ;; List foreigns that we could run, from customization list.
      (funcall report-f
	 (emt:testral:make-suite
	    :contents 
	    (emt:testral:make-runform-list
	       :els
	       (mapcar 
		  #'(lambda (x)
		       (let
			  ((name (first x)))
			  (emtt:make-explorable
			     :how-to-run  
			     (list 'foreign name)
			     :prestn-path
			     (list 'foreign name))))
		  emt:foreign:launchables))
	    :grade nil)
	 '())))

;;;_ , Register it

;;;_ , Insinuate
;;;###autoload (eval-after-load 'emtest/main/all-explorers
;;;###autoload  '(emt:exps:add 'foreign #'emtt:explore-foreign
;;;###autoload  "Testers in other executables" t))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/explorers/foreign)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/foreign.el ends here
