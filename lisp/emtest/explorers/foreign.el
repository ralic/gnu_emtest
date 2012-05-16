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
	  (integer
	     :value 120
	     :tag "Timeout"
	     :help-echo "How many seconds the last test to shut down the executable")))
   
   :group 'emtest)


;;;_ , Alist of foreign testers currently running
(defvar emt:foreign:current-testers
   '()
   ;; For now, timer is not used.
   "Alist of current foreign processes.  

Each element is of the form \(name tq timer launchable\)" )

;;;_ , emt:foreign:make-tq
;; Could share this with expect.el
(defun emt:foreign:make-tq (exec+args shell proc-base-name)
   ""
   (let* 
      ((proc 
	  (apply 
	     (if shell
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
   
   (emt:foreign:make-tq (second launchable) (fourth launchable) "foreign"))

;;;_ , emt:foreign:revive-tester
(defun emt:foreign:revive-tester (tester)
   "Make sure the tq of TESTER is alive.

TESTER should be an element of emt:foreign:current-testers."
   
   (destructuring-bind (name tq timer launchable) tester
      (unless
	 (buffer-live-p (tq-buffer tq))
	 ;; Cancel any timer that was running wrt it.  (Even though we
	 ;; don't make timers yet).
	 (when (timerp timer) (cancel-timer timer))
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

;; time = number of seconds, just like repeat.  

;;(run-at-time time repeat function &rest args)
;;(cancel-timer timer)
;;(tq-close queue)

;; Timer should just check tq every so often, and check that it got
;; some answer in the meantime.
;;;_ , Utility

;;;_ , Text to Csexp
;;;_  . emt:foreign-read-buffer-csexp-loop
(defun emt:foreign-read-buffer-csexp-loop ()
   ""
   
   (let
      ((rv-csexp (list)))
      ;; We come directly here when we see the end of a list.
      (catch 'emt:foreign-csexp-EOL
	 (while (not (eobp))
	    ;; We come directly here, skipping push, when we read a
	    ;; non-object.
	    (catch 'emt:foreign-csexp-NOTHING
	       (push (emt:foreign-read-buffer-csexp-single) rv-csexp))))
      (nreverse rv-csexp)))

;;;_  . emt:foreign-read-buffer-csexp-single
(defun emt:foreign-read-buffer-csexp-single ()
   ""
   
   (if
      (looking-at "[0-9]*:")
      (let*
	 (  (start (match-beginning 0))
	    (end (match-end 0))
	    ;; First we read how many chars to read - careful not
	    ;; to include ":"
	    (digit-str (buffer-substring start (1- end)))
	    (num-chars (read digit-str))
	    (end-of-read  (+ end num-chars))
	    ;; Then we read the string itself
	    (str (buffer-substring end end-of-read)))
	 (goto-char end-of-read)
	 str)
	 
      (case (prog1
	       (char-after)
	       ;; Don't move past end of buffer, in case we're reading
	       ;; the last char in the buffer.
	       (unless (eobp) (forward-char)))
	 
	 (?\( 
	    (emt:foreign-read-buffer-csexp-loop))
	 (?\)
	    (throw 'emt:foreign-csexp-EOL nil))
	 ;; Read nothing for whitespace etc.
	 (t (throw 'emt:foreign-csexp-NOTHING nil)))))
;;;_  . emt:foreign-read-buffer-csexp
(defun emt:foreign-read-buffer-csexp (text)
   ""

   (with-temp-buffer
      (insert text)
      (goto-char 1)
      (catch 'emt:foreign-csexp-EOL
	 (catch 'emt:foreign-csexp-NOTHING
	    (emt:foreign-read-buffer-csexp-single)))))
;;;_ , Csexp to text
;;;_ , Stringtree to obj
;;;_  . emt:foreign-ctor-alist

(defvar emt:foreign-ctor-alist 
   '(
       (list    list t)
       (integer read t)
       (grade   intern)
       (symbol  intern)
       ;; (prestn-path)

       (suite    emt:testral:make-suite   nil)
       (note     emt:testral:note         nil)
       (runforms emt:testral:runform-list nil)
       (notes    emt:testral:note-list    nil)
       (report   emt:testral:report       nil))
   
   "Alist of ctors of foreign-able types from stringtrees, for incoming objects.

Each entry is of the form \(SYMBOL CTOR POSITIONAL?\).

If POSITIONAL? is nil, each argument is expected to be of the form \(KEY
VALUE\).  Before CTOR is called, VALUE is recursively parsed and key
is interned, prepended with \":\".

If POSITIONAL? is t, each argument is simply recursively parsed first." )

;;;_  . emt:foreign-stringtree->object

(defun emt:foreign-stringtree->object (stringtree)
   "Given a stringtree, construct a corresponding object"

   (typecase stringtree
      (string stringtree)
      (cons
	 (when (car stringtree)
	    (let*
	       (
		  (sym (intern (car stringtree)))
		  (cell (assq sym emt:foreign-ctor-alist)))
	       (when cell
		  (destructuring-bind
		     (functor func positionalp) cell
		     (if positionalp
			;; Positionally
			(apply func
			   (mapcar 
			      #'emt:foreign-stringtree->object
			      (cdr stringtree)))
			;;By key - basically imitates structure
			;;construction.
			(apply func
			   (apply #'append
			      (mapcar 
				 #'(lambda (x)
				      (destructuring-bind
					 (key value) x
					 (list 
					    ;; Key had better be a string
					    (intern (concat ":" key)) 
					    (emt:foreign-stringtree->object
					       value))))
				 (cdr stringtree))))))))))))

;;;_ , Obj to stringtree
;;;_  . emt:foreign-stringtreer-alist 
(defvar emt:foreign-stringtreer-alist 
   '((listp "list" t identity)
       (emt:testral:suite-p "suite" struct emt:testral:suite)
       
       )
   "Alist of the stringtree-ers of foreign-able types, for outgoing objects.

Each entry is of one of the forms
 * \(PREDICATE NAME t POSITIONAL-STRINGTREER\).
 * \(PREDICATE NAME nil KEYWISE-STRINGTREER\).
 * \(PREDICATE NAME struct STRUCT-SYMBOL\).

Positional-Stringtreer is expected to return a list of positional
arguments.  

Keywise-Stringtreer should return a list where each element of
the list is in the form \(KEY VALUE\), and KEY is the symbol of a
slot (without ':', which will be added in reading)."
   )

;;;_  . emt:foreign:struct-stringtreer
(defun emt:foreign:struct-stringtreer (struct-sym x)
   ""
   (let
      ((slots 
	  (get struct-sym 'cl-struct-slots))
	 (i 0))

      (delq nil
	 (mapcar
	    #'(lambda (slot-form)
		 (let
		    ((key (car slot-form))
		       (value (aref x i)))
		    (incf i)
		    (unless
		       (eq key 'cl-tag-slot)
		       (list 
			  (symbol-name key)
			  (emt:foreign:object->stringtree value)))))
	    slots))))
;;;_  . emt:foreign:keyvalue-stringtreer
(defun emt:foreign:keyvalue-stringtreer (x)
   ""
   
   (destructuring-bind
      (sym value) x
      (list 
	 ;; Sym had better be a symbol
	 (symbol-name sym)
	 (emt:foreign:object->stringtree
	    value))))
;;;_  . emt:foreign:object->stringtree

(defun emt:foreign:object->stringtree (object)
   "Return a stringtree corresponding to OBJECT"

   (if
      (stringp object)
      object
      (let* 
	 ((occurence
	     (find object emt:foreign-stringtreer-alist
		:test 
		#'(lambda (ob form)
		     (funcall (car form) ob)))))
	 (when occurence
	    (destructuring-bind
	       (predicate name positional stringtreer) occurence
	       (cons
		  name
		  (case positional
		     ((t) 
			(mapcar 
			   #'emt:foreign:object->stringtree
			   (funcall stringtreer object)))
		     ((nil) 
			(mapcar 
			   #'emt:foreign:keyvalue-stringtreer
			   (funcall stringtreer object)))
		     (struct
			(emt:foreign:struct-stringtreer
			   stringtreer object))
		     (t '()))))))))

;;;_ , Text to/from TESTRAL
;;;_ , emt:foreign-encode-TESTRAL
(defun emt:foreign-encode-TESTRAL (raw-question)
   ""
   ;; Punt for now
   "()")
;;;_ , emt:foreign-decode-to-TESTRAL
(defun emt:foreign-decode-to-TESTRAL (text)
   "Convert answer to csexp and thence to object."

   (let*
      ((stringtree (emt:foreign-read-buffer-csexp text))
	 (object
	    (emt:foreign-stringtree->object stringtree)))))

;;;_ , The explorer proper
;;;_  . emt:foreign-report-results
(defun emt:foreign-report-results (passed-object answer)
   "Report the results when we get an answer"
   
   (destructuring-bind
      (report-f tester) passed-object
      (funcall report-f
	 (let
	    ((object (emt:foreign-decode-to-TESTRAL answer)))
	    ;; $$RETHINK This seems to be the expected return, but
	    ;; maybe it should be emt:testral:report?  But report-f
	    ;; expects a suite.
	    (if
	       (emt:testral:suite-p object)
	       object
	       (emt:testral:make-suite
		  ;; Add a note about getting the wrong object.
		  :contents '()
		  :grade 'blowout)))
	 ;; Could schedule any tests a suite returns.
	 '())))

;;;_  . emtt:explore-foreign
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
