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
	  (string  :value "Unnamed" 
	     :tag "name"
	     :help-echo "Your nickname for this entry")
	  (file    :value ""        
	     :tag "executable"
	     :help-echo "The executable's filename")
	  (file    :value ""        
	     :tag "database-file"
	     :help-echo "Optional: database file for persistent test data.  Need not already exist.")
	  (boolean :value nil       
	     :tag "Run all immediately"
	     :help-echo "Whether to immediately run all tests")
	  (string  :value "2 min"   
	     :tag "Timeout"
	     :help-echo "How soon after the last test to shut down the executable, like \"2 min\"")))
   
   :group 'emtest)


;;;_ , Alist of current ones
(defvar emt:foreign:current-tqs
   '()
   ;; For now, timer is not used.
   "Alist of current foreign processes.  

Each element is of the form \(name tq timer\)" )

;;;_ , emt:foreign:revive-tq
(defun emt:foreign:revive-tq (element)
   "Make sure the tq of ELEMENT is alive.

ELEMENT should be an element of emt:foreign:current-tqs."
   ;; (cancel-timer timer)
   (let*
      ()
      
      ))
;;;_ , emt:foreign:get-tq

(defun emt:foreign:get-tq (name)
   "Get the process for name.

NAME should be the nickname of some launchable"
   
   (let*
      ((cell (assoc name emt:foreign:current-tqs)))
      (if cell
	 (emt:foreign:revive-tq cell)
	 
	 
	 )
      
      ))
;; (tq-create proc)

;; time = "2 min"
;;(run-at-time time repeat function &rest args)
;;(cancel-timer timer)
;;;_ , emt:foreign-report-results
;; This will get a bundled object

;;;_ , emtt:explore-foreign
;;;###autoload
(defun emtt:explore-foreign (test-id props-unused path report-f)
   ""
   (if (cdr test-id)
      (let* 
	 (
	    (launchable-name (second test-id))
	    (tq (emt:foreign:get-tq launchable-name)))
	 (let
	    (  
	       )
	       
	    #'(lambda (report-f)
		 (emt:foreign-report-results report-f)
		 )
	    (funcall report-f
	       ;; Convert
	       (emt:testral:make-suite
		  :contents 
		  (emt:testral:make-runform-list
		     :els (reverse rv-list-to-run))
		  :grade '())
	       ;; Maybe pluck this from results, maybe not.
	       (reverse rv-list-to-run))))

      ;; List foreigns that we could run, from customization list.
      (funcall report-f
	 (emt:testral:make-suite
	    :contents 
	    (emt:testral:make-runform-list
	       :els
	       (mapcar 
		  #'(lambda (x)
		       (let
			  ((sym (first x))
			     (path (second x)))
			  (emtt:make-explorable
			     :how-to-run  
			     (list 'library:elisp-load path sym)
			     :prestn-path
			     (list 'library:elisp-load sym))))
		  (emtt:all-testing-libs)))
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
