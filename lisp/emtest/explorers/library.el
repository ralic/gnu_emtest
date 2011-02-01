;;;_ emtest/explorers/library.el --- Library explorer for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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

(require 'emtest/types/testral-types)
(require 'emtest/types/run-types)
(require 'emtest/explorers/suite)
(require 'emtest/launch/all)

;;;_. Body
;;;_ , Types
'
(defstruct (emthow:library:elisp-load
	      (:type list)
	      (:copier nil)
	      (:constructor emthow:make-library:elisp-load)
	      (:conc-name emthow:library:elisp-load->))
   "Arglist for exploring library"
   load-name
   lib-sym)
;;;_ , Support functions
;;;_  . emtl:ldhst-el->symbol

(defun emtl:ldhst-el->symbol (x)
   ""
   (if
      (symbolp x)
      x
      (if
	 (memq (car x)
	    '(autoload defun provide))
	 (cdr x))))

;;;_  . Helper emtt:lib-sym->suites

(defun emtt:lib-sym->suites (lib-sym)
   ""
   (emtt:lib-suites
      (locate-library
	 (symbol-name lib-sym))))

;;;_  . emtt:lib-suites
(defun emtt:lib-suites (lib-path)
   "Return a list of test suites for LIB-PATH.

Specifically, symbols defined in the library at LIB-PATH that
have associated test suites.
LIB-PATH must be a path to a library that is already loaded."
   (let*
      (
	 (lib-data (assoc lib-path load-history))
	 ;;List of symbols.
	 (suites
	    (delq nil
	       (mapcar
		  #'(lambda (x)
		       (let
			  ((sym (emtl:ldhst-el->symbol x)))
			  (when (get sym 'emt:suite) sym)))
		  (cdr lib-data)))))
      suites))
;;;_  . emtt:lib-path->lib-sym
(defun emtt:lib-path->lib-sym (lib-path)
   ""

   (let*
      (
	 (lib-data (assoc lib-path load-history))
	 (provide-cell
	    (assq 'provide lib-data)))
      (cdr provide-cell)))
;;;_ , Launcher
;;;_  . emt:lib-at-point

;;Command: Run the library of symbol at point, or failing that, file
;;at point.  Give a prompt for confirmation.
;;Can use `symbol-file'

;;;_  . emtel:read-testable-library
(defun emtel:read-testable-library (prompt)
   "Interactively read the name of a library containing tests.
PROMPT is a prompt string"
   
   (completing-read 
      prompt
      load-history
      ;;Narrow to just libraries that have tests in them.
      #'(lambda (lib-data)
	   (some
	      #'(lambda (x)
		   (get (emtl:ldhst-el->symbol x) 'emt:suite))
	      (cdr lib-data)))
      t))
;;$$ADD TESTS There are example loads in emtest/explorers/library/tests

;;;_  . emt:library
;;;###autoload
(defun emt:library (library &optional receiver)
   "Run the test suites of LIBRARY.
LIBRARY is the absolute file name of the library"
   
   (interactive
      (list
	 (emtel:read-testable-library 
	    "Run tests of which library: ")))
   
   (let
      ((lib-sym
	  (emtt:lib-path->lib-sym library)))
      (emtl:dispatch-normal 
	 `(library:elisp-load ,library ,lib-sym) 
	 (list (concat "library " (symbol-name lib-sym))) 
	 receiver)))

;;;_ , Explorer
;;;_  . emtt:explore-library
;;;###autoload
(defun emtt:explore-library (test-id props path report-f)
   ""

   (destructuring-bind (gov lib-path lib-sym) test-id
   (let* 
      (  
	 (lib-sym (or lib-sym (emtt:lib-path->lib-sym lib-path)))
	 (suite-list
	    (emtt:lib-suites lib-path))
	 (list-to-run
	    (mapcar
	       #'(lambda (suite-sym)
		    (emtt:make-explorable
		       :how-to-run
		       `(suite ,suite-sym)
		       :prestn-path 
		       (append 
			  path
			  (list (symbol-name suite-sym)))
		       ;;For now, libraries have no properties.
		       :properties ()
		       :aliases ()))
	       suite-list)))

      (funcall report-f 
	 (emt:testral:make-suite
	    :contents
	    (emt:testral:make-runform-list
	       :els list-to-run)
	    ;;$$IMPROVE ME Set this if it crapped out right here.
	    :grade '())
	 list-to-run))))
;;;_ , Insinuate
;;;###autoload (eval-after-load 'emtest/explorers/all
;;;###autoload  '(emtt:add-explorer 'library:elisp-load #'emtt:explore-library
;;;###autoload  "Elisp library"))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/explorers/library)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/library.el ends here
