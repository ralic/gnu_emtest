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

;;;_. Body
;;;_ , Types
(defstruct (emthow:library:elisp-load
	      (:copier nil)
	      (:constructor emthow:make-library:elisp-load)
	      (:conc-name emthow:library:elisp-load->)
	      (:include emthow))
   "Explorer class for exploring library"
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
;;;_  . utim:setf-new
(defmacro utim:setf-new (place value)
   ""
   `(unless 
      ,place
      (setf ,place ,value)))
;;;_  . emtt:lib-conform
(defun emtt:lib-conform (howto)
   ""
   ;;If symbol is nil, find it.
   (utim:setf-new
      (emthow:library:elisp-load->lib-sym howto)
      (emtt:lib-path->lib-sym 
	 (emthow:library:elisp-load->load-name howto)))
   

   ;;If path is nil, find it.
   (utim:setf-new
      (emthow:library:elisp-load->load-name howto)
      (locate-library
	 (symbol-name (emthow:library:elisp-load->lib-sym howto))))
   
   ;;Possibly try to load foo/tests.el  (Later, controlled by flags)
   ;;Punt.  They're probably already loaded.
   howto)

;;$$ADD TESTS for all 3 behaviors.

;;;_ , emtt:explore-library
;;;###autoload
(defun emtt:explore-library (test-id props path report-f)
   ""

   (emtt:lib-conform test-id)
   (let* 
      (  
	 (lib-path
	    (emthow:library:elisp-load->load-name test-id))
	 ;;See [[id:li6i8qd0xxe0][Refactoring dispatchers]]
	 (lib-sym
	    (emtt:lib-path->lib-sym lib-path))
	 (suite-list
	    (emtt:lib-suites lib-path))
	 (list-to-run
	    (mapcar
	       #'(lambda (suite-sym)
		    (emtt:make-explorable
		       :how-to-run
		       (emthow:make-suite
			  :suite-ID suite-sym)
		       :prestn-path 
		       (append 
			  path
			  (list (symbol-name suite-sym)))
		       ;;For now, libraries have no
		       ;;properties. 
		       :properties ()
		       :aliases ()))
	       suite-list)))

      (funcall report-f 
	 (emt:testral:make-suite
	    :contents
	    (emt:testral:make-runform-list
	       :els list-to-run)
	    :grade '() ;;Punt - only if it crapped
	    ;;out right here.
	    )
	 list-to-run)))
;;;_ , Insinuate
;;;###autoload (emtt:add-explorer #'emthow:library:elisp-load-p #'emtt:explore-library
;;;###autoload "Elisp library") 

;;;_. Footers
;;;_ , Provides

(provide 'emtest/explorers/library)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/library.el ends here
