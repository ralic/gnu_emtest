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

(require 'emtest/main/find-tests)
(require 'emtest/types/run-types)
(require 'emtest/types/testral-types)

;;;_. Body
;;;_ , Support functions
;;;_  . emt:xp:library:ldhst:el->symbol

(defun emt:xp:library:ldhst:el->symbol (x)
   ""
   (if
      (symbolp x)
      x
      (if
	 (memq (car x)
	    '(autoload defun provide))
	 (cdr x))))

;;;_  . Helper emt:xp:library:sym->suites

(defun emt:xp:library:sym->suites (lib-sym)
   ""
   (emt:xp:library:suites
      (locate-library
	 (symbol-name lib-sym))))

;;;_  . emt:xp:library:suites
(defun emt:xp:library:suites (lib-path)
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
			  ((sym (emt:xp:library:ldhst:el->symbol x)))
			  (when (get sym 'emt:suite) sym)))
		  (cdr lib-data)))))
      suites))
;;;_  . emt:xp:library:path->lib-sym
(defun emt:xp:library:path->lib-sym (lib-path)
   ""

   (let*
      (
	 (lib-data (assoc lib-path load-history))
	 (provide-cell
	    (assq 'provide lib-data)))
      (cdr provide-cell)))
;;;_  . emt:xp:library:ldhst:line-has-tests
(defun emt:xp:library:ldhst:line-has-tests (lib-data)
   "Return non-nil just if LIB-DATA has any tests.

LIB-DATA must be in the format of a line from load-history."

   (some
      #'(lambda (x)
	   (get (emt:xp:library:ldhst:el->symbol x) 'emt:suite))
      (cdr lib-data)))

;;;_  . emt:xp:library:get-all-testable
(defun emt:xp:library:get-all-testable ()
   "Return a list of all testable libraries, each a list of
\(symbol absolute-filename\)."
   
   (delq nil
      (mapcar
	 #'(lambda (line)
	      (if (emt:xp:library:ldhst:line-has-tests line)
		 (let
		    ((cell (assoc 'provide line)))
		    (if cell
		       (list (cdr cell) (car line))
		       nil))
		 nil))
	  load-history)))

;;;_ , Launcher
;;;_  . emt:lib-at-point

;;Command: Run the library of symbol at point, or failing that, file
;;at point.  Give a prompt for confirmation.
;;Can use `symbol-file'

;;;_  . emt:xp:library:read-testable-library
(defun emt:xp:library:read-testable-library (prompt)
   "Interactively read the name of a library containing tests.
PROMPT is a prompt string"
   
   (completing-read 
      prompt
      load-history
      ;;Narrow to just libraries that have tests in them.
      #'emt:xp:library:ldhst:line-has-tests
      t))
;;$$ADD TESTS There are example loads in emtest/explorers/library/tests

;;;_  . emtest:library
;;;###autoload
(defun emtest:library (library &optional arg)
   "Run the test suites of LIBRARY.
LIBRARY is the absolute file name of the library"
   
   (interactive
      (list
	 (emtel:read-testable-library 
	    "Run tests of which library: ")
	 current-prefix-arg))
   
   (let
      ((lib-sym
	  (emt:xp:library:path->lib-sym library)))
      (emt:lch:run 
	 `(library:elisp-load ,library ,lib-sym)
	 (emt:lch:get-prop-list arg)
	 (list (concat "library " (symbol-name lib-sym))))))

;;;_ , Explorer
;;;_  . emt:xp:library
;;;###autoload
(defun emt:xp:library (test-id props path report-f)
   ""
   (if (cdr test-id)
      (destructuring-bind (gov lib-path lib-sym) test-id
	 (let* 
	    (  
	       (lib-sym (or lib-sym (emt:xp:library:path->lib-sym lib-path)))
	       (suite-list
		  (emt:xp:library:suites lib-path))
	       (list-to-run
		  (mapcar
		     #'(lambda (suite-sym)
			  (emt:t:make-explorable
			     :how-to-run
			     (emt:t:->how `(suite ,suite-sym))
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
	       list-to-run)))
      
      
      ;; Tell about libraries that we know about.
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
		       (emt:t:make-explorable
			  :how-to-run  
			  (emt:t:->how (list 'library:elisp-load path sym))
			  :prestn-path
			  (list 'library:elisp-load sym))))
		  (emt:xp:library:get-all-testable)))
	    :grade nil)
	 '())))

;;;_ , Insinuate
;;;###autoload (eval-after-load 'emtest/main/all-explorers
;;;###autoload  '(emt:exps:add 'library:elisp-load #'emt:xp:library
;;;###autoload  "Elisp library" t))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/explorers/library)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/library.el ends here
