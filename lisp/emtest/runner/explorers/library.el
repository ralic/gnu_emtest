;;;_ emtest/runner/explorers/library.el --- Library explorer for Emtest

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

(require 'emtest/common/testral-types)
(require 'emtest/common/result-types)

;;;_. Body

;;;_ , emtl:ldhst-el->symbol

(defun emtl:ldhst-el->symbol (x)
   ""
   (if
      (symbolp x)
      x
      (if
	 (memq (car x)
	    '(autoload defun))
	 (cdr x))))

;;;_ , Helper emtt:lib-sym->suites

;;$$RETHINK ME Should this take symbol or string?  We seemed to
;;convert it and then convert it back.

;;$$CHANGE ME Also offer everything in the respective /tests library,
;;and try to load it.  Add tests for this behavior.

;;$$CHANGE ME Also allow the lib's symbol as a test-suite symbol.

;;$$CHANGE ME Could also offer tests on every library that this
;;library requires.  Gotta control execution, though.  Don't want to
;;run them too eagerly.  So this would have to return more than just a
;;symbol for each.  In fact, it could become the workhorse for the
;;`emt:test-ID:e-n:library:elisp-load' case
(defun emtt:lib-sym->suites (lib-sym)
   ""
   (let*
      (
	 (lib-path
	    (locate-library
	       (symbol-name lib-sym)))
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


;;;_ , emtt:explore-library
(defun emtt:explore-library (test-id props)
   ""
   
   (let* 
      (  
	 (lib-sym
	    (emt:test-ID:e-n:library:elisp-load-load-name test-id))
	 ;;See [[id:li6i8qd0xxe0][Refactoring dispatchers]]
	 (suite-list
	    (emtt:lib-sym->suites lib-sym))
	 (path
	    (list "library" (symbol-name lib-sym)))
	 (list-to-run
	    (mapcar
	       #'(lambda (suite-sym)
		    (emtt:make-explorable
		       :how-to-run
		       (make-emt:test-ID:e-n:suite
			  :suite-ID suite-sym)
		       ;;CHANGED to append lib name.
		       :prestn-path 
		       (append 
			  path
			  (list (symbol-name suite-sym)))
		       ;;For now, libraries have no
		       ;;properties. 
		       :properties ()))
	       suite-list)))
      (list
	 nil
	 (make-emt:testral:suite
	    :contents
	    (emt:testral:make-runform-list
	       :els list-to-run)
	    :badnesses '() ;;Punt - only if it crapped
	    ;;out right here.
	    :info '() ;;Punt info for now.
	    ))))
;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/explorers/library)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/explorers/library.el ends here
