;;;_ tester/launch.el --- Launchers for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(require 'tester/tester)
(require 'viewers/emviewer)

;;;_. Body

;;;_ , Borrowed variables
;;;_  . Counter

;;Should probably live elsewhere.  Possibly in viewers/receive
(defvar emtt:testrun-counter 0 
   "A counter used to make testrun-id.
With `cl' loaded, use it as (incf emtt:testrun-counter)." )
;;;_  . emtt:receiver-f
;;$$MOVE ME belongs in a config library
;;Cheat for now: Always know to use emviewer.  Later use a
;;customizable variable.  Default to using `viewers/emviewer'
(defconst emtt:receiver-f
   #'emtest:viewer:receive
   "" )

;;;_ , Helper emtt:form->test-id
;;Obsolete
'
(defun emtt:form->test-id (form)
   ""
   (make-emt:test-ID
      :context ()
      :explore-next
      (make-emt:test-ID:e-n:form
	 :test-form form)))

;;;_ , emtt:sexp-at-point->result
;;A test helper.  Obsolete.  No longer makes sense, because
;;the output is what is tested.
'
(defun emtt:sexp-at-point->result (form)
   ""
   (emt:test-finder:top 
      (make-emt:test-ID:e-n:form
	 :test-form form)
      (list "form")
      (prin1-to-string (incf emtt:testrun-counter))
      ;;$$FIXME  This no longer makes sense.  Now we should test thru
      ;;receive, which puts results into a more orderly form.
      #'identity))

;;;_ , emtt:ts:run-test
;;Meant to support view-tests
(defun emtt:ts:run-test (test-form callback &optional prefix testrun-id)
   "
NB, TEST-FORM is a *test-form*, which should begin with a docstring."
   (emt:test-finder:top 
      (make-emt:test-ID:e-n:form
	 :test-form test-form)
      (or prefix (list "test-form"))
      (or testrun-id "0")
      callback))

;;;_ , emtt:dispatch-normal
(defun emtt:dispatch-normal (what-to-run &optional prefix)
   ""
   (emt:test-finder:top 
      what-to-run 
      prefix  ;;Default is the empty list.
      (prin1-to-string (incf emtt:testrun-counter))
      emtt:receiver-f))

;;;_ , emtt:sexp-at-point

(defun emtt:sexp-at-point (form)
   ""
   (interactive 
      (list 
	 (save-excursion (read (current-buffer)))))
   
   (emtt:dispatch-normal
      (make-emt:test-ID:e-n:form
	 :test-form form)
      (list "form")))
 

;;;_ , emtt:suite
;;Obsolete.  Use `emtt:defun-at-point'


;;;_ , emtt:defun-at-point
;;;###autoload
(defun emtt:defun-at-point (arg)
  "Run tests on the function or suite under point.

If prefix ARG is non-nil, eval it first.

Does nothing if the buffer is not in a known lisp mode."

   (interactive "P")
   ;;Only proceed if we know how to run tests
   (when (eq major-mode 'emacs-lisp-mode)

      ;;If `arg', eval that definition first.
      (when arg (eval-defun nil))
      (let
	 ((suite-name
	     (emt:suite-sym-at-point)))
	 (check-type suite-name symbol)
	 ;;Why was this circumlocution neccessary before?
	 '(funcall #'emtt:suite suite-name)
	 (emtt:dispatch-normal 
	    (make-emt:test-ID:e-n:suite
	       :suite-ID suite-name)))))


;;;_  . Helpers (Lisp-syntax-reading stuff)

(defconst emt:defun-types 
  '(defun defun* defsubst defsubst* defmacro defmacro* defmethod
      deftype defadvice
      emt:deftest-2 emt:deftest-3)
   
  "List of defun-variant symbols we might see" )

(defun emt:suite-sym-at-point-x (arg)
   "Return the symbol that names the definition at point.
With `nil' ARG, look backwards for it.
With non-nil ARG, look forwards for it."
   (condition-case err
      (save-excursion
	 (beginning-of-defun (if arg -1 nil))
	 (down-list 1)
	 (let*
	    (  (type
		  (read
		     (current-buffer)))
	       (symbol
		  (if
		     (memq type emt:defun-types)
		     (read
			(current-buffer))))
	       ;;This transformation belongs in an explore-method instead
	       ;;of here.
	       (proxy
		  (get symbol 'rtest:test-thru)))
	    (or proxy symbol)))
      (scan-error nil)
      ))

(defun emt:suite-sym-at-point () 
   "Return the symbol of the test suite relevant to the definition at point"
   
   (or
      ;;First try to find it backwards
      (emt:suite-sym-at-point-x nil)
      ;;If that fails, try to find it forwards
      (emt:suite-sym-at-point-x -1)))



;;;_ , emtt:library

;;;###autoload
(defun emtt:library (library)
   "Run the test suites of LIBRARY"
   
   (interactive
      (list
	 (completing-read 
	    "Run test suites of which library: "
	    load-history
	    nil	;;No narrowing provided yet.
	    t)))

   (let*
      (
	 (test-id
	    (make-emt:test-ID:e-n:library:elisp-load
	       ;;$$INSPECTME Should this by symbol or string?  Or
	       ;;allow both?
	       :load-name (intern-soft library))))
      (emtt:dispatch-normal test-id)))
;;;_  . Helper emtt:lib-sym->suites

(defun emtt:lib-sym->suites (lib-sym)
   ""
   (let*
      (
	 (lib-data (assoc lib-sym load-history))
	 ;;Or could (remove* :test-not)
	 ;;List of symbols.
	 (suites
	    (loop
	       for x in (cdr lib-data)
	       if 
	       (and
		  (symbolp x)
		  (get x 'emt:suite))
	       collect x)))
      suites))



;;;_. Footers
;;;_ , Provides

(provide 'tester/launch)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/launch.el ends here
