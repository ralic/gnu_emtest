;;;_ emtest/main/surrounders.el --- Surrounder handling for Emtest

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
(require 'utility/misc)
(require 'emtest/support/individual)

;;;_. Body
;;;_ , test surrounders
;;;_  . emt:sur:always-surrounders

(defconst emt:sur:always-surrounders 
   '(
       ;;$$WRITE ME
       ;;emtt:message-trap
       (save-window-excursion)
       (with-temp-buffer)
       ;;$$WRITE ME as a function, responding to properties.
       ;;(with-timeout 1.0)
       emt:sur:with-debugging

       ;;Add other *standard* ones here.  
       )
   "Standard surrounders.

This should not be customized because it should not vary between
installations lest it affect results.

Error-trapping does not go here because that is an inherent part of
emtest tester." )
;;;_  . emt:sur:extra-surrounders
(defvar emt:sur:extra-surrounders
   '()
   "Non-standard surrounders added by helper modules as they are loaded." )

;;;_  . emt:sur:set-surrounder
(defun emt:sur:set-surrounder (surrounder &optional where)
   "Add SURROUNDER to emtest's surrounders for this session.
WHERE is a dummy argument for now, eventually it will allow them to be
placed first or last."
   
   (pushnew surrounder emt:sur:extra-surrounders))
;;;_  . emt:sur:clear-surrounder
(defun emt:sur:clear-surrounder (filter)
   "Remove surrounders that match FILTER.
Not implemented yet."
   
   (error "Not implemented yet"))

;;;_  . emt:sur:add-surrounders
(defun emt:sur:add-surrounders (form surrounders props)
   "Add SURROUNDERS around FORM.
SURROUNDERS is a list whose elements must each be either:
 * A list.
 * A function taking 1 argument (props) and returning a list or `nil'.

In either case, FORM is added as the last element of the list.

PROPS is a property list."
   
   (dolist (surrounder-0 (reverse surrounders) form)
      (let
	 ((surrounder-1
	     (cond
		((utim:proper-list-p surrounder-0) 
		   surrounder-0)
		((functionp surrounder-0)
		   ;;$$MAKE ME SAFE Protect this call.  Use
		   ;;`emth:trap-errors'?
		   (funcall surrounder-0 props))
		(t
		   ;;Otherwise complain and stop.  For now, do it cheap.
		   (assert (utim:proper-list-p surrounder-0))))))
	 (when surrounder-1
	    (setq form
	       (append surrounder-1 (list form)))))))


;;;_   , emt:sur:get-surrounders
(defun emt:sur:get-surrounders (props)
   "Return a list of the appropriate surrounders for a form.
PROPS is the property list of the form."
   (append
      emt:sur:always-surrounders
      emt:sur:extra-surrounders
      (utim:get-properties :surrounders props)))


;;;_   , Some surrounders and surrounder-makers
;;;_    . Allow debugging
;;;_     , emt:sur:debug-p
(defvar emt:sur:debug-p nil 
   "Debug all tests on entry" )

;;;_     , emt:sur:with-debugging
(defun emt:sur:with-debugging (props)
   "Surrounder-maker, enable debug in the form if appropriate."
   ;;Will probably use (utim:get-properties :debug props) instead
   ;;eventually. 
   (let
      ((debug-p emt:sur:debug-p))
      (if debug-p
	 '(let
	     (  (debug-on-signal t)
		(debug-on-error  t)))
	 ;;Otherwise, empty list.
	 '())))

;;;_     , emtest:debug-on-entry
;;$$IMPROVE ME  Make the interface much more specific.
;;;###autoload
(defun emtest:debug-on-entry (&rest r)
   "Debug all test clauses on entry."
   
   (interactive)
   (setq emt:sur:debug-p t))

;;;_     , emtest:cancel-debug-on-entry
;;$$IMPROVE ME  Make the interface volunteer only currently debugged
;;runnables.
;;;###autoload
(defun emtest:cancel-debug-on-entry (&rest r)
   ""
   
   (interactive)
   (setq emt:sur:debug-p nil))


;;;_       , Tests

;;Can't easily automatically test that it in fact debugs.


;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/surrounders)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/main/surrounders.el ends here
