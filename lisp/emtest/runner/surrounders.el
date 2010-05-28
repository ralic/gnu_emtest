;;;_ emtest/runner/surrounders.el --- Surrounder handling for Emtest

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
(require 'utility/misc)

;;;_. Body
;;;_ , test surrounders
;;;_  . emtts:always-surrounders

(defconst emtts:always-surrounders 
   '(
       ;;$$WRITE ME
       ;;emtt:message-trap
       (save-window-excursion)
       (with-temp-buffer)
       ;;$$WRITE ME as a function, responding to properties.
       ;;(with-timeout 1.0)
       ;;$$WRITE ME as a function, figure out whether to debug
       ;;(emtts:with-debugging)

       ;;Add other *standard* ones here.  
       )
   "Standard surrounders.

This should not be customized because it should not vary between
installations lest it affect results.

Error-trapping does not go here because that is an inherent part of
emtest tester." )
;;;_  . emtts:extra-surrounders
(defvar emtts:extra-surrounders
   '()
   "Non-standard surrounders added by helper modules as they are loaded." )

;;;_  . emtts:set-surrounder
(defun emtts:set-surrounder (surrounder &optional where)
   "Add SURROUNDER to emtest's surrounders for this session.
WHERE is a dummy argument for now, eventually it will allow them to be
placed first or last."
   
   (pushnew surrounder emtts:extra-surrounders))
;;;_  . emtts:clear-surrounder
(defun emtts:clear-surrounder (filter)
   "Remove surrounders that match FILTER.
Not implemented yet."
   
   (error "Not implemented yet"))

;;;_  . Place form within test-protectors
'  ;;OBSOLETE
(defun emtts:surround (form protectors)
   ""
   (let
      ((rv-protectors (reverse protectors)))
      
      (dolist (i rv-protectors form)
	 (setq form (list i form)))))

;;;_  . emts:add-surrounders
(defun emts:add-surrounders (form surrounders props)
   "Add SURROUNDERS around FORM.
SURROUNDERS is a list whose elements must each be either:
 * A list.
 * A function taking 1 argument (props) and returning a list

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
		   ;;`emth:trap-errors'?  And examine `emtt:*abort-p*'
		   ;;afterwards.
		   (funcall surrounder-0 props))
		(t
		   ;;Otherwise complain and stop.  For now, do it cheap.
		   (assert (utim:proper-list-p surrounder-0))))))
	 (setq form
	    (append surrounder-1 (list form))))))


;;;_   , emtts:get-surrounders
(defun emtts:get-surrounders (props)
   "Return a list of the appropriate surrounders for a form.
PROPS is the property list of the form."
   (append
      emtts:always-surrounders
      emtts:extra-surrounders
      (utim:get-properties :surrounders props)))


;;;_   , Some surrounders

;;;_    . emtts:with-debugging
;;Exists just to make `emtts:get-surrounders' neater.
(defmacro emtts:with-debugging (&rest form)
   ""
   
   `(progn
       (debug) 
       ,@form))

;;;_     , Tests

;;Can't easily automatically test that it in fact debugs.

;;;_    . emth:trap-errors
;;$$USE ME
;;$$MOVE ME To standard testhelp
(defmacro emth:trap-errors (&rest body)
   ""
   `(progn
       (declare (special emt:testral:*events-seen* emtt:*abort-p*))
       (condition-case err
	  (progn ,@body)
	  ('emt:already-handled
	     (setq emtt:*abort-p* t))
	  ;;$$ADD ME an error case for dormancy pseudo-errors.  It
	  ;;should push a dormancy note (here, not lower down, which
	  ;;may be somehow wrong?)
	  (error
	     (push
		(emt:testral:make-error-raised
		   :err err
		   :badnesses '(ungraded))
		emt:testral:*events-seen*)
	     (setq emtt:*abort-p* t)))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/surrounders)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/surrounders.el ends here
