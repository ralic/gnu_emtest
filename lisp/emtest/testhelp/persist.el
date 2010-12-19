;;;_ emtest/testhelp/persist.el --- The testhelp portion of Emtest persist functionality

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
;;This is testhelp, distinct from emtest/common/persist which is
;;implementation.

;;;_ , Requires

(require 'emtest/common/result-types)
(require 'emtest/common/persist-2)


;;;_. Body

;;;_ , Persist functions 

;;;_  . emt:eq-persist-p
;;$$NORMALIZE ME  emt:trace:properties should be renamed and set by
;;runner functionality.
;;;###autoload
(defun emt:eq-persist-p (compare-f value id &optional backend)
   "Compare VALUE to the value of ID in a database, using COMPARE-F.
If ID does not exist in the database, make a TESTRAL note which
includes the value of VALUE.

BACKEND, if given, describes the database backend."

   ;;$$IMPROVE ME This implies it is called in an clause context and
   ;;an abort-controlled context.  We should check those and act OK
   ;;even when they're false.
   (declare (special emt:trace:properties emtt:*abort-p*))
   (let*
      ((backend
	  (or
	     backend
	     ;;$$ENCAP ME - this should be a standard property-getter,
	     ;;and not expose `emt:trace:properties'.
	     ;;Use utim:get-properties.  But first remove that from
	     ;;tester.el and give it the right default.
	     ;;(utim:get-properties 'db-id emt:trace:properties)
	     (let
		((cell (assoc 'db-id emt:trace:properties)))
		(when cell
		   (second cell)))
	     ;;Here add any other ways of learning the backend
	     (error "No backend was provided"))))

      (condition-case err
	 (let*
	    ((stored-value
		(emdb:get-value backend id 'correct-answer))
	       (result
		  (funcall compare-f value stored-value)))

	    ;;Note the result.
	    (emtt:testral:add-note
	       (emt:testral:make-newstyle
		  ;;$$TRANSITIONAL Later we'll encap this in testral.
		  ;;$$TRANSITIONAL Later we'll accept integers as ids.
		  :id (prin1-to-string (incf (car emt:testral:*id-counter*)))
		  :relation "trace" ;;For now relations are strings
		  :governor 'comparison-w/persist
		  :value
		  (list
		     ;;Result, coerced to boolean
		     (if result t nil)
		     stored-value
		     backend
		     id)
		  ;;Failing the comparison does not neccessarily imply
		  ;;a bad grade, that's for emt:assert to decide.
		  :badnesses '()))
	    result)
	       
	 ;;If we can't get the object, make a note.
	 (emdb:error
	    (emtt:testral:add-note
	       (emt:testral:make-not-in-db
		  :id-in-db id
		  :backend  backend
		  :value    value
		  :badnesses 
		  (emt:testral:make-grade:ungraded
		     :contents 
		     "ID is not in the database")))
	    (setq emtt:*abort-p* t)))))





;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/persist)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/persist.el ends here
