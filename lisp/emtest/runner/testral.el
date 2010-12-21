;;;_ emtest/runner/testral.el --- Testral functions for emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
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



;;;_. Body
;;;_ , Declarations
(declare
   (special 
      emt:testral:*events-seen*
      emt:testral:*path-prefix*
      emt:testral:*id-counter*
      emt:testral:*parent-id*))
;;;_ , Support
;;;_  . Predicates
;;;_   , emtt:testral:p
(defsubst emtt:testral:p ()
   "Non-nil if called in a scope collecting TESTRAL notes"
   (boundp 'emt:testral:*events-seen*))

;;;_  . Note IDs
;;;_   , emtt:testral:create-counter
;;Counter to make unique IDs.  Although UUIDs are appealing, they are
;;slower to make.
(defsubst emtt:testral:create-counter ()
   "Create a TESTRAL counter"
   (list 1))
;;;_   , emtt:testral:new-id
(defsubst emtt:testral:new-id ()
   "Get a node id.
This uses a TESTRAL counter."
   ;;$$TRANSITIONAL Later we'll accept integers as ids.
   (prin1-to-string (incf (car emt:testral:*id-counter*))))
;;;_   , emtt:testral:create-parent-id
(defsubst emtt:testral:create-parent-id (id)
   "Create a TESTRAL parent-id container"
   (list id))

;;;_   , emtt:testral:get-parent-id
(defsubst emtt:testral:get-parent-id ()
   "Return the current TESTRAL parent-id"
   (car emt:testral:*parent-id*))
;;;_   , emtt:testral:with-parent-id
(defmacro emtt:testral:with-parent-id (id &rest body)
   "Evaluate BODY with ID as the current TESTRAL parent-id"
   
   `(let
       ((emt:testral:*parent-id*
	   (emtt:testral:create-parent-id id)))
       ,@body))

;;;_  . Note queues.
;;;_   , emtt:testral:create
(defsubst emtt:testral:create ()
   "Create a TESTRAL receiver"
   (list '()))
;;;_   , emtt:testral:push-note
(defsubst emtt:testral:push-note (note)
   "Push a TESTRAL note"
   (when
      (emtt:testral:p)
      (push note
	 (cdr emt:testral:*events-seen*))))
;;;_   , emtt:testral:get-notes

;;Reverse the note list so it's in the order that it was received in.
(defsubst emtt:testral:get-notes ()
   "Return a list of the notes received in the same order they were
received in."
   (nreverse (cdr emt:testral:*events-seen*)))
;;;_ , Entry points primarily for Emtest itself
;;;_  . emtt:testral:with
(defmacro emtt:testral:with (&rest body)
   "Evaluate BODY with TESTRAL facilities available"
   
   `(let*
      (
	 (emt:testral:*id-counter*  (emtt:testral:create-counter))
	 (emt:testral:*events-seen* (emtt:testral:create))
	 (emt:testral:*path-prefix* ())  ;;$$OBSOLESCENT
	 (emt:testral:*parent-id*   (emtt:testral:create-parent-id nil)))
       ,@body))

;;;_  . Continued note-collecting
;;;_   , emtt:testral:make-continuing
(defun emtt:testral:make-continuing ()
   "Make an object suitable for use in `emtt:testral:continued-with'."
   
   (list 
      (emtt:testral:create-counter) 
      (emtt:testral:create)
      (emtt:testral:create-parent-id nil)))


;;;_   , emtt:testral:continued-with
(defmacro emtt:testral:continued-with (obj &rest body)
   "Evaluate BODY with TESTRAL facilities available.
OBJ should be an object made by `emtt:testral:make-continuing'.  
This continues any previous invocations of
`emtt:testral:continued-with' with the same OBJ argument.
"

   (let
      ((obj-sym (make-symbol "obj")))
      `(let*
	  (
	     (,obj-sym ,obj)
	     (emt:testral:*id-counter*  (first ,obj-sym))
	     (emt:testral:*events-seen* (second ,obj-sym))
	     (emt:testral:*path-prefix* ())  ;;$$OBSOLESCENT
	     (emt:testral:*parent-id*   (third ,obj-sym)))
	  ,@body)))

;;;_ , Entry points for test code and its support
;;;_  .  emtt:testral:add-note
(defun emtt:testral:add-note (relation grade governor &rest args)
   "Add a TESTRAL note.

RELATION gives the relation to the parent note or the suite.  It
must be a `emtvp:relation-element' - for now, that's a string.

GOVERNOR is a symbol indicating a specific formatter for the output."
   (when (emtt:testral:p)
      (emtt:testral:push-note
	 (condition-case err
	    (progn
	       (check-type relation emtvp:relation-element)
	       (check-type governor symbol)
	       (check-type grade    emt:testral:grade-aux)
	       (emt:testral:make-newstyle
		  :id        (emtt:testral:new-id)
		  :parent-id (emtt:testral:get-parent-id)
		  :relation  relation
		  :governor  governor
		  :value     args
		  ;;Failing the comparison does not neccessarily imply
		  ;;a bad grade, that's for emt:assert to decide.
		  :badnesses grade))
	    (error
	       (emt:testral:make-newstyle
		  :id        (emtt:testral:new-id)
		  :parent-id (emtt:testral:get-parent-id)
		  :relation  'problem
		  :governor  'error-raised
		  :value     err
		  :badnesses 
		  (emt:testral:make-grade:ungraded
		     :contents
		     "An error was seen while storing a note")))))))



;;;_  . emtt:testral:report-false
;;Higher level, may belong elsewhere.
;;$$IMPROVE ME  Give this its own type of note.
;;$$RETHINK ME  Since we're no longer using presentation prefix, callers
;;need to do something else, perhaps emtt:testral:with-parent-id.
(defun emtt:testral:report-false (prestn-prefix str)
   "Report that a compare leaf was false"
   (when (emtt:testral:p)
      (emtt:testral:add-note
	 "trace"
	 nil
	 'fail
	 str)))


;;;_  . emtt:testral:note-list
(defun emtt:testral:note-list ()
   ""
   (unless 
      (emtt:testral:p)
      (error "Not in a TESTRAL collection scope"))
   (emt:testral:make-note-list
      :notes (emtt:testral:get-notes)))

;;;_  . emtt:testral:set-object-origin
(defun emtt:testral:set-object-origin (object origin)
   ""

   ;;Punt for now.  Later, store its identity on some sort of alist.
   (let*
      ()
      
      ))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/testral)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/testral.el ends here
