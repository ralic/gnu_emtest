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
      emt:testral:*id-counter*
      emt:testral:*prestn-path*
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
   "Return the current TESTRAL parent-id or `nil' if none."
   (car emt:testral:*parent-id*))
;;;_   , emtt:testral:with-parent-id
(defmacro emtt:testral:with-parent-id (id &rest body)
   "Evaluate BODY with ID as the current TESTRAL parent-id."
   
   `(let
       ((emt:testral:*parent-id*
	   (emtt:testral:create-parent-id ,id)))
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
	     (emt:testral:*parent-id*   (third ,obj-sym)))
	  ,@body)))
;;;_ , Presentation-paths
;;;_  . emtt:testral:make-prestn-path
(defun emtt:testral:make-prestn-path (&optional name)
   "Return a presentation path with no components"
   (let
      ((data (list
		'parent
		(emtt:testral:get-parent-id))))
      (list data)))


;;;_  . emtt:testral:add-to-prestn-path
(defun emtt:testral:add-to-prestn-path (name path)
   "Return PATH with NAME added as its leafward prefix."
   (append 
      (reverse
	 (if (listp name)
	    name
	    (list name)))
      path))
;;;_  . emtt:testral:update-prestn-path
(defun emtt:testral:update-prestn-path (name)
   "Return the current presentation path with NAME added.
If there is none, create one.  Caller has the responsibility of
scoping it."
   (when (emtt:testral:p)
      (let* 
	 ((prestn-path
	     (if (boundp 'emt:testral:*prestn-path*)
		emt:testral:*prestn-path*
		(emtt:testral:make-prestn-path))))
	 (emtt:testral:add-to-prestn-path
	    name prestn-path))))


;;;_  . emtt:testral:with-prestn-path (Entry point)
;;;###autoload
(defmacro emtt:testral:with-prestn-path (name &rest body)
   "Evaluate BODY with a presentation-path defined.
NAME should be nil, a `emt:testral:id-element' or list of
`emt:testral:id-element'.
This is intended for notes that should only be made in abnormal
circumstances, but that still want scoping."
  
   `(let
       ((emt:testral:*prestn-path*
	   (emtt:testral:update-prestn-path ,name)))
       ,@body))

;;;_  . emtt:testral:get-anchor-id
(defun emtt:testral:get-anchor-id (data)
   "Return the id of an anchor note, according to DATA.
Make one if neccessary, setting DATA accordingly.
DATA must the head of a presentation-path."
   (if
      (eq (car data) 'parent)
      ;;No parent note made yet.  Make one and store its id.
      (let ((parent-id (emtt:testral:new-id)))
	 (emtt:testral:add-note-aux 
	    parent-id
	    (emtt:testral:get-parent-id)
	    '()
	    relation nil 'scope)
	 (setf (first data)  'self)
	 (setf (second data)  id)
	 parent-id)
      ;;Parent note already made.  Re-use it.
      (progn
	 (assert (eq (car data) 'self))
	 (second data))))
;;;_ , Making notes
;;;_  .  emtt:testral:add-note-aux
(defun emtt:testral:add-note-aux 
   (id parent-id prestn-path relation grade governor &rest args)
   "Add a TESTRAL note.
Must be called in a TESTRAL scope.

RELATION gives the relation to the parent note or the suite.  It
must be a `emt:testral:id-element'.

GOVERNOR is a symbol indicating a specific formatter for the output."
   (assert (emtt:testral:p))
   (emtt:testral:push-note
      (condition-case err
	 (progn
	    (check-type relation emt:testral:id-element)
	    (check-type governor symbol)
	    (check-type grade    emt:testral:grade-aux)
	    (emt:testral:make-newstyle
	       :id          id
	       :parent-id   parent-id
	       :prestn-path prestn-path
	       :relation    relation
	       :governor    governor
	       :value       args
	       ;;Failing the comparison does not neccessarily imply
	       ;;a bad grade, that's for emt:assert to decide.
	       :badnesses   grade))
	 (error
	    (emt:testral:make-newstyle
	       :id          id
	       :parent-id   parent-id
	       :prestn-path '()
	       :relation    'problem
	       :governor    'error-raised
	       :value       err
	       :badnesses 
	       (emt:testral:make-grade:ungraded
		  :contents
		  "An error was seen while storing a note"))))))
;;;_  . Entry points
;;;_   , emtt:testral:add-note-w/id
(defun emtt:testral:add-note-w/id (id relation grade governor &rest args)
   "Add a TESTRAL note.

RELATION gives the relation to the parent note or the suite.  It
must be a `emtvp:relation-element' - for now, that's a string.

GOVERNOR is a symbol indicating a specific formatter for the output."
   ;;$$IMPROVE ME This should be protected by the condition-case.
   (when (emtt:testral:p)
      (if (boundp 'emt:testral:*prestn-path*)
	 (let* 
	    (
	       (prestn-data
		  (reverse emt:testral:*prestn-path*))
	       (head
		  (car prestn-data))
	       (prestn-path
		  (cdr prestn-data))
	       (parent-id
		  (emtt:testral:get-anchor-id head)))
	    
	    (apply
	       #'emtt:testral:add-note-aux
	       id
	       parent-id
	       prestn-path
	       relation grade governor args))
	    
	 (apply
	    #'emtt:testral:add-note-aux
	    id
	    (emtt:testral:get-parent-id)
	    '()
	    relation grade governor args))))

;;;_   , emtt:testral:add-note
(defun emtt:testral:add-note (&rest args)
   ""
   (apply #'emtt:testral:add-note-w/id
      (emtt:testral:new-id)
      args))

;;;_   , emtt:testral:note-list
(defun emtt:testral:note-list ()
   ""
   (unless 
      (emtt:testral:p)
      (error "Not in a TESTRAL collection scope"))
   (emt:testral:make-note-list
      :notes (emtt:testral:get-notes)))

;;;_ , emtt:testral:set-object-origin
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
