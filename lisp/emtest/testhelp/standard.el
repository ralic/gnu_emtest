;;;_ emtest/testhelp/standard.el --- Standard testhelp for Emtest

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

(require 'emtest/common/testral-types)
(require 'emtest/common/result-types)
(require 'emtest/runner/testral)
(require 'emtest/runner/emt-funcall)  ;;$$OBSOLESCENT

;;Defined in `emtest/runner/emt-funcall/testhelp'
(defvar emt:report-control:thd:report-all)

;;;_ , Customization

(defgroup emtest/testhelp ()
   "Customization for Emtest testhelp"
   :group 'emtest)


;;;_. Body
;;;_ , Doc
;;;_  . emt:doc
;;;###autoload
(defun emt:doc (str &rest r)
   ""
   (emt:testral:add-note
      (emt:testral:make-doc :str str)))

;;;_  . emt:stage
;;$$WRITE ME
;;See  [[id:47ad9e14-9a38-40e2-a5ea-91cbc4dfb97f][redesign]]: Now this
;;just stores a note 

;;This is a scoped form that stores a pop note when it's done.  
(defmacro emt:stage (stage-args &rest body)
   "Run BODY in an Emtest stage"
   
   `(progn ,@body))

;;Usage will be something like:
'
(emtg:narrow-f
   `(list (list ',tag ,name))
   `(emt:stage 
     ("Iteration" ;;Name
      ;;$$CHANGE ME This will become a parameter note
      (emt:testral:add-note
       (emt:testral:make-doc 
	:str 
	(concat 
	 (prin1-to-string ',tag)
	 " = " 
	 (prin1-to-string ,name)))))
     ,@body))

;;;_ , emth:try-all
;;For now, it only watches for errors.  Doesn't try to logically
;;conjoin branches.
;;Early, I want it to map over the branches, so we can wrap map forms
;;in this.
;;$$TEST ME
;;$$USE ME  In particular, emtg:map should use emth:map&trap
(defmacro emth:try-all (&rest forms)
   "Error if any branch errors, but try all branches"
   (let
      ((form (make-symbol "form")))
      `(let
	  ((emtt:*abort-p* nil))
	  (declare (special emtt:*abort-p*))
	  (dolist (,form ',forms)
	     (emth:trap-errors ,form))
	  (when emtt:*abort-p*
	     (signal emt:already-handled ())))))

;;$$TEST ME
;;$$USE ME  In particular, emtg:map should use emth:map&trap
(defun emth:map&trap (func list)
   ""
   (declare (special emtt:*abort-p*))
   (let
      ((emtt:*abort-p* nil)
	 (results
	    (mapcar
	       #'(lambda (el)
		    (emth:trap-errors (funcall func el)))
	       list)))
      
      (if emtt:*abort-p*
	 (signal emt:already-handled ())
	 results)))

;;Usage: On forms, just (emth:map&trap #'eval form-list)


;;;_ , "should"
;;;_  . emth:wrap-form
;; OBSOLESCENT.  Only used in `emth:should-f;
(defun emth:wrap-form (form)
   ""

   (if
      (and
	 (listp form)
	 (eq (car form) 'equal))
      `(emt:funcall (function ,(car form)) ,@(cdr form))
      form))

;;;_  . emth:should-f
;;Done in an obsolete way
(defun emth:should-f (form)
   ""
   ;;$ADDME When `emt:testral:*parent-id*' etc are not bound, act like
   ;;plain `assert'.  Or possibly push results onto a ring that can be
   ;;examined. 
   (declare (special emt:testral:*id-counter* emt:testral:*parent-id*))


   (let*
      (
	 (parent-id emt:testral:*parent-id*)
	 ;;Use a counter or a uuid.
	 (id (incf emt:testral:*id-counter*))
	 ;;Bind a new parent id
	 (emt:testral:*parent-id* id)
	 
	 ;;Create empty badnesses, just because `unwind-protect' must
	 ;;see it
	 (badnesses '()))
      (emt:testral:add-note
	 (emt:testral:make-check:push
	    :info (list (list 'form form))
	    :parent-id parent-id
	    :id id))
      


      ;;`unwind-protect' this, in place of the last form:
      ;;Make&send a `emt:testral:check:pop'.  To know
      ;;badnesses, if any: Each case assigns to the `badnesses'
      ;;variable, which we use in making that. 
      
      ;;$$CHANGEME This protect call is obsolete, assumes the old
      ;;style and special behavior isn't needed with the new design.
      ;;So build things normally and just use `unwind-protect'
      (emt:trace:protect
	 (condition-case err
	    (let*
	       (  
		  (form-x (emth:wrap-form form))
		  (retval
		     (eval form-x)))
	       (unless retval
		  (push '(failed) badnesses))
	       retval)

	    ;;$$ADDME There'd be a clause to intercept a special
	    ;;"dormant" error too, for contributing checks that were
	    ;;disabled.

	    ;;And a `many-bads' error that directly held a collected
	    ;;list of badnesses?  In case `and' both fails and is
	    ;;dormant.  Maybe that's the special error to use always.

	    ;;Add to badnesses, unless already there.
	    ('emt:already-handled 
	       (pushnew '(ungraded) badnesses)
	       (signal 'emt:already-handled ()))
	    ;;Add to badnesses, unless already there.
	    (error
	       (pushnew '(ungraded) badnesses)
	       (signal (car err)(cdr err))))

	 ;;This is now the meat of the operation.
	 (emt:testral:add-note
	    (emt:testral:make-check:pop
	       :parent-id parent-id
	       :id id
	       ;;Punt
	       :badnesses badnesses)))))


;;;_  . should
;;Essentially unused except a bit of exploration in early development.
;;;###autoload
(defmacro* should (form &key doc)
   ""

   `(emth:should-f ',form))


;;;_   , Test helper

(defmacro* emth:should:th 
   ((&key 
       initial-stored 
       (report-control emt:report-control:thd:report-all))
      &rest body)
   ""
   
   `(let
       ((emt:trace:current-event-list ,initial-stored)
	  ;;Other control such as report-control is not supported yet,
	  ;;so can't control it here.
	  )
       
       (list
	  (progn ,@body)
	  emt:trace:current-event-list)
       ;;We aren't interested in "should"s return value
       emt:trace:current-event-list))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/standard)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;;  emtest/testhelp/standard.el ends here

