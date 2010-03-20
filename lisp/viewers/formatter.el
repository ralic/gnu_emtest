;;;_ viewers/formatter.el --- Formatter from structured data to text

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: 

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
(require 'pp) ;;Alternatively, cust-print

;;;_. Body
;;;_ , Structures
;;;_  . stage2-state
;;For now, not making a structure, just punting.
(defsubst emt:plain-viewer:make-stage2-state (depth)
   ""
   depth)
(defsubst emt:plain-viewer:stage2-state->depth (state)
   ""
   (1+ state))

(defsubst emt:plain-viewer:stage2-state->nested-state (state)
   ""
   (1+ state))

;;;_ , Constants
;;;_  . emt:plain-viewer:stage2-initial-state
(defconst emt:plain-viewer:stage2-initial-state
   (emt:plain-viewer:make-stage2-state 0) 
   "The initial state for a stage2 parse.
Basically means outline depth = 0." )
;;;_ , emt:plain-viewer:->format-stage2
;;Some of the cases are fairly redundant.  `sequence' (Now the same as
;;just a list whose head is not a symbol).  `w/headline' vs `section'
;;taking a headline arg.
(defun emt:plain-viewer:->format-stage2 (tree depth-state)
   ""

   (cond
      ((consp tree)
	 (case (car tree)
	    (sequence
	       (cons
		  'sequence
		  (list*
		     (mapcar 
			#'(lambda (x)
			     (emt:plain-viewer:->format-stage2
				x
				(emt:plain-viewer:stage2-state->nested-state 
				   depth-state)))
			(cdr tree)))))
	    ;;`list' distinguishes its direct elements.  Next stage
	    ;;could allow list bullets.
	    (list
	       (cons
		  'sequence
		  (apply #'append
		     (mapcar
			#'(lambda (x)
			     (list
				(emt:plain-viewer:->format-stage2 
				   x
				   (emt:plain-viewer:stage2-state->nested-state 
				      depth-state))
				'(sep 2)))
			(cdr tree)))))

	    (w/headline
	       (destructuring-bind (args headline &rest body) (cdr tree)
		  (cons
		     'sequence
		     (list
			`(start-headline 
			    ,(emt:plain-viewer:stage2-state->depth depth-state)
			    ;;Pass a face argument according to style
			    ;;args.  And something to control
			    ;;foldedness.
			    
			    )
			headline
			'(sep 3)
			;;The remainder is a single item, encompassed
			;;in a section.
			;;Properties are still to be determined.
			`(section-x ()
			    ,@(mapcar 
				 #'(lambda (x)
				      (emt:plain-viewer:->format-stage2
					 x 
					 (emt:plain-viewer:stage2-state->nested-state 
					    depth-state)))
				 body))))))
	    (section
	       (destructuring-bind (args &rest body) (cdr tree)
		  (cons
		     'sequence
		     (append
			(mapcar
			   #'(lambda (x)
			     (emt:plain-viewer:->format-stage2
				x
				(emt:plain-viewer:stage2-state->nested-state 
				   depth-state)))
			   body)
			'((sep 3))))))

	    (data-persist-used
	       (let*
		  (  (props (caddr tree))
		     (bare-str
			;;$$Possibly should be
			;;`custom-prin1-to-string' instead.
			(pp-to-string
			   (second tree)))
		     
		     (str
			(if props
			   (apply #'propertize bare-str props)
			   bare-str)))
		  
;; 		  (list
;; 		     'sequence
;; 		     '(sep 2)
;; 		     str
;; 		     '(sep 2))
		  `(sequence (sep 2) ,str (sep 2))))
	    
	    ;;Separators pass thru unchanged.
	    (sep tree)))
      
      ((stringp tree) tree)))

;;;_  . Tests


;;The output here may have only text and quasi-text and `sequence'.
(rtest:deftest emt:plain-viewer:->format-stage2
   ;;An inspection test.  Manual for now.
   ;;All should check (check-type X formatter:stage2)

   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name simple))
	 (emt:plain-viewer:->format-stage2
	    (emt:plain-viewer:->format-tree
	       (emt:eg (type event-list)))
	    emt:plain-viewer:stage2-initial-state
	    )))
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	  ((name aborted-after-events))
	 (emt:plain-viewer:->format-stage2
	    (emt:plain-viewer:->format-tree (emt:eg (type
						       event-list)))
	    emt:plain-viewer:stage2-initial-state)))

   ;;Grade events
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name pass))
	 (emt:plain-viewer:->format-stage2
	    (emt:plain-viewer:->format-tree (emt:eg (type
						       grade-event)))
	    )))

   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name fail))
	 (emt:plain-viewer:->format-stage2
	    (emt:plain-viewer:->format-tree (emt:eg (type
						       grade-event)))
	    emt:plain-viewer:stage2-initial-state)))

   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name simple))
	 (emt:plain-viewer:->format-stage2
	    (emt:plain-viewer:->format-tree 
	       (emt:eg (type result-group)(name simple)))
	    emt:plain-viewer:stage2-initial-state)))
  

   (  "Param: A data object (of type `data-persist-used').
Response: Return value has, somewhere in it, a string with those
objects as properties.
"
      (emt:eg:narrow 
	 ;;This would usually have (library persist) but this part
	 ;;merges persist and viewer functionality (though viewer is
	 ;;not yet using these examples)
	 ((project emtest)(section persist-viewer))
	 (require 't/emt-persist "t/emt-persist.el")
	 (emt:db:internal:ts:mock (emt:eg (type versions))
	    (let
	       ((result
		   (emt:plain-viewer:->format-stage2
		      (emt:plain-viewer:->format-tree 
			 (emt:eg (type result-diag)(foundp t)))
		      emt:plain-viewer:stage2-initial-state)))

	       ;;Test that there's a string with those properties
	       (assert
		  (emt:somewhere-in-tree
		     #'(lambda (a placeholder diag-call)
			  (and
			     (stringp a)
			     (equal
				(get-text-property 1 'emt:diag:tried a)
				placeholder)
			     (equal
				(get-text-property 1 'emt:diag:call a)
				diag-call)))
		     result
		     (emt:eg (type tried)(foundp t))
		     (emt:eg (type result-diag)(foundp t))) 
		  t)
	       
	       ;;This is here just for viewability.  Later it will
	       ;;itself be a persist object.
	       ;;`result'  wants to be viewable
	       t
	       )))
      )


   )

;;;_ , Convert stage2 to stage3

;;The input here may have only text and quasi-text and outline-ish
;;structurers.  The rendering would vary by the target mode: outline,
;;org, text.

;;;_  . Types
;;Requires deep-type-checker.el for `list' with args.
;;Untried
(deftype formatter:stage1 ()
   '(or
       string
       (list (member sep) integer)

       ;;(repeat formatter:stage1) ;;Maybe instead of `sequence'
       (list* (member sequence) (repeat formatter:stage1))
       (list* (member list) (repeat formatter:stage1))
       (list* (member w/headline) (repeat *) formatter:stage1 
	  (repeat formatter:stage1))
       (list (member section)  (repeat *) (repeat formatter:stage1))
       (list (member data-persist-used) * *)))


;;Requires deep-type-checker.el for `list' with args.
(deftype formatter:stage2 ()
   '(or
       string
       (list (member sep) integer)

       ;;(repeat formatter:stage2) ;;List, replacing `sequence'
       (list* (member sequence) (repeat *))
       ;;First is property-list suitable for next stage.
       (list* (member section-x) (repeat *) (repeat *))
       (list (member start-headline) integer (satisfies facep))
       (list (member end-headline))))

;;;_  . emt:plain-viewer:stage2->stage3
(defun emt:plain-viewer:stage2->stage3 (tree)
   ""
   
   (cond
      ((consp tree)
	 (case (car tree)
	    ;;I've since redesigned this - lists that don't start with
	    ;;a symbol are just recursed on.
	    (sequence
	       (mapcar #'emt:plain-viewer:stage2->stage3
		  (cdr tree)))
	    (section-x
	       `(overlay ,(cadr tree)
		   ,(mapcar #'emt:plain-viewer:stage2->stage3
		       (cddr tree))))
	    (sep
	       (case (cadr tree)
		  (0 " ")
		  (1 " - ")
		  (2 '(nl-if-none))
		  (3 "\n")
		  (4 '((nl-if-none) "\n"))
		  (5 (list '(nl-if-none) (make-string 20 ?-)"\n"))
		  (6 (list '(nl-if-none) "\n" (make-string 40 ?*)"\n\n"))))
	    (start-headline
	       (list
		  (apply
		     #'(lambda (depth &optional face)
			  (list "\n" (make-string depth ?*)))
		     (cdr tree))
		  " "))
	    (end-headline "\n")))
      
      (t tree)))

;;;_  . Tests
(rtest:deftest emt:plain-viewer:stage2->stage3
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name simple))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-info-about))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-child-events))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-child-event-groups))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-mixed-children-1))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-mixed-children-2))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name aborted-no-events))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name aborted-after-events))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
    
   (  "Param: A diag that says a call tried to use a persist.
Response: A string in it is usable by a command."
      (emt:eg:narrow 
	 ;;This would usually have (library persist) but this part
	 ;;merges persist and viewer functionality (though viewer is
	 ;;not yet using these examples)
	 ((project emtest)(section persist-viewer))
	 (require 't/emt-persist "t/emt-persist.el")
	 (emt:db:internal:ts:mock (emt:eg (type versions))
	    (let
	       ((result
		   (emt:plain-viewer:stage2->stage3
		      (emt:plain-viewer:->format-stage2
			 (emt:plain-viewer:->format-tree 
			    (emt:eg (type result-diag)(foundp t)))
			 emt:plain-viewer:stage2-initial-state))))

	       ;;This is not the right place to test that commands
	       ;;work when point is on that string.
	       (assert
		  (emt:somewhere-in-tree
		     #'(lambda (a)
			  (and
			     (stringp a)
			     ;;Not looking at that property's value,
			     ;;just its existence.
			     (get-text-property 1 'emt:diag:tried a)
			     (get-text-property 1 'emt:diag:call a)))
		     result) 
		  t)
	       
	       ;;This is here just for viewability.  Later it will
	       ;;itself be a persist object.
	       ;;`result' wants to be viewable.
	       t
	       )))
      )

   
   )
;;;_ , Insert format-tree into current buffer

;;Input can only have text and quasi-text structure.

;;;_  . Types

(deftype formatter:stage3 ()
     '(or
	 string
	 ;;First is property-list
	 (list* (member overlay) (repeat *) (repeat *))
	 (list (member nl-if-none))
	 (repeat formatter:stage3)))



;;;_  . emt:plain-viewer:insert
;;$$MOVE ME
;;$$RENAME ME
;;$$USE ME
(defun emt:plain-viewer:insert (tree)
   "Print a format-tree into the current buffer.
TREE must be a format-tree.
The insert function is always `insert'."
   
   (cond
      ((consp tree)
	 (if (symbolp (car tree))
	    (case (car tree)
	       (overlay
		  (destructuring-bind (properties &rest r) (cdr tree)
		     (let
			(  ;;Remember where point was
			   (beg (set-marker (make-marker) (point))))
			
			;;Recurse
			(mapcar #'emt:plain-viewer:insert r)

			;;Now we know the text the overlay must cover,
			;;so we can create it.  Couldn't create it
			;;earlier and let it advance, because
			;;rear-advance must be nil.
			(let
			   ((ov
			       (make-overlay beg (point) nil nil nil)))
			   (dolist (prop properties)
			      (destructuring-bind (name value) prop
				 (overlay-put ov name value)))
			   ;;Now don't need the marker any more.
			   (set-marker beg nil)))))
	       
	       (nl-if-none
		  (unless (bolp) (insert "\n"))))
	    
	    (mapcar #'emt:plain-viewer:insert tree)))
      (t (insert tree))))

;;;_   , Tests

(rtest:deftest emt:plain-viewer:insert

   (  "Param: A bare string
Response: Inserts that string."
      (with-temp-buffer
	 (emt:plain-viewer:insert "a")
	 (equal "a" (buffer-string))))
   
   (  "Param: A list of strings
Response: Inserts those strings in order."
      (with-temp-buffer
	 (emt:plain-viewer:insert '("a" "b" "c"))
	 (equal "abc" (buffer-string))))

   (  "Param: A tree of strings
Response: Inserts those strings in order."
      (with-temp-buffer
	 (emt:plain-viewer:insert '("a" ("b") "c"))
	 (equal "abc" (buffer-string))))
      
   (  "Param: A string with properties
Response: The inserted text has the same properties."
      (with-temp-buffer
	 (let
	    ((str "abc")
	       (props '(face bold)))
	    (set-text-properties 0 3 props str)
	    (emt:plain-viewer:insert str)
	    (equal 
	       (text-properties-at 1)
	       props))))
   ;;Overlays

   (  "Param: An overlay spec.
Afterwards: There's an overlay encompassing just the data that was
given inside the overlay spec."
      (with-temp-buffer
	 (emt:plain-viewer:insert
	    '("123"
		;;An overlay, no properties
		(overlay () "45" "67")
		"890"))
	 (overlay-recenter 1)
	 (let*
	    ;;Get all the overlays after center, which is all of them.
	    ((overlays (cdr (overlay-lists)))
	       ;;Convert them into comparable data
	       (overlay-data
		  (mapcar
		     #'(lambda (ov)
			  (list
			     (overlay-start ov)
			     (overlay-end ov)))
		     overlays)))
	    
	    ;;Compare to expected value
	    (assert
	       (equal
		  overlay-data
		  '((4 8)))
	       t))
	 t))
   
   ;;nl-if-none

   (  "Situation: Point is not at beginning of line.
Operation: (nl-if-none)
Behavior: Insert a new line."
      (with-buffer-containing-object 
	 (:string "abc!" :point-replaces "!")
	 (emt:plain-viewer:insert '(nl-if-none))
	 (assert
	    (equal "abc\n" (buffer-string))
	    t)
	 
	 t))
   
   (  "Situation: Point is at beginning of line.
Operation: (nl-if-none)
Behavior: Insert nothing."
      (with-buffer-containing-object 
	 (:string "abc\n!" :point-replaces "!")
	 (emt:plain-viewer:insert '(nl-if-none))
	 (assert
	    (equal "abc\n" (buffer-string))
	    t)
	 
	 t))


   ;;Indent region - not yet

   )


;;;_ , Top

;;;_  . formatter:top-x
(defun formatter:top-x (buf-name obj format-func)
   ""
   
   (let
      ((buf
	  (get-buffer-create buf-name)))
      (with-current-buffer buf
	 (erase-buffer)
	 (funcall format-func obj))
      (pop-to-buffer buf)))

;;;_  . formatter:display-from-stage1
(defun formatter:display-from-stage1 (obj buf-name)
   "
OBJ is an object in stage 1 format
FUNC-1->2 transforms obj to a level 2 representation"
   
   (formatter:top-x buf-name obj
      #'(lambda (stage-1-obj)
	   (emt:plain-viewer:insert
	      (emt:plain-viewer:stage2->stage3
		 (emt:plain-viewer:->format-stage2
		    stage-1-obj
		    emt:plain-viewer:stage2-initial-state))))))


;;;_. Footers
;;;_ , Provides

(provide 'viewers/formatter)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/formatter.el ends here
