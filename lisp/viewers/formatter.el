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
;;This might become or be moved into hiformat, or be split into calls
;;available in hiformat
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
	    ;;`list' distinguishes its direct elements.
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

	    ;;This was premature
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


;;;_ , Outline stage
(require 'viewers/outformat)

;;;_ , Insert format-tree into current buffer

;;Input can only have text and quasi-text structure.
(require 'viewers/loformat)



;;;_ , Top

;;;_  . formatter:top-x
'
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
'
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
