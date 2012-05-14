;;;_ emtest/viewer/emformat.el --- Formatting functions specific to Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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

(require 'custom)
(require 'emtest/main/find-tests)
(require 'emtest/types/run-types)
(require 'emtest/types/testral-types)
(require 'emtest/viewer/all-note-formatters)
(require 'emtest/viewer/sumgrades)
(require 'emtest/viewer/view-types)
(require 'formatter/hiformat)
(require 'formatter/outline)
(require 'utility/dynvars)
(require 'utility/pathtree)

;;;_. Body
;;;_ , Data
;;;_  . Faces
;;;_   , Grades
(defface emtvf:face:ok 
   '((default :foreground "green3" :weight bold))
   "Face for reporting passed tests"
   :group 'emtest)

(defface emtvf:face:failed 
   '((default :foreground "red" :weight bold))
   "Face for reporting failed tests"
   :group 'emtest)

(defface emtvf:face:ungraded
   '((default :foreground "red" :strike-through t))
   "Face for reporting ungraded tests"
   :group 'emtest)

(defface emtvf:face:blowout 
   '((default :foreground "black" :background "red" :weight bold))
   "Face for reporting blown-out tests"
   :group 'emtest)

(defface emtvf:face:dormant
   '((default :foreground "black"))
   "Face for reporting dormant tests"
   :group 'emtest)

;;;_   , Comparisons
(defface emtvf:face:mismatch
   '((default :foreground "pink" :weight bold))
   "Face for reporting mismatches.
NB, this is not a grade.  It indicates failure of a comparison,
which may not imply failure of an assertion."
   :group 'emtest)

(defface emtvf:face:ok-match
   '((default :foreground "green4" :weight bold))
   "Face for reporting correct matches.
NB, this is not a grade.  It indicates a successful comparison,
which may not imply success of an assertion."
   :group 'emtest)

;;;_   , Pieces
(defface emtvf:face:title
   '((default 
	:height 1.8
	:foreground "black"))
   
   "Face for displaying Emtest banner"
   :group 'emtest)
(defface emtvf:face:suitename
   '((default 
	:foreground "blue1"))
   
   "Face for displaying test names"
   :group 'emtest)
;;;_  . Configuration
;;;_   , emtvf:dynvars
(defconst emtvf:dynvars 
   (append
      '((emtvf:*hdln-path*))
      emtvf:outline:dynvars)
   "Dynamic variables that emformat uses" )

;;;_ , Lower format functions
;;;_  . emtvf:make-dynamic
(defun emtvf:make-dynamic (obj func)
   "Make a form that calls a dynamic object"
   `(dynamic 
       ,obj 
       ,func
       ,(utidyv:capture-vars emtvf:dynvars)))

;;;_ , Helper functions
;;;_  . Characterizing object representation
;;;_   , emtvf:short-obj-p 
(defun emtvf:short-obj-p (obj)
   "Return t is object's printed representation is fairly short"
   (or
      (symbolp obj)
      (and
	 (stringp obj)
	 (< (length obj) 40))))

;;;_  . Singles-Path
;;;_   , Special variables
(declare (special emtvf:*hdln-path*))

;;;_   , emtvf:with-blank-singles-path
(defmacro emtvf:with-blank-singles-path (&rest body)
   "Eval BODY in a blank singles-path."
   
   `(let ((emtvf:*hdln-path* '()))
       ,@body))
;;;_   , emtvf:with-more-singles-path
(defmacro emtvf:with-more-singles-path (name &rest body)
   ""
   
   `(let ((emtvf:*hdln-path* (cons name emtvf:*hdln-path*)))
       ,@body))

;;;_   , emtvf:singles-path
(defun emtvf:singles-path ()
   ""
   (apply #'nconc
      (mapcar
	 #'(lambda (x)
	      (list x " "))
	 (nreverse (remq nil emtvf:*hdln-path*)))))
;;;_  . Buttons
;;;_   , emtvf:button-explore-func
(defun emtvf:button-explore-func (button)
   "Explore explorable, given BUTTON."
   ;;$$IMPROVE ME Distinguish proplists we want to use.
   (emt:lch:run
      (button-get button 'how-to-run)
      emt:lch:proplist:vanilla
      (button-get button 'prestn-path)))

;;;_   , emtvf:button-to-explore
(defun emtvf:button-to-explore (explorable text)
   "Make a button to explore EXPLORABLE."
   (when explorable
      `(button ,text 
	  action ,#'emtvf:button-explore-func
	  help-echo "Rerun this test"
	  how-to-run  ,(emtt:explorable->how-to-run  explorable)
	  prestn-path ,(emtt:explorable->prestn-path explorable))))
;;;_   , emtvf:viewable->mark-text
(defun emtvf:viewable->mark-text (viewable)
   "Return the text of VIEWABLE's current mark."
   (if (emt:view:suite->mark viewable) "X" "_"))

;;;_   , emtvf:reprint-button
;;$$IMPROVE ME  Make this work for buttons in general, not just
;;(un)mark on viewables
(defun emtvf:reprint-button (button)
   "Cause BUTTON to be reprinted"
   (let
      (  (viewable (button-get button 'viewable))
	 (pos (button-start button)))

      ;;This removes the whole thing, overlay and all.
      (delete-region pos (button-end button))
      ;;Insert a new button at that position
      (save-excursion
	 (goto-char pos)
	 (loformat:insert
	    (emtvf:button-toggle-mark viewable)
	    emtv2:format-alist))))

;;;_   , emtvf:button-toggle-mark-func
(defun emtvf:button-toggle-mark-func (button)
   "Toggle the mark on viewable given on BUTTON."
   (let
      ((viewable (button-get button 'viewable)))
      (setf
	 (emt:view:suite->mark viewable)
	 (not (emt:view:suite->mark viewable)))
      (emt:ind:set-prop
	 (emtt:explorable->how-to-run
	    (emt:view:suite->how-to-run viewable))
	 'user-says-rerun
	 t)
      (emtvf:reprint-button button)))

;;;_   , emtvf:button-toggle-mark
(defun emtvf:button-toggle-mark (viewable)
   "Make a button to toggle the mark on VIEWABLE."
   (when viewable
      `(button ,(emtvf:viewable->mark-text viewable) 
	  action ,#'emtvf:button-toggle-mark-func
	  help-echo "Mark this test-suite"
	  viewable ,viewable)))

;;;_  . Objects
;;;_   , emtvf:obj-or-string
(defun emtvf:obj-or-string (value)
   "Display VALUE.
If VALUE is a string, display it literally, otherwise pretty-print it."
   (if
      (stringp value)
      ;;Indent it so it can't affect outline
      ;;structure. 
      `(indent 4 ,value)
      `(object ,value nil)))
;;;_  . Direct emformat support
;;;_   , emtvf:outline-item-emformat
(defmacro emtvf:outline-item-emformat (headtext contents &optional face fold)
   ""
   
   `(emtvf:outline-item
       (list (emtvf:singles-path) ,headtext)
       (emtvf:with-blank-singles-path ,contents)
       ,face
       ,fold))

;;;_   , emtvf:mapnodes 
(defun emtvf:mapnodes (list els=0)
   "Map emtvf:node over LIST, making dynamic entries"
   (hiformat:map 
      #'(lambda (obj &rest d)
	   (emtvf:make-dynamic 
	      obj 
	      #'emtvf:node))
      list
      :separator "\n"
      :els=0 els=0))

;;;_  . emtvf:shortcut-single
(defmacro emtvf:shortcut-single (name children rest-headline face
   format-no-child &optional fold)
   "Display an item and its children, or display its single child.
Intended for items that are basically just containers."
   (let
      ((name-sym (make-symbol "name"))
	 (children-sym (make-symbol "children")))
      `(let
	  ((,name-sym ,name)
	     (,children-sym ,children))
	  (if
	     (= (length ,children-sym) 1)
	     (emtvf:with-more-singles-path ,name-sym
		(emtvf:make-dynamic 
		   (car ,children-sym)
		   #'emtvf:node))
	     (emtvf:outline-item-emformat
		(list ,name-sym ,rest-headline)
		(emtvf:mapnodes ,children-sym ,format-no-child)
		,face
		,fold)))))

;;;_ , Format functions
;;;_  . emtvf:top

(defun emtvf:top (view-node)
   "Make a format form for VIEW-NODE.
VIEW-NODE must be at least an `emtvp:node'."

   (check-type view-node emtvp:node)
   (utidyv:top 
      emtvf:dynvars
      `(
	  (w/face "Emtest results" emtvf:face:title)
	  "\n"
	  ,(emtvf:sum-grades-long (emt:view:presentable->sum-grades view-node))
	  ,(emtvf:node view-node))))

;;;_  . emtvf:node
(defun emtvf:node (view-node)
   "Make a format form for VIEW-NODE.
VIEW-NODE must be an `emt:view:presentable'.
Must be called in a `utidyv:top' context."

   (check-type view-node emtvp:node)

   (let*
      ((suite view-node)
	 (name
	    (emtvp:node->name view-node))
	 (children
	    (emtvp:node->children view-node))
	 (grades
	    (emt:view:presentable->sum-grades suite))
	 (grade-face
	    (emtvf:grade-overall-face grades))
	 (grades-sum
	    (emtvf:sum-grades-short grades))
	 (boring-p 
	    (emtvf:grade-boring grades)))
      
      (etypecase suite
	 (emt:view:suite
	    (let*
	       (
		  (object
		     (emt:view:suite->result suite))
		  (explorable
		     (emt:view:suite->how-to-run suite)))
	       (etypecase object
		  (null "A null viewable")
		  ;; $$OBSOLESCENT
		  (emt:testral:test-runner-info
		     (emtvf:outline-item-emformat
			(hiformat:separate
			   (list
			      `(w/face ,name emtvf:face:suitename)
			      grades-sum)
			   " ")
		;; Each should xform to a button to launch that.
			(emt:testral:test-runner-info->explore-methods-supported
	 object)
			;;(emtvf:mapnodes children "No child suites")
			grade-face
			boring-p))
		  
		  
		  (emt:testral:suite
		     (emtvf:outline-item-emformat
			(hiformat:separate
			   (delq nil
			      (list
				 (emtvf:button-toggle-mark view-node)
				 `(w/face ,name emtvf:face:suitename)
				 (emtvf:button-to-explore explorable "[RUN]")
				 grades-sum))
			   " ")
			(emtvf:mapnodes children "No child suites")
			grade-face
			boring-p)))))
	 (emt:view:how-to-run
	    (emt:vw:how-to-run
	       (emt:view:how-to-run->contents view-node)
	       name))
	 
	 (emt:view:note
	    (emt:vw:note view-node))

	 (emt:view:note-placeholder
	    (emtvf:shortcut-single
	       nil
	       (emtvp:node->children view-node)
	       '()
	       nil
	       "[Note placeholder with no children]"))
	 
	 ;;Base type, appears for the root node.
	 (emt:view:presentable
	    (emtvf:shortcut-single 
	       nil
	       (emtvp:node->children view-node)
	       grades-sum
	       grade-face
	       "[Suite placeholder with no children]")))))


;;;_  . emt:vw:note (TESTRAL note formatter)
(defun emt:vw:note (obj &rest d)
   "Make a format form for OBJ.
OBJ must be a TESTRAL viewable (`emt:view:note')."
   (check-type obj emt:view:note)
   (condition-case err
      (let
	 ((note (emt:view:note->contents obj)))
	 (apply 
	    (emt:vw:note-get-formatter 
	       (emt:testral:note->governor note))
	    obj
	    (emt:testral:note->value note)))
      (error
	 `((w/face "Error in formatter: " emtvf:face:blowout) 
	     (object ,err nil)
	     "\n"))))
;;;_  . emt:vw:how-to-run
(defun emt:vw:how-to-run (obj name)
   "Make a format form for a emt:view:how-to-run, which encases a emtt:explorable."
   (emtvf:outline-item-emformat
      (list 
	 `(w/face ,(symbol-name name) emtvf:face:suitename)
	 " "
	 (emtvf:button-to-explore obj "[RUN]"))
      nil
      'emtvf:face:dormant))

;;;_ , About grades
;;;_  . Structure emtvf:grade-fmt
(defstruct (emtvf:grade-fmt
	      (:type list)
	      (:constructor emtvf:make-grade-fmt)
	      (:copier nil)
	      (:conc-name emtvf:grade-fmt->))
   "Describes how a given grade is formatter"
   (symbol () :type symbol
      :doc "The symbol that represents this grade-type.  Can also be
`ok' which represents any non-fail grade."
      )
   (fail-p () :type boolean
      :doc "True if grade is a type of bad grade")
   (face   () :type symbol
      :doc "The face to display this grade in")
   (plural "NO DESCRIPTION"
      :type string
      :doc "A string saying the plural of this, eg \"Failures\"")
   (singular "NO DESCRIPTION"
      :type string
      :doc "A string saying the singular of this, eg \"Failure\"")
   ;;$$RENAME this field "severity"
   (severity 0
      :type integer
      :doc "The severity of this grade type, higher numbers being more \
severe." 
      ))



;;;_  . Data
;;;_   , emtvf:grade-fmt-default
(defconst emtvf:grade-fmt-default 
   (emtvf:make-grade-fmt
      :symbol nil
      :fail-p nil
      :face   'emtvf:face:dormant
      :plural   "(UNUSED: No tests)"
      :singular "(UNUSED: No tests)"
      :severity 0)
   "The default grade formatting info" )
;;;_   , emtvf:grade-fmt-alist
(defconst emtvf:grade-fmt-alist 
   (list
      (emtvf:make-grade-fmt
	 :symbol 'blowout
	 :fail-p t
	 :face   'emtvf:face:blowout
	 :plural   "Blowouts"
	 :singular "Blowout"
	 :severity 100
	 )
      (emtvf:make-grade-fmt
	 :symbol 'ungraded
	 :fail-p t
	 :face   'emtvf:face:ungraded
	 :plural   "Ungraded tests"
	 :singular "Ungraded test"
	 :severity 75
	 )
      (emtvf:make-grade-fmt
	 :symbol 'fail
	 :fail-p t
	 :face   'emtvf:face:failed
	 :plural   "Failures"
	 :singular "Failure"
	 :severity 50
	 )
      (emtvf:make-grade-fmt
	 :symbol 'dormant
	 :fail-p t
	 :face   'emtvf:face:dormant
	 :plural   "Dormant tests"
	 :singular "Dormant test"
	 :severity 25
	 )
      ;;$$IMPROVE ME  Encap making a passing grade type, omitting
      ;;redundant info from arglist
      (emtvf:make-grade-fmt
	 :symbol 'ok
	 :fail-p nil
	 :face   'emtvf:face:ok
	 :plural   "(UNUSED: ok)"
	 :singular "(UNUSED: ok)"
	 :severity 10
	 )
      (emtvf:make-grade-fmt
	 :symbol 'test-case
	 :fail-p nil
	 :face   'emtvf:face:ok
	 :plural   "Test cases"
	 :singular "Test case"
	 :severity 10)
      emtvf:grade-fmt-default)
   
   "Alist of grade formatting info" )
;;;_   , emtvf:get-grade-info
(defun emtvf:get-grade-info (sym)
   "Return summary & formatting info about SYM.
SYM should be a grade symbol, but this returns a valid object in any case."
   (or 
      (assq sym emtvf:grade-fmt-alist)
      emtvf:grade-fmt-default))
;;;_  . Grade helpers
;;;_   , emtvf:grade-boring
(defun emtvf:grade-boring (obj)
   "Return non-nil if OBJ is all passing grades.
OBJ must be a `emt:grade:summary'"
   (let*
      ((nobj (emtvr:->grade-summary obj))
	 (worst (emt:grade:summary->worst nobj))
	 (info (emtvf:get-grade-info worst)))
      (not
	 (emtvf:grade-fmt->fail-p info))))

;;;_   , emtvf:grade-overall-face
(defun emtvf:grade-overall-face (obj)
   "Return a face that hints at the overall quality of grades in OBJ.
OBJ should be an `emt:grade:summary'."

   (let*
      ((nobj (emtvr:->grade-summary obj))
	 (worst (emt:grade:summary->worst nobj))
	 (info (emtvf:get-grade-info worst)))
      (emtvf:grade-fmt->face info)))

;;;_   , emtvf:map-grades
(defun emtvf:map-grades (func nobj &optional separator)
   "Map FUNC over the grades seen in NOBJ.
Any nil items are omitted, which makes a difference in separation.
SEPARATOR, if non-nil, is what separates the items."
   (hiformat:separate
      (delq nil
	 (mapcar
	    #'(lambda (grade)
		 (funcall func 
		    (emtvf:get-grade-info (first grade))
		    (second grade)))
	    (emt:grade:summary->grades nobj)))
      separator))
;;;_  . Grade formatters
;;;_   , emtvf:sum-grades-short
(defun emtvf:sum-grades-short (obj &rest d)
   "Give a summary of grades for this object."
   (let*
      (
	 (nobj (emtvr:->grade-summary obj))
	 (worst (emt:grade:summary->worst nobj)))
      
      (cond
	 ((null worst)
	    '(w/face "Nothing was tested" emtvf:face:dormant))
	 ((emtvf:grade-fmt->fail-p 
	     (emtvf:get-grade-info worst))
	    (list
	       '(w/face "Problems: " emtvf:face:failed)
	       (emtvf:map-grades
		  #'(lambda (info count)
		       (if
			  (emtvf:grade-fmt->fail-p info)
			  `(w/face 
			      ,(emtvf:grade-fmt->plural info)
			      ,(emtvf:grade-fmt->face   info))
			  '()))
		  nobj
		  ", ")
	       "."))
	 (t
	    '(w/face "All OK" emtvf:face:ok)))))


;;;_   , emtvf:sum-grades-long
(defun emtvf:sum-grades-long (obj &rest d)
   "Give a summary of grades for this object."
   (let*
      (
	 (nobj  (emtvr:->grade-summary obj))
	 (worst (emt:grade:summary->worst nobj))
	 (successes
	    (emtvf:map-grades
	       #'(lambda (info count)
		    (if
		       (not (emtvf:grade-fmt->fail-p info))
		       (hiformat:grammar:num-and-noun
			  count 
			  (emtvf:grade-fmt->singular
			     info)
			  (emtvf:grade-fmt->plural
			     info))
		       '()))
	       nobj
	       "\n"))
	 (failures
	    (emtvf:map-grades
	       #'(lambda (info count)
		    (if
		       (emtvf:grade-fmt->fail-p info)
		       (hiformat:grammar:num-and-noun
			  count 
			  (emtvf:grade-fmt->singular
			     info)
			  (emtvf:grade-fmt->plural
			     info))
		       '()))
	       nobj
	       "\n")))
      (cond
	 ((null worst)
	    '(w/face ("Nothing was tested" "\n") emtvf:face:dormant))
	 ((emtvf:grade-fmt->fail-p 
	     (emtvf:get-grade-info worst))
	    (list
	       "Problems:"
	       '(sep 5)
	       failures
	       '(sep 4)
	       (if successes 
		  `("Completions:" (sep 5) ,successes)
		  '("Nothing succeeded"))
	       "\n"))
	 (t
	    (list
	       '(w/face "All OK" emtvf:face:ok)
	       '(sep 5)
	       successes
	       '(sep 4))))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emformat.el ends here
