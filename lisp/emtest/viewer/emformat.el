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
(defface emt:view:face:ok 
   '((default :foreground "green3" :weight bold))
   "Face for reporting passed tests"
   :group 'emtest)

(defface emt:view:face:failed 
   '((default :foreground "red" :weight bold))
   "Face for reporting failed tests"
   :group 'emtest)

(defface emt:view:face:ungraded
   '((default :foreground "red" :strike-through t))
   "Face for reporting ungraded tests"
   :group 'emtest)

(defface emt:view:face:blowout 
   '((default :foreground "black" :background "red" :weight bold))
   "Face for reporting blown-out tests"
   :group 'emtest)

(defface emt:view:face:dormant
   '((default :foreground "black"))
   "Face for reporting dormant tests"
   :group 'emtest)

;;;_   , Comparisons
(defface emt:view:face:mismatch
   '((default :foreground "pink" :weight bold))
   "Face for reporting mismatches.
NB, this is not a grade.  It indicates failure of a comparison,
which may not imply failure of an assertion."
   :group 'emtest)

(defface emt:view:face:ok-match
   '((default :foreground "green4" :weight bold))
   "Face for reporting correct matches.
NB, this is not a grade.  It indicates a successful comparison,
which may not imply success of an assertion."
   :group 'emtest)

;;;_   , Pieces
(defface emt:view:face:title
   '((default 
	:height 1.8
	:foreground "black"))
   
   "Face for displaying Emtest banner"
   :group 'emtest)
(defface emt:view:face:suitename
   '((default 
	:foreground "blue1"))
   
   "Face for displaying test names"
   :group 'emtest)
;;;_  . Configuration
;;;_   , emt:fmt:dynvars
(defconst emt:fmt:dynvars 
   (append
      '((emt:fmt:*hdln-path*))
      emt:fmt:outline:dynvars)
   "Dynamic variables that emformat uses" )

;;;_ , Lower format functions
;;;_  . emt:fmt:make-dynamic
(defun emt:fmt:make-dynamic (obj func)
   "Make a form that calls a dynamic object"
   `(dynamic 
       ,obj 
       ,func
       ,(utidyv:capture-vars emt:fmt:dynvars)))
;;;_  . emt:fmt:sym->suitename
(defun emt:fmt:sym->suitename (sym)
   "Make a form for a suitename given as a symbol"
   `(w/face 
       ,(etypecase sym
	   (symbol (symbol-name sym))
	   (string sym))
       emt:view:face:suitename))

;;;_ , Helper functions
;;;_  . Characterizing object representation
;;;_   , emt:fmt:short-obj-p 
(defun emt:fmt:short-obj-p (obj)
   "Return t is object's printed representation is fairly short"
   (or
      (symbolp obj)
      (and
	 (stringp obj)
	 (< (length obj) 40))))

;;;_  . Singles-Path
;;;_   , Special variables
(declare (special emt:fmt:*hdln-path*))

;;;_   , emt:fmt:with-blank-singles-path
(defmacro emt:fmt:with-blank-singles-path (&rest body)
   "Eval BODY in a blank singles-path."
   
   `(let ((emt:fmt:*hdln-path* '()))
       ,@body))
;;;_   , emt:fmt:with-more-singles-path
(defmacro emt:fmt:with-more-singles-path (name &rest body)
   ""
   
   `(let ((emt:fmt:*hdln-path* (cons name emt:fmt:*hdln-path*)))
       ,@body))

;;;_   , emt:fmt:singles-path
(defun emt:fmt:singles-path ()
   ""
   (apply #'nconc
      (mapcar
	 #'(lambda (x)
	      (list x " "))
	 (nreverse (remq nil emt:fmt:*hdln-path*)))))
;;;_  . Buttons
;;;_   , emt:fmt:button-explore-func
(defun emt:fmt:button-explore-func (button)
   "Explore explorable, given BUTTON."
   ;;$$IMPROVE ME Distinguish proplists we want to use.
   (let
      ((viewable
	  (button-get button 'viewable)))
      ;; Set the mark on viewable because user probably wants to see
      ;; the results.
      (if (emt:view:suite-p viewable)
	 (setf (emt:view:suite->mark viewable) t))
      ;; Run it.
      (emt:lch:run
	 ;; emt:lch:run wants just the contents, not the whole object.
	 (emt:run:how->contents
	    (button-get button 'how-to-run))
	 emt:lch:proplist:vanilla
	 (button-get button 'prestn-path))))

;;;_   , emt:fmt:button-to-explore
(defun emt:fmt:button-to-explore (viewable explorable text)
   "Make a button to explore EXPLORABLE."
   (when explorable
      `(button ,text 
	  action ,#'emt:fmt:button-explore-func
	  help-echo "Rerun this test"
	  viewable ,viewable
	  how-to-run  ,(emt:run:explorable->how-to-run  explorable)
	  prestn-path ,(emt:run:explorable->prestn-path explorable))))
;;;_   , emt:fmt:viewable->mark-text
(defun emt:fmt:viewable->mark-text (viewable)
   "Return the text of VIEWABLE's current mark."
   (if (emt:view:suite->mark viewable) "X" "_"))

;;;_   , emt:fmt:reprint-button
;;$$IMPROVE ME  Make this work for buttons in general, not just
;;(un)mark on viewables
(defun emt:fmt:reprint-button (button)
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
	    (emt:fmt:button-toggle-mark viewable)
	    emt:vw:top:format-alist))))

;;;_   , emt:fmt:button-toggle-mark-func
(defun emt:fmt:button-toggle-mark-func (button)
   "Toggle the mark on viewable given on BUTTON."
   (let
      ((viewable (button-get button 'viewable)))
      (setf
	 (emt:view:suite->mark viewable)
	 (not (emt:view:suite->mark viewable)))
      (emt:ind:set-prop
	 (emt:run:how->contents
	    (emt:run:explorable->how-to-run
	       (emt:view:suite->explorable viewable)))
	 'user-says-rerun
	 t)
      (emt:fmt:reprint-button button)))

;;;_   , emt:fmt:button-toggle-mark
(defun emt:fmt:button-toggle-mark (viewable)
   "Make a button to toggle the mark on VIEWABLE."
   (when viewable
      `(button ,(emt:fmt:viewable->mark-text viewable) 
	  action ,#'emt:fmt:button-toggle-mark-func
	  help-echo "Mark this test-suite"
	  viewable ,viewable)))

;;;_  . Objects
;;;_   , emt:fmt:obj-or-string
(defun emt:fmt:obj-or-string (value)
   "Display VALUE.
If VALUE is a string, display it literally, otherwise pretty-print it."
   (if
      (stringp value)
      ;;Indent it so it can't affect outline
      ;;structure. 
      `(indent 4 ,value)
      `(object ,value nil)))
;;;_  . Direct emformat support
;;;_   , emt:fmt:outline:item-emformat
(defmacro emt:fmt:outline:item-emformat (headtext contents &optional face fold)
   ""
   
   `(emt:fmt:outline:item
       (list (emt:fmt:singles-path) ,headtext)
       (emt:fmt:with-blank-singles-path ,contents)
       ,face
       ,fold))

;;;_   , emt:fmt:mapnodes 
(defun emt:fmt:mapnodes (list els=0)
   "Map emt:fmt:node over LIST, making dynamic entries"
   (hiformat:map 
      #'(lambda (obj &rest d)
	   (emt:fmt:make-dynamic 
	      obj 
	      #'emt:fmt:node))
      list
      :separator "\n"
      :els=0 els=0))

;;;_  . emt:fmt:shortcut-single
(defmacro emt:fmt:shortcut-single (name children rest-headline face
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
	     (emt:fmt:with-more-singles-path ,name-sym
		(emt:fmt:make-dynamic 
		   (car ,children-sym)
		   #'emt:fmt:node))
	     (emt:fmt:outline:item-emformat
		(list ,name-sym ,rest-headline)
		(emt:fmt:mapnodes ,children-sym ,format-no-child)
		,face
		,fold)))))

;;;_ , Format functions
;;;_  . emt:fmt:top

(defun emt:fmt:top (view-node)
   "Make a format form for VIEW-NODE.
VIEW-NODE must be at least an `pathtree:node'."

   (check-type view-node pathtree:node)
   (utidyv:top 
      emt:fmt:dynvars
      `(
	  (w/face "Emtest results" emt:view:face:title)
	  "\n"
	  ,(emt:fmt:sum-grades-long (emt:view:presentable->sum-grades view-node))
	  ,(emt:fmt:node view-node))))

;;;_  . emt:fmt:node
(defun emt:fmt:node (view-node)
   "Make a format form for VIEW-NODE.
VIEW-NODE must be an `emt:view:presentable'.
Must be called in a `utidyv:top' context."

   (check-type view-node pathtree:node)

   (let*
      ((suite view-node)
	 (name
	    (pathtree:node->name view-node))
	 (children
	    (pathtree:node->children view-node))
	 (grades
	    (emt:view:presentable->sum-grades suite))
	 (grade-face
	    (emt:fmt:grade-overall-face grades))
	 (grades-sum
	    (emt:fmt:sum-grades-short grades))
	 (boring-p 
	    (emt:fmt:grade-boring grades)))
      
      (etypecase suite
	 (emt:view:suite
	    (let*
	       (
		  (object
		     (emt:view:suite->result suite))
		  (explorable
		     (emt:view:suite->explorable suite)))
	       
	       (etypecase object
		  (null "A null viewable")
		  (emt:testral:suite
		     (emt:fmt:outline:item-emformat
			(hiformat:separate
			   (delq nil
			      (list
				 (emt:fmt:button-toggle-mark view-node)
				 (emt:fmt:sym->suitename name)
				 (emt:fmt:button-to-explore 
				    view-node explorable "[RUN]")
				 grades-sum))
			   " ")
			(emt:fmt:mapnodes children "No child suites")
			grade-face
			boring-p)))))
	 (emt:view:explorable
	    (emt:vw:explorable view-node name))
	 
	 (emt:view:note
	    (emt:vw:note view-node))

	 (emt:view:note-placeholder
	    (emt:fmt:shortcut-single
	       nil
	       (pathtree:node->children view-node)
	       '()
	       nil
	       "[Note placeholder with no children]"))
	 
	 ;;Base type, appears for the root node.
	 (emt:view:presentable
	    (emt:fmt:shortcut-single 
	       nil
	       (pathtree:node->children view-node)
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
	    (emt:vw:note:get-formatter 
	       (emt:testral:note->governor note))
	    obj
	    (emt:testral:note->value note)))
      (error
	 `((w/face "Error in formatter: " emt:view:face:blowout) 
	     (object ,err nil)
	     "\n"))))
;;;_  . emt:vw:explorable
(defun emt:vw:explorable (view-node name)
   "Make a format form for a emt:view:explorable, which encases a emt:run:explorable."
   (emt:fmt:outline:item-emformat
      (list 
	 (emt:fmt:sym->suitename name)
	 " "
	 (emt:fmt:button-to-explore 
	    view-node 
	    (emt:view:explorable->contents view-node)
	    "[RUN]"))
      nil
      'emt:view:face:dormant))

;;;_ , About grades
;;;_  . Structure emt:fmt:grade-fmt
(defstruct (emt:fmt:grade-fmt
	      (:type list)
	      (:constructor emt:fmt:make-grade-fmt)
	      (:copier nil)
	      (:conc-name emt:fmt:grade-fmt->))
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
;;;_   , emt:fmt:grade-fmt-default
(defconst emt:fmt:grade-fmt-default 
   (emt:fmt:make-grade-fmt
      :symbol nil
      :fail-p nil
      :face   'emt:view:face:dormant
      :plural   "(UNUSED: No tests)"
      :singular "(UNUSED: No tests)"
      :severity 0)
   "The default grade formatting info" )
;;;_   , emt:fmt:grade-fmt-alist
(defconst emt:fmt:grade-fmt-alist 
   (list
      (emt:fmt:make-grade-fmt
	 :symbol 'blowout
	 :fail-p t
	 :face   'emt:view:face:blowout
	 :plural   "Blowouts"
	 :singular "Blowout"
	 :severity 100
	 )
      (emt:fmt:make-grade-fmt
	 :symbol 'ungraded
	 :fail-p t
	 :face   'emt:view:face:ungraded
	 :plural   "Ungraded tests"
	 :singular "Ungraded test"
	 :severity 75
	 )
      (emt:fmt:make-grade-fmt
	 :symbol 'fail
	 :fail-p t
	 :face   'emt:view:face:failed
	 :plural   "Failures"
	 :singular "Failure"
	 :severity 50
	 )
      (emt:fmt:make-grade-fmt
	 :symbol 'dormant
	 :fail-p t
	 :face   'emt:view:face:dormant
	 :plural   "Dormant tests"
	 :singular "Dormant test"
	 :severity 25
	 )
      ;;$$IMPROVE ME  Encap making a passing grade type, omitting
      ;;redundant info from arglist
      (emt:fmt:make-grade-fmt
	 :symbol 'ok
	 :fail-p nil
	 :face   'emt:view:face:ok
	 :plural   "(UNUSED: ok)"
	 :singular "(UNUSED: ok)"
	 :severity 10
	 )
      (emt:fmt:make-grade-fmt
	 :symbol 'test-case
	 :fail-p nil
	 :face   'emt:view:face:ok
	 :plural   "Test cases"
	 :singular "Test case"
	 :severity 10)
      emt:fmt:grade-fmt-default)
   
   "Alist of grade formatting info" )
;;;_   , emt:fmt:get-grade-info
(defun emt:fmt:get-grade-info (sym)
   "Return summary & formatting info about SYM.
SYM should be a grade symbol, but this returns a valid object in any case."
   (or 
      (assq sym emt:fmt:grade-fmt-alist)
      emt:fmt:grade-fmt-default))
;;;_  . Grade helpers
;;;_   , emt:fmt:grade-boring
(defun emt:fmt:grade-boring (obj)
   "Return non-nil if OBJ is all passing grades.
OBJ must be a `emt:view:grade-summary'"
   (not
      (emt:view:grade-summary->alert (emt:grd:->grade-summary obj))))

;;;_   , emt:fmt:grade-overall-face
(defun emt:fmt:grade-overall-face (obj)
   "Return a face that hints at the overall quality of grades in OBJ.
OBJ should be an `emt:view:grade-summary'."

   (let*
      ((nobj (emt:grd:->grade-summary obj))
	 (worst (emt:view:grade-summary->worst nobj))
	 (info (emt:fmt:get-grade-info worst)))
      (emt:fmt:grade-fmt->face info)))

;;;_   , emt:fmt:map-grades
(defun emt:fmt:map-grades (func nobj &optional separator)
   "Map FUNC over the grades seen in NOBJ.
Any nil items are omitted, which makes a difference in separation.
SEPARATOR, if non-nil, is what separates the items."
   (hiformat:separate
      (delq nil
	 (mapcar
	    #'(lambda (grade)
		 (funcall func 
		    (emt:fmt:get-grade-info (first grade))
		    (second grade)))
	    (emt:view:grade-summary->grades nobj)))
      separator))
;;;_  . Grade formatters
;;;_   , emt:fmt:sum-grades-short
(defun emt:fmt:sum-grades-short (obj &rest d)
   "Give a summary of grades for this object."
   (let*
      (
	 (nobj (emt:grd:->grade-summary obj))
	 (worst (emt:view:grade-summary->worst nobj)))
      
      (cond
	 ((null worst)
	    '(w/face "Nothing was tested" emt:view:face:dormant))
	 ((emt:fmt:grade-fmt->fail-p 
	     (emt:fmt:get-grade-info worst))
	    (list
	       '(w/face "Problems: " emt:view:face:failed)
	       (emt:fmt:map-grades
		  #'(lambda (info count)
		       (if
			  (emt:fmt:grade-fmt->fail-p info)
			  `(w/face 
			      ,(emt:fmt:grade-fmt->plural info)
			      ,(emt:fmt:grade-fmt->face   info))
			  '()))
		  nobj
		  ", ")
	       "."))
	 (t
	    '(w/face "All OK" emt:view:face:ok)))))


;;;_   , emt:fmt:sum-grades-long
(defun emt:fmt:sum-grades-long (obj &rest d)
   "Give a summary of grades for this object."
   (let*
      (
	 (nobj  (emt:grd:->grade-summary obj))
	 (worst (emt:view:grade-summary->worst nobj))
	 (successes
	    (emt:fmt:map-grades
	       #'(lambda (info count)
		    (if
		       (not (emt:fmt:grade-fmt->fail-p info))
		       (hiformat:grammar:num-and-noun
			  count 
			  (emt:fmt:grade-fmt->singular
			     info)
			  (emt:fmt:grade-fmt->plural
			     info))
		       '()))
	       nobj
	       "\n"))
	 (failures
	    (emt:fmt:map-grades
	       #'(lambda (info count)
		    (if
		       (emt:fmt:grade-fmt->fail-p info)
		       (hiformat:grammar:num-and-noun
			  count 
			  (emt:fmt:grade-fmt->singular
			     info)
			  (emt:fmt:grade-fmt->plural
			     info))
		       '()))
	       nobj
	       "\n")))
      (cond
	 ((null worst)
	    '(w/face ("Nothing was tested" "\n") emt:view:face:dormant))
	 ((emt:fmt:grade-fmt->fail-p 
	     (emt:fmt:get-grade-info worst))
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
	       '(w/face "All OK" emt:view:face:ok)
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
