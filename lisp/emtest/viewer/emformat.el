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

(require 'utility/pathtree) ;;The view-node type
(require 'utility/loal)     ;;The data-list type
(require 'viewers/hiformat)
(require 'viewers/loformat)
(require 'emtest/viewer/view-types)
(require 'emtest/common/grade-types)
(require 'custom)
(require 'emtest/viewer/all-note-formatters)

;;;_. Body
;;;_ , Data
;;;_  . emtvf:format-alist
(defconst emtvf:format-alist loformat:default-alist
   "List of formatters that emformat uses.")
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

;;;_ , Lower format functions
;;;_  . emtvf:insert
;;$$MOVE ME maybe - this is the only part that directly deals with
;;loformat.
(defun emtvf:insert (top-node data-list extra-formats)
   "Insert TOP-NODE via loformat"
   
   (let*
      ((tree (emtvf:top top-node data-list)))
      (loformat:insert
	 tree
	 (append
	    extra-formats
	    emtvf:format-alist))))
;;;_ , Helper functions
;;;_  . emtvf:button
(defun emtvf:button (text func &optional extra-props)
   ""
   (let
      ((map
	  (make-sparse-keymap)))
      (define-key map "\r" func)
      (define-key map [mouse-1] func)
      `((w/props
	   ,text
	   (keymap ,map ,@extra-props)))))

;;;_  . emtvf:headline
;;$$OBSOLETE
'
(defun emtvf:headline (depth face headtext)
   "Make a headline of HEADTEXT for DEPTH, using FACE"
   `(
       (sep 3)
       (w/face ,(make-string depth ?*) ,face)
       " " 
       ,headtext
       (sep 2)))

;;;_  . Special variables
(declare (special emtvf:*outline-depth* emtvf:*folded*))
;;;_  . emtvf:outline-item-f
(defun emtvf:outline-item-f (depth face headtext contents &optional fold)
   "Make an outline item of DEPTH."
   `(
       (sep 3)
       (w/face ,(make-string depth ?*) ,face)
       " " 
       ,headtext
       ;;The heading terminator is made part of contents in order to
       ;;accord with outline-cycle's understanding of folded items.
       ,(cond
	   ((null contents) nil)
	   (fold
	      `(overlay (invisible outline) (sep 2) ,contents))
	   (t
	      `((sep 2) ,contents)))
       (sep 2)))

(defmacro emtvf:outline-item (headtext contents &optional face fold)
   "Make an outline item.
HEADTEXT gives the heading and CONTENTS as contents.
FACE is the face to display the heading in.
If FOLD is non-nil, fold that contents."
   (let
      (  (contents-sym  (make-symbol "contents"))
	 (fold-now      (make-symbol "fold-now"))
	 (new-depth     (make-symbol "new-depth")))
      
      `(let*
	  (  (,new-depth (1+ emtvf:*outline-depth*))
	     ;;Don't overlay if this item is already in a folded
	     ;;thing.
	     (,fold-now (and ,fold (not emtvf:*folded*)))
	     (,contents-sym
		(let
		   (  (emtvf:*outline-depth* ,new-depth)
		      (emtvf:*folded* (or emtvf:*folded* ,fold-now)))
		   ,contents)))
	  (emtvf:outline-item-f ,new-depth ,face ,headtext
	     ,contents-sym ,fold-now))))


;;;_  . emtvf:button-to-explore
(defun emtvf:button-to-explore (explorable text)
   "Make a button to explore EXPLORABLE.
Hack: We add a space after the button."
   ;;$$IMPROVE ME - instead of always making a space let's wrap these
   ;;in something that alternates items with separators, a la
   ;;mapconcat or hiformat:map
   (when explorable
      (list
	 (emtvf:button text
	    `(lambda ()
		(interactive)
		(emtl:dispatch-normal
		   ',(emtt:explorable->how-to-run 
		       explorable)
		   ',(emtt:explorable->prestn-path 
			explorable)))
	    '(help-echo "Rerun this test"))
	 " ")))
;;;_  . emtvf:obj-or-string
(defun emtvf:obj-or-string (value)
   "Display VALUE.
If VALUE is a string, display it lerally, otherwise pretty-print it."
   (if
      (stringp value)
      ;;Indent it so it can't affect outline
      ;;structure. 
      `(indent 4 ,value)
      `(object ,value nil)))


;;;_ , Format functions
;;;_  . emtvf:top

(defun emtvf:top (view-node &optional data-list)
   "Make a format form for VIEW-NODE.
VIEW-NODE must be at least an `emtvp:node'.
Must be called in a `emtv2:dynamic:top' context."

   (check-type view-node emtvp:node)
   (emtv2:dynamic:top
      `(
	  (w/face "Emtest results" emtvf:face:title)
	  "\n"
	  ,(emtvf:node view-node data-list))))

;;;_  . emtvf:node
(defun emtvf:node (view-node &optional data-list)
   "Make a format form for VIEW-NODE.
VIEW-NODE must be an `emt:view:presentable'.
Must be called in a `emtv2:dynamic:top' context."

   (check-type view-node emtvp:node)

   (let*
      ((suite view-node)
	 (name
	    (emtvp:node->name view-node))
	 (children
	    (emtvp:node->children view-node))
	 (grades
	    (emt:view:presentable->sum-badnesses suite))
	 (grade-face
	    (emtvf:grade-overall-face grades))
	 (grades-sum
	    (emtvf:sum-badnesses-short grades data-list))
	 (boring-p 
	    (emtvf:grade-boring grades))
	 ;;This gives us the prefix of the headline if we have skipped
	 ;;one or more plys of the tree because they were singletons.
	 (name-prefix
	    (apply #'append
	       (mapcar
		  #'(lambda (x)
		       (list x " "))
		  emtvf:*hdln-path*))))
      
      (etypecase suite
	 (emt:view:suite-newstyle
	    (let*
	       (
		  (object
		     (emt:view:suite-newstyle->result suite))
		  (explorable
		     (emt:view:suite-newstyle->how-to-run suite)))
	       (etypecase object
		  (null "A null viewable")
		  (emt:testral:test-runner-info
		     (emtvf:outline-item
			`(  ,name-prefix
			    (w/face ,name emtvf:face:suitename)
			    " "
			    ,grades-sum)
			(hiformat:map 
			   ;;Formatting for each child
			   #'(lambda (obj data &rest d)
				(emtvf:make-dynamic 
				   obj 
				   #'emtvf:node))
			   
			   children
			   :separator '("\n")) 
			grade-face
			boring-p))
		  
		  
		  (emt:testral:suite
		     (emtvf:outline-item 
			`(  ,name-prefix
			    (w/face ,name emtvf:face:suitename)
			    " "
			    ,(emtvf:button-to-explore explorable "[RUN]")
			    ,grades-sum)
			(etypecase (emt:testral:suite->contents object)
			   (emt:testral:runform-list
			      (hiformat:map 
				 ;;Formatting for each child
				 #'(lambda (obj data &rest d)
				      (emtvf:make-dynamic 
					 obj 
					 #'emtvf:node))
				 children
				 :separator '("\n")
				 :els=0 '("No child suites")))
			   ;;$$IMPROVE ME If there are children, use
			   ;;them instead.  This will work the same as
			   ;;for `emt:testral:runform-list'.
			   (emt:testral:note-list
			      (hiformat:map
				 #'emtvf:TESTRAL
				 (emt:testral:note-list->notes
				    (emt:testral:suite->contents object))
				 :separator '("\n")
				 :els=0 '("No notes")))
			   (null
			      '("No known contents"))) 
			grade-face
			boring-p)))))
	 
	 (emt:view:TESTRAL
	    (emtvf:TESTRAL view-node data-list))

	 ;;Base type, appears for the root node.
	 (emt:view:presentable
	    (if
	       (and
		  (= (length children) 1))
	       ;;Shortcut any singletons.
	       (let
		  ((emtvf:*hdln-path* (list name)))
		  (emtvf:make-dynamic 
		     (car children)
		     #'emtvf:node))
	       
	       (let
		  ((emtvf:*hdln-path* '()))

		  (emtvf:outline-item 
		     `(  ,name-prefix
			 (w/face ,name emtvf:face:suitename)
			 " "
			 ,grades-sum)
		     (hiformat:map 
			;;Formatting for each child
			#'(lambda (obj data &rest d)
			     (emtvf:make-dynamic
				obj 
				#'emtvf:node))
			children
			:separator '("\n")) 
		     grade-face)))))))


;;;_  . emtvf:TESTRAL (TESTRAL note formatter)
(defun emtvf:TESTRAL (obj &optional data &rest d)
   "Make a format form for OBJ.
OBJ must be a TESTRAL note."
   (declare (special data-list))
   (let*
      ()
      (list
	 ;;$$OBSOLETE
	 (apply #'append
	    (mapcar
	       #'(lambda (x)
		    (list x " "))
	       (emt:testral:base->prestn-path obj)))
	 (etypecase obj
	    ;;This is the only one that will actually carry over in the
	    ;;long term, the others are actually obsolescent.
	    (emt:testral:newstyle
	       (apply 
		  (emtvf:get-TESTRAL-formatter 
		     (emt:testral:newstyle->governor obj))
		  obj
		  (emt:testral:newstyle->value obj)))))))

;;;_  . emtvf:grade-boring
(defun emtvf:grade-boring (obj)
   "Return non-nil if OBJ is all passing grades.
OBJ must be a `emt:testral:grade:summary'"
   ;;$$REDESIGN ME  Complete hack here.
   (eq (emtvf:grade-overall-face obj) 'emtvf:face:ok))

;;;_  . emtvf:grade-overall-face
(defun emtvf:grade-overall-face (obj)
   ""
   
   (let*
      (
	 (obj (emtvr:grade->summary obj))
	 (test-cases (emt:testral:grade:summary->test-cases obj))
	 (fails      (emt:testral:grade:summary->fails      obj))
	 (ungradeds  (emt:testral:grade:summary->ungradeds  obj))
	 (dormants   (emt:testral:grade:summary->dormants   obj))
	 (blowouts   (emt:testral:grade:summary->blowouts   obj)))
      (cond
	 ((> blowouts   0) 'emtvf:face:blowout)
	 ((> ungradeds  0) 'emtvf:face:ungraded)
	 ((> fails      0) 'emtvf:face:failed)
	 ((> dormants   0) 'emtvf:face:dormant)
	 ((> test-cases 0) 'emtvf:face:ok)
	 (t                'emtvf:face:dormant))))

;;;_  . emtvf:sum-badnesses-short
(defun emtvf:sum-badnesses-short (obj &optional data &rest d)
   "Give a summary of grades for this object."
   (let*
      (
	 (obj (emtvr:grade->summary obj))
	 (test-cases (emt:testral:grade:summary->test-cases obj))
	 (fails      (emt:testral:grade:summary->fails      obj))
	 (ungradeds  (emt:testral:grade:summary->ungradeds  obj))
	 (dormants   (emt:testral:grade:summary->dormants   obj))
	 (blowouts   (emt:testral:grade:summary->blowouts   obj)))
      (if
	 (and
	    (= fails     0)
	    (= ungradeds 0)
	    (= dormants  0)
	    (= blowouts  0))
	 (if (> test-cases 0)
	    (list
	       '(w/face "All OK" emtvf:face:ok)
	       " ("
	       (hiformat:grammar:num-and-noun
		  test-cases "case" "cases")
	       ")")
	    '(w/face "Nothing was tested" emtvf:face:dormant))
	 (list
	    '(w/face "Problems: " emtvf:face:failed)
	    (hiformat:map 
	       #'(lambda (obj &rest r) obj)
	       (delq nil
		  (mapcar
		     #'(lambda (data)
			  (destructuring-bind (n text face) data
			     (when (> n  0) 
				`(w/face ,text ,face))))
		     (list
			(list blowouts  "Blowouts"	 'emtvf:face:blowout)
			(list ungradeds "Ungraded tests" 'emtvf:face:ungraded)
			(list fails     "Failures" 	 'emtvf:face:failed)
			(list dormants  "Dormant tests"  'emtvf:face:dormant))))
	       
	       :separator '(", "))
	    "."))))


;;;_  . emtvf:sum-badnesses-long
(defun emtvf:sum-badnesses-long (obj &optional data &rest d)
   "Give a summary of grades for this object."
   (let*
      (
	 (obj (emtvr:grade->summary obj))
	 (test-cases (emt:testral:grade:summary->test-cases obj))
	 (fails      (emt:testral:grade:summary->fails      obj))
	 (ungradeds  (emt:testral:grade:summary->ungradeds  obj))
	 (dormants   (emt:testral:grade:summary->dormants   obj))
	 (blowouts   (emt:testral:grade:summary->blowouts   obj)))
      (if
	 (and
	    (= fails     0)
	    (= ungradeds 0)
	    (= dormants  0)
	    (= blowouts  0))
	 (if (> test-cases 0)
	    (list
	       "All OK ("
	       (prin1-to-string test-cases)
	       " "
	       (hiformat:grammar:number-agreement 
		  test-cases "case" "cases")
	       ")" "\n")
	    (list "Nothing was tested" "\n"))
	 (list
	    "Problems: \n"
	    (hiformat:map 
	       #'(lambda (obj &rest r)
		    obj)
	       (delq nil
		  (list
		     (when (> blowouts  0) 
			(hiformat:grammar:num-and-noun 
			   blowouts
			   "Blowout" "Blowouts"))
		     (when (> ungradeds 0) 
			(hiformat:grammar:num-and-noun 
			   ungradeds
			   "Ungraded test" "Ungraded tests"))
		     (when (> fails     0) 
			(hiformat:grammar:num-and-noun 
			   fails
			   "Failure" "Failures"))
		     (when (> dormants  0) 
			(hiformat:grammar:num-and-noun 
			   dormants
			   "Dormant test" "Dormant tests"))))
	       :separator '(".\n"))
	    "\n"
	    (if (> test-cases 0)
	       (list
		  (prin1-to-string test-cases)
		  " successful "
		  (hiformat:grammar:number-agreement 
		     test-cases "test case" "test cases"))
	       (list "No test cases succeeded"))
	    "\n"))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emformat.el ends here
