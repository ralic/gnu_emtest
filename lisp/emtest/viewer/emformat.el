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
;;;_  . Special variables
(declare (special emtvf:*outline-depth* emtvf:*fold*))
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
;;;_  . emtvf:headline
(defun emtvf:headline (depth face headtext)
   "Make a headline of HEADTEXT for DEPTH, using FACE"
   
   `(
       (sep 3)
       (w/face ,(make-string depth ?*) ,face)
       " " 
       ,headtext
       (sep 2)))

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

;;;_  . emtvf:outline-item
;;$$IMPROVE ME Make this a macro so it controls outline-depth itself.

;;If folded, properties ('invisible 'outline)
(defun emtvf:outline-item (depth face headtext contents &optional fold)
   "Make an outline item of DEPTH."
   `(
       ,(emtvf:headline depth face headtext)
       ,(if fold
	   `(w/props ,contents (invisible outline))
	   contents)
       ,(if contents '(sep 2))))
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
   ""
   (if
      (stringp value)
      ;;Indent it so it can't affect outline
      ;;structure. 
      `(indent 4 ,value)
      `(object ,value nil)))

;;;_ , Format functions
;;;_  . emtvf:top

(defun emtvf:top (view-node data-list)
   "Make a format form for VIEW-NODE.
VIEW-NODE must be at least an `emtvp:node'.
DATA-LIST must be a loal (list of alists)."

   (check-type view-node emtvp:node)
   `(
       (w/face "Emtest results" emtvf:face:title)
       "\n"
       ,(emtvf:node view-node data-list)))

;;;_  . emtvf:node
(defun emtvf:node (view-node data-list)
   "Make a format form for VIEW-NODE.
VIEW-NODE must be an `emt:view:presentable'.
DATA-LIST must be a loal."

   (check-type view-node emtvp:node)

   (let*
      ((suite view-node)
	 (name
	    (emtvp:node->name view-node))
	 (children
	    (emtvp:node->children view-node))
	 (depth
	    (loal:val 'depth data-list 0))
	 (grades
	    (emt:view:presentable->sum-badnesses suite))
	 (grade-face
	    (emtvf:grade-overall-face grades))
	 (grades-sum
	    (emtvf:sum-badnesses-short grades data-list))
	 ;;This gives us the prefix of the headline if we have skipped
	 ;;one or more plys of the tree because they were singletons.
	 (name-prefix
	    (apply #'append
	       (mapcar
		  #'(lambda (x)
		       (list x " "))
		  (loal:val 'hdln-path data-list '())))))
      
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
		     (list
			;;"Suites tested in " name "\n"
			(emtvf:headline 
			   (1+ depth) 
			   grade-face 
			   `(  ,name-prefix
			       (w/face ,name emtvf:face:suitename)
			       " "
			       ,grades-sum))
			(hiformat:map 
			   ;;Formatting for each child
			   #'(lambda (obj data &rest d)
				(list
				   `(dynamic ,obj 
				       ,(loal:acons 'depth (1+ depth) data)
				       ,#'emtvf:node)))
			   children
			   :data-loal data-list
			   :separator '("\n"))))
		  
		  (emt:testral:suite
		     (list
			(emtvf:headline 
			   (1+ depth) 
			   grade-face 
			   `(  ,name-prefix
			       (w/face ,name emtvf:face:suitename)
			       " "
			       ,(emtvf:button-to-explore explorable "[RUN]")
			       ,grades-sum))
			(etypecase (emt:testral:suite->contents object)
			   (emt:testral:runform-list
			      (hiformat:map 
				 ;;Formatting for each child
				 #'(lambda (obj data &rest d)
				      (list
					 `(dynamic ,obj 
					     ,(loal:acons 
						 'depth (1+ depth) data)
					     ,#'emtvf:node)))
				 children
				 :data-loal data-list
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
				 :data-loal data-list
				 :separator '("\n")
				 :els=0 '("No notes")))
			   (null
			      '("No known contents")))

			)))))

	 (emt:view:TESTRAL
	    (emtvf:TESTRAL view-node data-list))

	 ;;Base type, appears for the root node.
	 (emt:view:presentable
	    (if
	       (and
		  (= (length children) 1))
	       ;;Shortcut any singletons.
	       (list
		  `(dynamic ,(car children)
		      ;;$$IMPROVE ME Get value from old hdln-path
		      ;;(loal:update 'hdln-path FUNC-WRITE-ME data-list '())
		      ,(loal:acons 'hdln-path (list name) data-list)
		      ,#'emtvf:node))
	       (let
		  ((ch-data-list
		      (loal:acons 'hdln-path '() data-list)))
		  (list
		     (emtvf:headline 
			(1+ depth) 
			grade-face 
			`(  ,name-prefix
			    (w/face ,name emtvf:face:suitename)
			    " "
			    ,grades-sum))
		     "\n"
		     (hiformat:map 
			;;Formatting for each child
			#'(lambda (obj data &rest d)
			     (list
				`(dynamic ,obj 
				    ,(loal:acons 'depth (1+ depth) data)
				    ,#'emtvf:node)))
			children
			:separator '("\n")
			:data-loal data-list))))))))

;;;_  . emtvf:TESTRAL (TESTRAL note formatter)
(defun emtvf:TESTRAL (obj data &rest d)
   "Make a format form for OBJ.
OBJ must be a TESTRAL note."
   (declare (special data-list))
   (let*
      ((depth
	  ;;$$FIX ME We are not getting the incremented depth here.
	  ;;For now we add 1 but that's just a hack.
	  (1+ (loal:val 'depth data-list 0))))
      (list
	 (apply #'append
	    (mapcar
	       #'(lambda (x)
		    (list x " "))
	       (emt:testral:base->prestn-path obj)))
	 (etypecase obj
	    ;;This is the only one that will actually carry over in the
	    ;;long term, the others are actually obsolescent.
	    (emt:testral:newstyle
	       (let
		  ((gov-symbol (emt:testral:newstyle->governor obj)))
		  (apply 
		     (emtvf:get-TESTRAL-formatter 
			gov-symbol)
		     obj
		     (emt:testral:newstyle->value obj))))
	    
	    
	    (emt:testral:alone
	       (typecase obj
		  (emt:testral:error-raised
		     (error "Obsolete emt:testral:error-raised")
		     (emtvf:outline-item
			(1+ depth) 
			'emtvf:face:ungraded
			"OBSOLETE Error raised: "
			`(object ,(emt:testral:error-raised->err obj) nil)))
		  (emt:testral:doc
		     (error "Obsolete emt:testral:doc")
		     (let
			((doc (emt:testral:doc->str obj)))
			(cond
			   ((not (string-match "\n" doc))
			      (emtvf:outline-item
				 (1+ depth) nil doc nil))
			   ((string-match ": " doc)
			      (emtvf:outline-item
				 (1+ depth) 
				 nil
				 (substring doc 0 (match-end 0))
				 (substring doc (match-end 0))))
			   (t
			      (emtvf:outline-item
				 (1+ depth) nil "Doc" doc)))))
		  
		  (emt:testral:not-in-db
		     (error "Obsolete emt:testral:not-in-db")
		     (let
			((value (emt:testral:not-in-db->value obj)))
		     (emtvf:outline-item
			(1+ depth)
			'emtvf:face:ungraded
			"ID not in database "
			`(
			    "Obsolete emt:testral:not-in-db\n"
			   ,(emtvf:headline 
			       (+ 2 depth)
			       nil
			       (list
				  "Value "
				  (emtvf:button "[Accept]"
				     `(lambda ()
					 (interactive)
					 (emdb:set-value
					    ',(emt:testral:not-in-db->backend
					    obj)
					    ',(emt:testral:not-in-db->id-in-db
					    obj)
					    ',value
					    'correct-answer))
				     '(help-echo "Accept this value"))))
			    ,(if
				(stringp value)
				;;Indent it so it can't affect outline
				;;structure. 
				`(indent 4 ,value)
				`(object ,value nil))))))
		  (t 
		     (emtvf:outline-item (1+ depth)
			nil
			"An unhandled TESTRAL note"
			nil))))))))

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
(defun emtvf:sum-badnesses-short (obj data &rest d)
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
(defun emtvf:sum-badnesses-long (obj data &rest d)
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
