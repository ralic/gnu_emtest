;;;_ emtest/common/testral-types.el --- TESTRAL types for Emtest

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


;;;_ , Requires

(eval-when-compile
   (require 'cl))
(require 'emtest/common/grade-types)
(require 'emtest/common/prestn-path)

;;;_. Body

;;;_ , TESTRAL types
;;;_  . Base class
(defstruct (emt:testral:note
	      (:constructor emt:testral:make-note)
	      (:conc-name emt:testral:note->))
   "The TESTRAL base type"
   
   (id        () :type emt:testral:id-element)
   (parent-id () 
      :type emt:testral:id-element
      :doc "ID of the parent note. `nil' for notes that have no parent.")

   (prestn-path () 
      :type emt:testral:prestn-path
      :doc "Extra presentation path for this note.  Used to report
   some deeply-nested checks such as type checking.")

   ;;Reflects only the note's's intrinsic problems.  Even stages need
   ;;this field in case (say) a whole stage is dormantized or aborted.
   (grade () :type emt:testral:grade-aux)
   (relation () :type symbol
      :doc "The note's relation to its parent")
   (governor () :type symbol
      :doc "A symbol that indicates the note's meaning and its formatting.")
   value)

;;;_  . Contents discrimination for suite type

;;;_   , emt:testral:runform-list
(defstruct (emt:testral:runform-list
	    (:constructor emt:testral:make-runform-list)
	    (:conc-name emt:testral:runform-list->))
  
  "List of explorables"
  (els () :type (repeat emtt:explorable)))
;;;_   , emt:testral:note-list
(defstruct (emt:testral:note-list
	    (:constructor emt:testral:make-note-list)
	    (:conc-name emt:testral:note-list->))
  ""
  (notes () :type (repeat emt:testral:note)))

;;;_  . TESTRAL general report

(defstruct (emt:testral:report
	    (:constructor emt:testral:make-report)
	    (:conc-name emt:testral:report->))
  "A report sent by test-runner to viewer"
  (testrun-id     () :type emt:testral:testrun-id)
  (newly-pending  () :type integer)
  (test-id-prefix () :type emt:testral:prestn-path)
  (suites () :type 
	  (repeat
	   ;;$$IMPROVE ME Move emtt:explorable into a common ancestor
	     ;;of emt:testral:test-runner-info and emt:testral:suite.
	     ;;Adjust `emtvr:one-newstyle' accordingly.
	   (list 
	    emtt:explorable
	    null ;;let's leave that an empty list for now
	    (or emt:testral:suite emt:testral:test-runner-info)))))

;;;_  . test-runner info
(defstruct (emt:testral:test-runner-info
	    (:constructor emt:testral:make-test-runner-info)
	    (:conc-name emt:testral:test-runner-info->))
  "Info describing a tester."
  (name    ()  :type string)
  (version "0" :type string)
  ;;See [[id:b4sjlt20mze0][Test-runner info]]
  (explore-methods-supported () :type (repeat emtt:method)))


;;;_  . suite info
(defstruct (emt:testral:suite
	    (:constructor emt:testral:make-suite)
	    (:conc-name emt:testral:suite->))
  ""
  (contents () 
	    :type 
	    (or 
	     emt:testral:note-list
	     emt:testral:runform-list
	     null)) 
  (grade () :type emt:testral:grade-aux)
   ;;$$OBSOLESCENT
  info)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/testral-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/testral-types.el ends here
