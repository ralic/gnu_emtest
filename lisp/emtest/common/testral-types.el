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

;;;_. Body

;;;_  . TESTRAL types
;;;_   , Base class

(defstruct (emt:testral:base
	      (:constructor emt:testral:make-base)
	      (:conc-name emt:testral:base->))
   "The TESTRAL base type"
   ;;$$OBSOLESCENT
   id	    ;;Shared just between scope co-ordinating notes
   ;;$$OBSOLESCENT
   ;;Not clear that parent-id is of general use.
   parent-id ;;`nil' for root events that have no parent.

   info
   (prestn-path () 
      :type emt:testral:partial-suite-id
      :doc "The presentation path of this note")
   ;;Reflects only the note's's intrinsic problems.  Even push/pops
   ;;need it in case (say) a whole stage is dormantized or aborted.
   (badnesses () :type (repeat emt:result-badness)))

;;;_   , Basic notes 
;;(All inherit from the base class, none have data)
;;;_    . Alone
(defstruct (emt:testral:alone
	    (:constructor emt:testral:make-alone)
	    (:conc-name emt:testral:alone->)
	      (:include emt:testral:base))
   ""
   )
;;;_    . Push
(defstruct (emt:testral:push
	    (:constructor emt:testral:make-push)
	    (:conc-name emt:testral:push->)
	      (:include emt:testral:base))
   ""
   fenceposting ;;
   )
;;;_    . Pop
(defstruct (emt:testral:pop
	    (:constructor emt:testral:make-pop)
	    (:conc-name emt:testral:pop->)
	      (:include emt:testral:base))
   ""
   )
;;;_    . Separate
(defstruct (emt:testral:separate
	    (:constructor emt:testral:make-separate)
	    (:conc-name emt:testral:separate->)
	      (:include emt:testral:base))
   ""
   )

;;;_   , Specific ones
;;;_    . Doc
(defstruct (emt:testral:doc
	    (:constructor emt:testral:make-doc)
	    (:conc-name emt:testral:doc->)
	      (:include emt:testral:alone))
   "A note indicating a docstring"
   (str () :type string))

;;;_    . Check
(defstruct (emt:testral:check:push
	    (:constructor emt:testral:make-check:push)
	    (:conc-name emt:testral:check:push->)
	      (:include emt:testral:push))
   ""
   )
(defstruct (emt:testral:check:pop
	    (:constructor emt:testral:make-check:pop)
	    (:conc-name emt:testral:check:pop->)
	      (:include emt:testral:pop))
   ""
   )
;;;_    . Stage
(defstruct (emt:testral:stage:push
	    (:constructor emt:testral:make-stage:push)
	    (:conc-name emt:testral:stage:push->)
	      (:include emt:testral:push))
   ""
   (name () :type string))

(defstruct (emt:testral:stage:pop
	    (:constructor emt:testral:make-stage:pop)
	    (:conc-name emt:testral:stage:pop->)
	      (:include emt:testral:pop))
   ""
   )

;;;_    . Error-raised
(defstruct (emt:testral:error-raised
	    (:constructor emt:testral:make-error-raised)
	    (:conc-name emt:testral:error-raised->)
	      (:include emt:testral:alone))
   ""
   (err () :type t))

;;;_   , Contents discrimination for suite type

;;;_    . emt:testral:runform-list
(defstruct (emt:testral:runform-list
	    (:constructor emt:testral:make-runform-list)
	    (:conc-name emt:testral:runform-list->))
  
  "List of explorables"
  (els () :type (repeat emtt:explorable)))
;;;_    . emt:testral:note-list
(defstruct (emt:testral:note-list
	    (:constructor emt:testral:make-note-list)
	    (:conc-name emt:testral:note-list->))
  ""
  (notes () :type (repeat emt:testral:base)))

;;;_   , TESTRAL general report

(defstruct (emt:testral:report
	    (:constructor emt:testral:make-report)
	    (:conc-name emt:testral:report->))
  "A report sent by test-runner to viewer"
  (testrun-id     () :type emt:testral:testrun-id)
  (tester-id      () :type emt:testral:tester-id)  ;;$$OBSOLESCENT
  (run-done-p     () :type bool)  ;;$$OBSOLESCENT
  (newly-pending  () :type integer)
  ;;This is really visible-path prefix.
  (test-id-prefix () :type emt:testral:prefix-suite-id)
  (suites () :type 
	  (repeat
	   ;;Maybe this should be a type too.  NB, it's a list because
	   ;;it is a list in `emtvr:one-newstyle'
	   (list 
	    emtt:explorable
	    null ;;let's leave that an empty list for now
	    (or emt:testral:suite emt:testral:test-runner-info)))))

;;;_   , Suites etc specific reports

;;;_   , test-runner info
(defstruct (emt:testral:test-runner-info
	    (:constructor emt:testral:make-test-runner-info)
	    (:conc-name emt:testral:test-runner-info->))
  "Info describing a tester."
  (name    ()  :type string)
  (version "0" :type string)
  ;;See [[id:b4sjlt20mze0][Test-runner info]]
   ;;$$CHANGE ME
  ;;Type should become (repeat emtt:method)
  (explore-methods-supported () :type (repeat emt:testral:explore-method-id)))


;;;_   , NEW suite
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
  (badnesses () :type (repeat emt:result-badness))
  info)

;;;_   , (Suggested) emt:testral:problem
;;Name?  emt:testral:bad-launch
;;Suggested for when tester tries to launch a suite and can't.  This
;;would have or inherit a testral notelist about the problem
;;(emt:testral:note-list).  This would mean that `emt:testral:suite'
;;need have no `badnesses' of its own.




;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/testral-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/testral-types.el ends here
