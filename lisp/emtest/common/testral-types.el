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

(defstruct emt:testral:base
   ""
   id ;;Shared just between scope co-ordinating notes
   ;;Not clear that parent-id is of general use.
   parent-id ;;`nil' for root events that have no parent.
   info

   ;;Reflects only the note's's intrinsic problems.  Even push/pops
   ;;need it in case (say) a whole stage is dormantized or aborted.
   (badnesses () :type (repeat emt:result-badness)))

;;;_   , Basic notes 
;;(All inherit from the base class, none have data)
;;;_    . Alone
(defstruct (emt:testral:alone
	      (:include emt:testral:base))
   ""
   )
;;;_    . Push
(defstruct (emt:testral:push
	      (:include emt:testral:base))
   ""
   fenceposting ;;
   )
;;;_    . Pop
(defstruct (emt:testral:pop
	      (:include emt:testral:base))
   ""
   )
;;;_    . Separate
(defstruct (emt:testral:separate
	      (:include emt:testral:base))
   ""
   )

;;;_   , Specific ones
;;;_    . Doc
(defstruct (emt:testral:doc
	      (:include emt:testral:alone))
   "A note indicating a docstring"
   (str () :type string))

;;;_    . Check
;;"Should" will ct these.
(defstruct (emt:testral:check:push
	      (:include emt:testral:push))
   ""
   )
(defstruct (emt:testral:check:pop
	      (:include emt:testral:pop))
   ""
   )
;;;_    . Stage
(defstruct (emt:testral:stage:push
	      (:include emt:testral:push))
   ""
   (name () :type string))

(defstruct (emt:testral:stage:pop
	      (:include emt:testral:pop))
   ""
   )

;;;_    . Error-raised
(defstruct (emt:testral:error-raised
	      (:include emt:testral:alone))
   ""
   (err () :type t))

;;;_   , (Obsolete) TESTRAL notes in a DLL
'  ;;Obsolete
'
(deftype emt:result-node () 
   'emt:testral:base)


   '(satisfies 
       (lambda (obj)
	  ;;Breaks encap, but we can't just use `dll-element' to get
	  ;;it because we don't know dll.
	  (typep (elib-node-data obj) emt:testral:base)))


;;;_   , Contents discrimination for suite type

;;;_    . emt:testral:runform-list
(defstruct emt:testral:runform-list
   ""
   ;;Run-form

   ;;Or it could be:
   ;;(repeat emt:testral:both-ids)
   ;;It may have to be, since children must be launchable but have to
   ;;be comparable by visible place.

   ;;$$CHANGE ME Should repeat an emtt:explorable
   (els () :type (repeat emt:test-ID:e-n)))
;;;_    . emt:testral:note-list
(defstruct emt:testral:note-list
   ""
   (notes () :type (repeat emt:testral:base)))

;;;_   , TESTRAL general report

(defstruct emt:testral:report
   "A report sent by test-runner to viewer"
   (testrun-id () :type emt:testral:testrun-id)
   (tester-id  () :type emt:testral:tester-id)
   ;;This is really visible-path prefix.
   (test-id-prefix () :type emt:testral:prefix-suite-id)
   (suites () :type 
      (repeat
	 ;;Maybe this should be a type too.
	 (list 
	    ;;$$CHANGE ME
	    ;;The first should be a emtt:explorable.  It may fold in
	    ;;the second field, but let's leave that an empty list for
	    ;;now. 
	    emt:test-ID:e-n
	    emt:testral:partial-suite-id
	    (or emt:testral:suite emt:testral:test-runner-info)))))

;;;_   , Suites etc specific reports
;;;_    . New TESTRAL suite report type

(deftype emtvr:suite-newstyle-id ()
   "Can be a UUID string or a how-to-run object"
   '(or string emt:test-ID:e-n))
(defstruct emtvr:suite-newstyle
   ""
   (id () :type emtvr:suite-newstyle-id)
   ;;How to run this as a test.
   (how-to-run ():type emt:test-ID:e-n)
   ;;Tester or how to call tester

   ;;The path to the corresponding node in the tree.
   (presentation-path ()   :type emt:testral:suite-id)

   ;;This is becoming more of a datestamp/circumstance info.
   (testrun-id ():type emt:testral:testrun-id)
   ;;OR info for a particular tester
   (suite ()     :type (or null 
			  emt:testral:suite 
			  emt:testral:test-runner-info)))

;;;_   , test-runner info
(defstruct emt:testral:test-runner-info
   "Info describing a tester.  Not fleshed out yet."
   (name () :type string)
   ;;$$CHANGE ME - See [[id:b4sjlt20mze0][Test-runner info]]
   ;;Type should become (repeat emtt:method)
   (explore-methods-supported () :type (repeat emt:testral:explore-method-id)))


;;;_   , NEW suite
(defstruct emt:testral:suite
   ""
   ;;Either TESTRAL list or ids of child suites or nil.
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
