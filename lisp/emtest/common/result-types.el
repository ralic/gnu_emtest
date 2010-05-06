;;;_ emtest/common/result-types.el --- Result types for emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2008  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp

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

(eval-when-compile (require 'cl))
(require 'emtest/common/testral-types)
;;;_. Body
;;;_ , Root
(defstruct emt:result-base
   "The base class of test results"
   ;;No fields.
   )

;;;_  . NEW Persistence types

;;;_   , Placeholder types
;;;_    . Placeholder of a set of versions

(defstruct emt:db:id-index.
   "Archive placeholder object"		;
   ;;Not neccessarily a simple type.
   id
   ;;Backend info - a list, first arg selects db functionality, other
   ;;args are particular to a backend
   backend
   cache  ;;Unused for now.
   )
;;;_    . placeholder of a version
(defstruct emt:db:version-index.
   "Version placeholder object"
   (id-index () :type emt:db:id-index.)
   ;;The version of the object provided, as understood by the backend.
   ;;Will eventually be used for versioning.
   version-id
   cache  ;;Unused for now.
   )
;;;_   , use-category type
(deftype emt:persist:use-category () 
   '(member correct-answer correct-type wrong-answer nil))

;;;_   , Database types are with the implementation code

;;;_  . (RETHUNK) Group

(defstruct (emt:result-group (:include emt:result-base))
   ""
   (grouping nil :type emt:test-ID)
   (info     ()  :type (repeat emt:result:info-about))
   (status   nil :type emt:result:status))


;;;_   , Status
(defstruct emt:result:status
   "The base class of group test-results' statuses"
   ;;No fields
   )
;;;_    . placeholder
(defstruct (emt:result:status:placeholder 
	      (:include emt:result:status))
   ""
   ;;No fields
   )

;;;_    . Not explored (dormant)
(defstruct (emt:result:status:not-explored (:include emt:result:status))
   ""
   )
(defstruct (emt:result:status:dormant-this-session
	      (:include emt:result:status:not-explored))
   ""
   (explanation nil :type string)
   )

(defstruct (emt:result:status:command-stops-here 
	      (:include emt:result:status:not-explored))
   ""
   ;;No fields
   )

(defstruct (emt:result:status:no-reason-given
	      (:include emt:result:status:not-explored))
   ""
   ;;No fields
   )


(defstruct (emt:result:status:unmet-dependencies
	      (:include emt:result:status:not-explored))
   ""
   (list-unmet nil :type (repeat emt:result-what-dependency)))


(defstruct (emt:result:status:too-heavy 
	      (:include emt:result:status:not-explored))
   ""
   ;;In a format understood by test-finder
   heaviness)

(defstruct (emt:result:status:todo (:include emt:result:status:not-explored))
   ""
   ;;No fields
   )

(defstruct (emt:result:status:misc (:include emt:result:status:not-explored))
   ""
   (explanation nil :type string))



;;;_    . bad-before-test
(defstruct (emt:result:status:bad-before-test
	      (:include emt:result:status))
   ""
   )
;;;_     , bad-method

(defstruct (emt:result:status:bad-method
	      (:include emt:result:status:bad-before-test))
   ""
   
   )

;;;_     , bad-control-data

(defstruct (emt:result:status:bad-control-data
	      (:include emt:result:status:bad-before-test))
   ""
   
   )


;;;_     , not-found
(defstruct (emt:result:status:not-found
	      (:include emt:result:status:bad-before-test))
   ""
   )
(defstruct (emt:result:status:not-found:not-exist
	      (:include emt:result:status:not-found))
   ""
   )
(defstruct (emt:result:status:not-found:lacks-context
	      (:include emt:result:status:not-found))
   ""
   )

;;;_     , bad-precondition
(defstruct (emt:result:status:bad-precondition 
	      (:include emt:result:status:bad-before-test))
   ""
   )

(defstruct (emt:result:status:cant-find-data
	      (:include emt:result:status:bad-precondition))
   ""
   ;;Freeform object
   data
   )

(defstruct (emt:result:status:computation-failed
	      (:include emt:result:status:bad-precondition))
   ""
   (failure-report nil :type emt:result:event:group))


;;;_     , bad-body

(defstruct (emt:result:status:bad-body
	      (:include emt:result:status:bad-before-test))
   ""
   ;;Non-native freeform object
   object  
   )

;;;_    . was-explored
(defstruct (emt:result:status:group-explored (:include emt:result:status))
   ""
   (children () :type (repeat emt:result-group)))


(defstruct (emt:result:status:was-run (:include emt:result:status))
   ""
   (events nil :type emt:result:event:group))


;;;_  . (RETHUNK) Test event

(defstruct (emt:result:event (:include emt:result-base))
   ""
   id ;;ID type is undecided.
   timestamp ;;Type is undecided.
   )
;;;_   , Pseudo-events
;;;_    . Base

(defstruct (emt:result:pseudo-event (:include emt:result:event))
   ""
   )

;;;_    . Event-group

(defstruct (emt:result:event:group (:include emt:result:pseudo-event))
   ""
   (info-about nil :type (repeat emt:result:info-about))
   (children ()    :type (repeat emt:result:event))
   (aborted-p nil  :type bool))

;;;_    . Others

(defstruct (emt:result:event:messages (:include emt:result:pseudo-event))
   "Messages as from emacs' *Messages* buffer"
   ;;Format is undecided yet
   data
   )

(defstruct (emt:result:event:stage-begins (:include emt:result:pseudo-event))
   ""
   (name nil :type string))


(defstruct (emt:result:event:run-of-passes (:include emt:result:pseudo-event))
   ""
   (num nil :type integer))


;;May go away or be placed somewhere else
(defstruct (emt:result:event:parameters (:include emt:result:pseudo-event))
   ""
   ;;A list of (key value) pairs in the target language.
   (kv-list () :type (repeat (list * *))))

;;May go away.
(defstruct (emt:result:event:used-persister (:include emt:result:pseudo-event))
   ""
   role
   object
   )

;;;_   , Grade events
(defstruct (emt:result:event:grade (:include emt:result:event))
   ""
   (grade 'ungraded :type emt:result-gradedness)
   form  ;;The form
   (info-about () :type (repeat emt:result:info-about))
   (diagnostic-info () :type (repeat emt:result:diag:bool)))

;;;_  . Info-about (NO, these should be list objects)

(defstruct emt:result:info-about
   ""
   )
;;;_   , Name
(defstruct (emt:result:info-about:name (:include emt:result:info-about))
   ""
   (name nil :type string))


;;;_   , Docstring
(defstruct (emt:result:info-about:docstring (:include emt:result:info-about))
   ""
   (string nil :type string))


;;;_   , Location
(defstruct (emt:result:info-about:location (:include emt:result:info-about))
   "Location.  An abstract class")

(defstruct (emt:result:info-about:location:file-line
	      (:include emt:result:info-about:location)) 
   "Location as file and line number"
   (file nil :type string)
   (line nil :type integer))

(defstruct (emt:result:info-about:location:file-by-platform
	      (:include emt:result:info-about:location)) 
   "Location as file, platform, and platform-specific data"
   (file nil :type string)
   platform ;;A Platform-ID - type is TBD
   data ;;Non-native freeform data.
   )

(defstruct (emt:result:info-about:location:adhoc
	      (:include emt:result:info-about:location)) 
   "Indicates that there is no location; test was ad hoc")

;;;_   , Likely-fix
(defstruct (emt:result:info-about:likely-fix
	      (:include emt:result:info-about))
   "Advice from the test to a user.  Useful for recurring mistakes and
especially in configuration testing for new installations"
   (string :type string))

;;;_   , Style-Warning
(defstruct (emt:result:info-about:style-warning 
	      (:include emt:result:info-about))
   "Advice from library code to the test-writer"
   (string :type string))

;;;_   , Summary info
(defstruct (emt:result:info-about:summary (:include emt:result:info-about))
   ""
   ;;No type for this yet.
   last-mod-time
   )

(deftype emt:result:info-about:summary:event-list-status:type
   () '(member cant-tell all-passed some-dormant some-fail aborted))

(defstruct (emt:result:info-about:summary:event-list-status
	      (:include emt:result:info-about:summary))
   ""
   (status nil
      :type emt:result:info-about:summary:event-list-status:type))

(defstruct (emt:result:info-about:summary:counts
	      (:include emt:result:info-about:summary))
   ""
   (all () :type integer)
   (checks-passed () :type integer)
   (checks-failed () :type integer)
   (checks-bad () :type integer)
   (tests-passed () :type integer)
   (tests-failed () :type integer)
   (tests-bad () :type integer)
   (tests-dormant () :type integer)
   (tests-aborted () :type integer))


;;;_   , Test form
(defstruct (emt:result:info-about:test-form (:include emt:result:info-about))
   ""
   ;;S-expression or non-native
   form
   )
;;;_   , emt:result:info-about:tried-persist
;;Obsolete
'
(defstruct (emt:result:info-about:tried-persist 
	      (:include emt:result:info-about))
   ""
   (placeholder () :type emt:db:version-index.)
   (unique () :type (member t none-found too-many-found))
   (placeholder-ix () :type integer))  ;;0-based


;;;_  . (REORGANIZE ME) Test ID
(defstruct emt:test-ID
   ""
   (context () :type (repeat (list emt:test-ID:context:key *)))
   (explore-next () :type emt:test-ID:e-n))

;;;_   , Context (OBSOLETE)

;;Keys include testrun-ID, tester-ID, and user extensions.  For Elisp,
;;Symbol as Suite-ID.

(deftype emt:test-ID:context:key ()
   '(or
       (member testrun-ID tester-ID)
       ;;Unwritten:  Or it's a member of some user-defined list of
       ;;symbols. 
       symbol
       ;;'(satisfies)
       ))

;;;_    . Suite-ID (OBSOLETE)
;;A symbol
;;;_    . clause-ID (OBSOLETE)
;;To be determined, see "design.org" for some possibilities.

;;;_   , Explore-next (NOT obsolete)

;;;_    . Base
(defstruct emt:test-ID:e-n
   ""
   ;;Abstract.
   )
;;;_    . emt:test-ID:e-n:hello
(defstruct (emt:test-ID:e-n:hello (:include emt:test-ID:e-n))
   "")

;;;_    . emt:test-ID:e-n:invalid
(defstruct (emt:test-ID:e-n:invalid (:include emt:test-ID:e-n))
   "")

;;;_    . emt:test-ID:e-n:suite
(defstruct (emt:test-ID:e-n:suite (:include emt:test-ID:e-n))
   ""
   ;;Type not expressed.  Co-varies with tester.
   ;;$$This is NOT emt:testral:suite-id, which is just a name.
   suite-ID)

;;;_    . emt:test-ID:e-n:form
(defstruct (emt:test-ID:e-n:form (:include emt:test-ID:e-n))
   ""
   ;;A test form.
   test-form
   )
;;;_    . emt:test-ID:e-n:indexed-clause
;;Formerly was called `*:unique-clause'
(defstruct (emt:test-ID:e-n:indexed-clause (:include emt:test-ID:e-n))
   ""
   (suite-sym () :type symbol)
   ;;Formerly index was considered part of context.
   (clause-index 0 :type integer))


;;;_    . emt:test-ID:e-n:library
(defstruct (emt:test-ID:e-n:library (:include emt:test-ID:e-n))
   ""
   ;;Abstract.
   )
;;Not settled yet
(defstruct (emt:test-ID:e-n:library:elisp-load 
	      (:include emt:test-ID:e-n:library))
   ""
   load-name
   )
(defstruct (emt:test-ID:e-n:library:elisp-file 
	      (:include emt:test-ID:e-n:library))
   ""
   file-name
   )

;;;_    . emt:test-ID:e-n:project
;;Not settled yet
(defstruct (emt:test-ID:e-n:project (:include emt:test-ID:e-n))
   ""
   name
   )

;;;_    . emt:test-ID:e-n:all-projects
(defstruct (emt:test-ID:e-n:all-projects (:include emt:test-ID:e-n))
   ""
   ;;No fields
   )

;;;_    . emt:test-ID:e-n:all-libraries
(defstruct (emt:test-ID:e-n:all-libraries (:include emt:test-ID:e-n))
   ""
   ;;No fields
   )

;;;_    . emt:test-ID:e-n:all-testers
(defstruct (emt:test-ID:e-n:all-testers (:include emt:test-ID:e-n))
   ""
   )

;;;_    . emt:test-ID:e-n:from-t-dir
(defstruct (emt:test-ID:e-n:from-t-dir (:include emt:test-ID:e-n))
   ""
   (dir-name () :type string))


;;;_    . emt:test-ID:e-n:from-dir
(defstruct (emt:test-ID:e-n:from-dir (:include emt:test-ID:e-n))
   ""
   (dir-name () :type string))
;;;_    . emt:test-ID:e-n:dynamic
(defstruct (emt:test-ID:e-n:dynamic 
	      (:include emt:test-ID:e-n))
   "Special method to make explorables at runtime."
   name
   params)


;;;_   , emtt:explorable (Full runnable)
(defstruct (emtt:explorable
	      (:conc-name emtt:explorable->)
	      (:constructor emtt:make-explorable))
   "All the information needed to specify how to run a test or suite/"

   (id () 
      :type emt:test-ID:e-n
      :doc "What to launch for this exploration.")
   
   (path-prefix () 
      :type emt:testral:partial-suite-id
      :doc "The path prefix so far in the descent, for presentation")

   (properties () 
      :type (repeat (list symbol *))
      :doc "The properties that this exploranle has when it's run")
   (aliases () 
      :type (repeat emt:test-ID:e-n) 
      :doc "A possibly empty list of othed IDs that would launch the
      same thing")) 

;;;_   , emtt:dynamic-method
(defstruct emtt:dynamic-method
   "A dynamic exploration method."
   name
   keys)

;;;_   , emtt:method (Union of those types)
(deftype emtt:method ()
   ", for test-runner-info"
   '(or emtt:dynamic-method emtt:explorable))


;;;_  . emt:test-info (OBSOLETE)
;;Doesn't seem to be used anywhere.  
'
(defstruct emt:test-info
   ""
   test-ID
   control
   )

;;;_  . unmet-dependency
;;Unsettled
(defstruct emt:result-what-dependency
   ""
   ;;Abstract.  No fields
   )

;;;_  . NEW diagnostic-info (May be obsolete)

;;;_   , Boolean-valued things
;;;_    . emt:result:diag:bool
;;This class is abstract.  If it is ever tried to be used, make a new
;;class for it using this as a base.
(defstruct emt:result:diag:bool
   "Base class for boolean-value things in diagnostic traces"

   (info-about nil :type (repeat emt:result:info-about))
   (status () :type (member t nil error)))


;;;_    . emt:result:diag:error
;;rtest:gives-error will use this too.  And "should" and others
(defstruct (emt:result:diag:error (:include emt:result:diag:bool))
   ""
   error)


;;;_    . emt:result:diag:call

(defstruct (emt:result:diag:call (:include emt:result:diag:bool))
   ""
   call-sexp
   ;;List of TBD
   (tried () :type (repeat emt:result:diag:tried.)))


;;;_    . emt:result:diag:logic

(defstruct (emt:result:diag:logic (:include emt:result:diag:bool))
   "Used to trace `and', `or', `not', etc."
   (functor nil :type symbol)
   (traces () :type (list emt:result:diag:bool)))

;;;_   , Try-types
;;;_    . Base type emt:result:diag:tried.
(defstruct emt:result:diag:tried.
   "Tried to use something as substitute argument"
   ;;0-based
   (arg-ix () :type integer))

;;;_    . emt:result:diag:tried-persist-archive.
(defstruct (emt:result:diag:tried-persist-archive.
	      (:include emt:result:diag:tried.))
   "Tried to use a persist archive as substitute argument"
   (placeholder () :type emt:db:id-index.)
   (use-category () :type emt:persist:use-category)
   (reason () :type (member t too-many-found none-found)))

;;;_    . emt:result:diag:tried-persist-version.
(defstruct (emt:result:diag:tried-persist-version.
	      (:include emt:result:diag:tried.))
   "Tried to use a persist version as substitute argument"
   (placeholder () :type emt:db:version-index.)
   ;;Don't really need this because placeholder provides it.
   ;;(use-category () :type emt:persist:use-category)

   )

;;;_  . (OBSOLETE) gradedness
'
(deftype emt:result-gradedness () '(member pass fail ungraded))
;;;_  . badnesses
;;Later this type may be expanded, because the badnesses carry data.
;;For now, leave it open, because we're developing the
;;(bad-before-test not-found) badness
;;
'(deftype emt:result-badness () '(member fail ungraded dormant))
(deftype emt:result-badness () t)

;;;_   , Summary info
;;Set of badnesses plus count of total checks.

;;;_  . IDs (NEWER)
;;;_   , Suites

;;We'd like to can restrict this.  But it depends on some internal
;;information about how elements are interpreted, depending on
;;`method-relaunch'.  Not clear how that can be passed into here.
;;$$RETHINKING We distinguish conceptual path from how-to-run
(deftype emt:testral:id-element () 
   "Id elements are strings."
   'string)
(deftype emt:testral:suite-id () '(repeat emt:testral:id-element))
(deftype emt:testral:partial-suite-id () '(repeat emt:testral:id-element))
(deftype emt:testral:prefix-suite-id () '(repeat emt:testral:id-element))
;;;_   , Others
;;Could have been an integer instead.
(defalias 'emt:testral:testrun-id-p 'stringp)
(defalias 'emt:testral:tester-id-p 'stringp)

;;;_   , Run instruction

;;$$REMOVE ME - This is unclear, and I have changed the approach
;;This is obsolete, see [[id:4v4h3s20mze0][Representing them]]
(defalias 'emt:testral:explore-method-id-p 'stringp)
(deftype emt:testral:explore-id ()
   "How to run an explorable."
   '(list* emt:testral:explore-method-id (repeat t)))

;;;_   , emt:testral:both-ids (Both)

;;$$OBSOLETE NEVER USED
;;This has merged with emtt:explorable.
'
(defstruct emt:testral:both-ids
   "ID sufficient to both name and (re)run a runnable."
   
   (id () 
      :type emt:test-ID
      :doc "Test ID, sufficient to run the test again")
   (aliases () :type (repeat emt:test-ID) 
      :doc "A possibly empty list of aliases")
   (presentation-path () 
      :type emt:testral:partial-suite-id
      :doc "Suggested path to the test result's presentation in the viewer"))




;;;_  . NEW for viewer (All moved or deleted)
;;;_   , (OBSOLESCENT) TESTRAL notes in viewer
'
(defstruct emt:view:testral
   ""
   ;;No name/id/testrun, that pertains to suites.
   (main () :type (or emt:testral:alone emt:testral:push))
   (end  () :type (or null emt:testral:pop))
   (args () :type (repeat emt:testral:separate))

   ;;Maybe cache what type of children we're holding: testral or
   ;;note-list.  (Rename `testral' symbol.  `testral-scoped'?)
   (child-type () :type (member scoped tails nil))
   (children () 
      :type 
      (or 
	 ;;For when we haven't scoped children yet.  The lists
	 ;;correspond to after main and after each arg.
	 (repeat (repeat emt:testral:base))  
	 (repeat emt:view:testral)))
   (display-info ()  :type (or null emt:view:display:base)))

;;;_   , (OBSOLETE) Suite nodes in viewer
'
(defstruct emt:view:suite
   ""
   (name ()      :type emt:testral:id-element)
   (full-id ()   :type emt:testral:suite-id)
   (unique-id () :type (or string null))
   ;;This is becoming more of a datestamp/circumstance info.
   (testrun-id ():type emt:testral:testrun-id)
   ;;OR info for a particular tester
   (suite ()     :type (or null emt:testral:suite))
   ;;$$CHANGE ME to emtt:explorable
   (how-to-run ():type emt:test-ID:e-n)
   ;;Actual known children.  This is the n-ary part of the structure.
   ;;NB, even if suite is given, it only tells us their ids, so this
   ;;info is not available in suite.
   (child-type () :type (member suite testral nil))
   ;;$$DESIGNME What about children that have not been reported, but
   ;;where we know how to run them?  That may be heterogeneous
   (children () 
      :type 
      (or 
	 (repeat emt:view:suite)
	 (repeat emt:view:testral)))  
   ;;Summarized badnesses, including any from suite.
   (sum-badnesses () :type (repeat emt:result-badness))
   (display-info ()  :type (or null emt:view:display:base)))

;;;_   , (OBSOLETE) Display info
;;;_    . (OBSOLESCENT) Base emt:view:display:base
;;Empty.  Don't make ctor etc.
'
(defstruct emt:view:display:base
   "")

;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/result-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/result-types.el ends here
