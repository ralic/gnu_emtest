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
;;$$OBSOLESCENT
(defstruct emt:result-base
   "The base class of test results"
   ;;No fields.
   )

;;;_  . ($$OBSOLESCENT) Group
;;$$OBSOLESCENT
;;This and deriveds are still in use in the examples.
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


;;;_  . ($$OBSOLESCENT) Test event

;;$$OBSOLESCENT Still used in some places (testhelp, old tests of
;;should, plain-viewer)
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

;;;_  . Info-about ($$OBSOLETE, these should be list objects)
;;$$OBSOLESCENT - used in testhelp examples and in plain-viewer
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
   (placeholder () :type emt:db:version-index)
   (unique () :type (member t none-found too-many-found))
   (placeholder-ix () :type integer))  ;;0-based


;;;_  . Test ID
;;;_   , Original type ($$OBSOLETE)
(defstruct emt:test-ID
   ""
   (context () :type (repeat (list emt:test-ID:context:key *)))
   (explore-next () :type emthow))

;;;_   , Context ($$OBSOLETE)

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


;;;_ , emthow
;;Most of this could be moved into runner/explorers/ directory.  
;;;_  . Base
(defstruct (emthow
	      (:copier nil)
	      (:constructor emthow:make)
	      (:conc-name emthow->))
   ""
   ;;Abstract.
   )
;;;_  . emthow:hello
(defstruct (emthow:hello
	      (:copier nil)
	      (:constructor emthow:make-hello)
	      (:conc-name emthow:hello->)
	      (:include emthow))
   "")

;;;_  . emthow:invalid
(defstruct (emthow:invalid
	      (:copier nil)
	      (:constructor emthow:make-invalid)
	      (:conc-name emthow:invalid->)
	      (:include emthow))
   "")

;;;_  . emthow:suite
(defstruct (emthow:suite
	      (:copier nil)
	      (:constructor emthow:make-suite)
	      (:conc-name emthow:suite->)
	      (:include emthow))
   ""
   ;;Type not expressed.  Co-varies with tester.
   ;;$$This is NOT emt:testral:suite-id, which is just a name.
   suite-ID)

;;;_  . emthow:form
(defstruct (emthow:form
	      (:copier nil)
	      (:constructor emthow:make-form)
	      (:conc-name emthow:form->)
	      (:include emthow))
   ""
   ;;A test form.
   test-form
   )
;;;_  . emthow:indexed-clause
(defstruct (emthow:indexed-clause
	      (:copier nil)
	      (:constructor emthow:make-indexed-clause)
	      (:conc-name emthow:indexed-clause->)
	      (:include emthow))
   ""
   (suite-sym () :type symbol)
   ;;Formerly index was considered part of context.
   (clause-index 0 :type integer))


;;;_  . emthow:library
(defstruct (emthow:library
	      (:copier nil)
	      (:constructor nil)
	      (:conc-name emthow:library->)
	      (:include emthow))
   ""
   ;;Abstract.
   )
;;Not settled yet


(defstruct (emthow:library:elisp-load
	      (:copier nil)
	      (:constructor emthow:make-library:elisp-load)
	      (:conc-name emthow:library:elisp-load->)
	      (:include emthow:library))
   ""
   load-name
   lib-sym
   )
(defstruct (emthow:library:elisp-file
	      (:constructor emthow:make-library:elisp-file)
	      (:conc-name emthow:library:elisp-file->)
	      (:include emthow:library))
   ""
   file-name
   lib-sym
   )

;;;_  . emthow:project
;;Not settled yet
(defstruct (emthow:project
	    (:constructor emthow:make-project)
	    (:conc-name emthow:project->)
	    (:include emthow))
  ""
  name
  )

;;;_  . emthow:all-projects
(defstruct (emthow:all-projects
	    (:constructor emthow:make-all-projects)
	    (:conc-name emthow:all-projects->)
	    (:include emthow))
   ""
   ;;No fields
   )

;;;_  . emthow:all-libraries
(defstruct (emthow:all-libraries
	    (:constructor emthow:make-all-libraries)
	    (:conc-name emthow:all-libraries->)
	    (:include emthow))
   ""
   ;;No fields
   )

;;;_  . emthow:all-testers
(defstruct (emthow:all-testers
	    (:constructor emthow:make-all-testers)
	    (:conc-name emthow:all-testers->)
	    (:include emthow))
   ""
   )

;;;_  . emthow:from-t-dir
(defstruct (emthow:from-t-dir
	      (:copier nil)
	      (:constructor emthow:make-from-t-dir)
	      (:conc-name emthow:from-t-dir->)
	      (:include emthow))
   ""
   (dir-name () :type string))


;;;_  . emthow:from-dir
(defstruct (emthow:from-dir
	      (:copier nil)
	      (:constructor emthow:make-from-dir)
	      (:conc-name emthow:from-dir->)
	      (:include emthow))
   ""
   (dir-name () :type string))
;;;_  . emthow:dynamic
(defstruct (emthow:dynamic
	      (:copier nil)
	      (:constructor emthow:make-dynamic)
	      (:conc-name emthow:dynamic->)
	      (:include emthow))
   "Special method to make explorables at runtime."
   name
   params)


;;;_  . emtt:explorable (Full runnable)
(defstruct (emtt:explorable
	      (:copier nil)
	      (:conc-name emtt:explorable->)
	      (:constructor emtt:make-explorable))
   "All the information needed to specify how to run a test or suite/"
   (how-to-run () 
      :type emthow
      :doc "What to launch for this exploration.")
   
   (prestn-path () 
      :type emt:testral:partial-suite-id
      :doc "The presentation path so far")

   (properties () 
      :type (repeat (list symbol *))
      :doc "The properties that this explorable has when it's run")
   ;;Aliases might also allow a string as UUID
   (aliases () 
      :type (repeat emthow) 
      :doc "A possibly empty list of other IDs that would launch the
      same thing")) 

;;;_  . emtt:dynamic-method
;;$$USE ME
(defstruct (emtt:dynamic-method
	      (:constructor emtt:make-dynamic-method)
	      (:copier nil)
	      (:conc-name emtt:dynamic-method->))
   "A dynamic exploration method."
   name
   keys)

;;;_  . emtt:method (Union of those types)
;;$$USE ME
(deftype emtt:method ()
   "A static or dynamic exploration method, for test-runner-info"
   '(or emtt:dynamic-method emtt:explorable))


;;;_  . (OBSOLESCENT) gradedness
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
(defalias 'emt:testral:testrun-id-p 'stringp)
(defalias 'emt:testral:tester-id-p 'stringp)


;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/result-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/result-types.el ends here
