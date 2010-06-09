;;;_ common/testral-types/testhelp.el --- Examples of TESTRAL types
;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp, maint

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

(require 'emtest/common/result-types)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/match)
(require 'emtest/runner/testral)

;;;_. Body
;;;_ , Preliminary
;;;_  . Pattern ctors

;;$$OBSOLESCENT
(emtm:define-struct-governor emt:view:suite
   name full-id unique-id suite child-type children sum-badnesses )

(emtm:define-struct-governor emt:view:testral
   ;;We leave out `display-info' since we can't expect to compare
   ;;against it meaningfully.
   main end args children child-type)

;;;_ , Examples
(defconst emt:testral:thd:examples
   (emtg:define+ ;;xmp:4c2dde89-a458-4cc7-a148-3725d8e7d692
      ((project emtest)(sub-project testral)(library types))
      ;;I don't like having `name' here but it is currently needed.  It
      ;;will get in the way when name parallelism is wanted for some
      ;;things later.  Similarly, `what-test' is needed but a PITA.
      ;;It was needed so that we could see suites, which aren't tagged
      ;;for that.
      (transparent-tags () (type subtype name role what-test tagged-by))
      (group
	 ;; Notes alone
	 ((type note)(subtype alone))
	 (type-must-be () emt:testral:alone)
	 (item ((name error-1))
	    (emt:testral:make-error-raised
	       :badnesses '(ungraded)
	       :err
	       ;;We ct it from the actual error form, so we can't get out
	       ;;of sync with it.
	       (emtg:see-err
		  (emtg:value 
		     :narrow ((name error-1)(type test-form))
		     :ignore-tags (subtype))))))
      (group
	 ;; Scoped notes
	 ((type note)(subtype scoped))
	 (group ((what checks))	;;Want better tagname
	    (group ((result pass)) ;;Want better tagname
	       (type-must-be () (or emt:testral:push emt:testral:pop))

	       ;;(item ((id)) 100)
	       ;;(item ((parent-id)) nil)
	       (item ((name push))
		  (emt:testral:make-check:push
		     :id 100
		     :parent-id nil
		     :info ()
		     :fenceposting nil))
	       (item ((name pop))
		  (emt:testral:make-check:pop
		     :id 100
		     :badnesses ())))
	 
	    ;;Add other results.  Fail.  Error.
	 
	    ))
      (group
	 ((type test-form))
	 ;;The error-1 note is made by evalling this, so it is the error
	 ;;that is recorded.
	 (item
	    ((name error-1))
	    '(error "An example error"))

	 ;;The `name' values here parallel those in (type testral-note-list)
	 (item
	    ((name empty))
	    '(progn))

	 (item
	    ((name one-node))
	    (emtg (type test-form)(name error-1)))

	 )

      (group
	 ;; TESTRAL
	 ((type testral-note-list)(subtype unconformed))
	 (type-must-be () emt:testral:note-list)
	 (item
	    ((name empty))
	    (emt:testral:make-note-list
	       :notes
	       (list)))
      
	 ;;Maybe alias this as (name error-1)
	 (item
	    ((name one-node))
	    (emt:testral:make-note-list
	       :notes
	       (list
		  (emtg (type note)(subtype alone)(name error-1))))))

      ;;Suites' intrinsic badnesses
      (group
	 ((type suite-own-badness-list))
	 (type-must-be () (repeat emt:result-badness))
	 (item
	    ((name test-bad))
	    '())
      
	 (item
	    ((name test-passes))
	    '())
      
	 (item
	    ((name has-children-1))
	    '())

	 (item
	    ((name gone))
	    ;;$$REVIEWME This representation is tentative.
	    (list
	       '(bad-before-test not-found))))

      ;;$$TEMPORARY until mapping can be easily used.
      (group
	 ((type suite-own-badness-list)(tagged-by role&test))
	 (type-must-be () (repeat emt:result-badness))
	 (item
	    ((what-test test-1)(role original-add))
	    (emtg (type suite-own-badness-list)(name test-bad)))
	 (item
	    ((what-test test-1)(role replace))
	    (emtg (type suite-own-badness-list)(name test-passes)))

	 (item
	    ((what-test test-1)(role remove-previous))
	    (emtg (type suite-own-badness-list)(name gone)))
      
	 (item
	    ((what-test test-2))
	    (emtg (type suite-own-badness-list)(name test-passes))))

      ;;Suites' badnesses, including that of their TESTRAL notes.
      (group
	 ((type suite-own+notes-badness-list))
	 (type-must-be () (repeat emt:result-badness))
	 (item
	    ((name test-bad))
	    '(ungraded))
      
	 (item
	    ((name test-passes))
	    '())
      
	 (item
	    ((name has-children-1))
	    '(ungraded))

	 (item
	    ((name gone))
	    (emtg (type suite-own-badness-list)(name gone))))

;;       (group
;; 	 ;;$$RETHINK ME  Suites should only have own badnesses
;; 	 ((type suite-badness-list))
;; 	 (type-must-be () (repeat emt:result-badness))

;; 	 (item
;; 	    ((name test-bad)(tagged-by name))
;; 	    '(ungraded))
;; 	 (item
;; 	    ((name test-passes)(tagged-by name))
;; 	    '())
      
;; 	 (item
;; 	    ((name has-children-1)(tagged-by name))
;; 	    '(ungraded))
;; 	 (item
;; 	    ((name gone)(tagged-by name))
;; 	    (emtg (type suite-own-badness-list)(name gone)))

;; 	 ;;For the sequence-of-reports tests (These alias other suite items)
;; 	 ;;$$OBSOLESCENT in favor of mapping
;; 	 (item
;; 	    ((what-test test-1)(role original-add))
;; 	    (emtg (type suite-badness-list)(name test-bad)))
;; 	 (item
;; 	    ((what-test test-1)(role replace))
;; 	    (emtg (type suite-badness-list)(name test-passes)))

;; 	 (item
;; 	    ((what-test test-1)(role remove-previous))
;; 	    (emtg (type suite-own-badness-list)(name gone)))
      
;; 	 (item
;; 	    ((what-test test-2))
;; 	    (emtg (type suite-badness-list)(name test-passes))))

      (group
	 ;; Suites
	 ((type suite)(tagged-by name))
	 (type-must-be () emt:testral:suite)
	 (item
	    ((name test-bad))
	    (emt:testral:make-suite
	       :contents (emtg 
			    (type testral-note-list)
			    (subtype unconformed)
			    (name one-node))
	       :badnesses 
	       (emtg (type suite-own-badness-list)(name test-bad))
	       :info ()))
      
	 (item
	    ((name test-passes))
	    (emt:testral:make-suite
	       :contents (emtg 
			    (type testral-note-list)
			    (subtype unconformed)
			    (name empty))
	       :badnesses 
	       (emtg (type suite-own-badness-list)(name test-passes))
	       :info ()))
      
	 ;;Suite reporting child suites (here, child is just an indexed
	 ;;clause)
	 (item
	    ((name has-children-1))
	    (emt:testral:make-suite
	       :contents
	       (emt:testral:make-runform-list
		  :els
		  (list 
		     (emtg (type explorable)(what-test index-1))))
	       :badnesses 
	       (emtg (type suite-own-badness-list)(name has-children-1))
	       :info ()))
	 (item
	    ((name gone))
	    (emt:testral:make-suite
	       :contents (emtg 
			    (type testral-note-list)
			    (subtype unconformed)
			    (name empty))
	       :badnesses 
	       (emtg (type suite-own-badness-list)
		  (what-test test-1)
		  (role remove-previous))
	       :info ())))
      
      (group
	 ;;$$OBSOLESCENT in favor of mapping
	 ;; Suites aliases
	 ;;For the sequence-of-reports tests
	 ((type suite)(tagged-by role&test))
	 (type-must-be () emt:testral:suite)
	 (item
	    ((what-test test-1)(role original-add))
	    (emtg (type suite)(name test-bad)))
	 (item
	    ((what-test test-1)(role replace))
	    (emtg (type suite)(name test-passes)))

	 (item
	    ((what-test test-1)(role remove-previous))
	    (emtg (type suite)(name gone)))
      
	 (item
	    ((what-test test-2))
	    (emtg (type suite)(name test-passes))))

      (group
	 ((type presentation-name))
	 (item
	    ((what-test test-1))
	    '("0"))
	 (item
	    ((what-test test-2))
	    '("1")))
      (group
	 ((type presentation-path))
	 (item
	    ((what-test test-1))
	    (cons "0"
	       (emtg (type presentation-name)(what-test test-1))))
	 (item
	    ((what-test test-2))
	    (cons "0"
	       (emtg (type presentation-name)(what-test test-2)))))

      (group
	 ;; Test-runner info
	 ((type test-runner-info))
	 (type-must-be () emt:testral:test-runner-info)
	 (item
	    ()
	    (emt:testral:make-test-runner-info
	       :name "My test runner")))
      (group
	 ((type how-to-run))
	 (item ((name test-1) ;;`name' seems redundant here now.
		  (what-test test-1))
	    (emthow:make-suite :suite-ID 'test-1))
	 (item ((name test-2)
		  (what-test test-2))
	    (emthow:make-suite :suite-ID 'test-2))
	 (item
	    ((what-test index-1))
	    (emthow:make-indexed-clause
	       :suite-sym 'first-suite
	       :clause-index 0)))

      (group
	 ((type explorable))
	 (type-must-be () emtt:explorable)
	 (item
	    ((name just-test-runner))
	    (emtt:make-explorable
	       :how-to-run  (emthow:make-hello)
	       :prestn-path () 
	       :properties  ()
	       :aliases     ()))
	 (item
	    ((what-test index-1))
	    (emtt:make-explorable
	       :how-to-run  (emtg (type how-to-run)(what-test index-1))
	       :prestn-path ()  ;;Empty presentation name
	       :properties  ()
	       :aliases     ()))
	 
	 (item
	    ((what-test test-1))
	    (emtt:make-explorable
	       :how-to-run  (emtg (type how-to-run)(what-test test-1))
	       :prestn-path (emtg (type presentation-name)(what-test test-1))
	       :properties  ()
	       :aliases     ()))

	 (item
	    ((what-test test-2))
	    (emtt:make-explorable
	       :how-to-run  (emtg (type how-to-run)(what-test test-2))
	       :prestn-path (emtg (type presentation-name)(what-test test-2))
	       :properties  ()
	       :aliases     ())))
      
      (group
	 ((type testrun-id))
	 (item
	    ((role original-add))
	    "0")
	 (item
	    ((role replace))
	    "1")
	 (item
	    ((role remove-previous))
	    "1"))
      
      ;;Mapping between name tagging and role&test tagging.  Tests use
      ;;this to get appropriate view-types when they iterate over
      ;;names.
      (group
	 ((type map:name->role&test-list))
	 ;;$$IMPROVE ME type can become more specific when it is made
	 ;;more specific in tagnames.el
	 (type-must-be () (repeat (repeat *)))

	 (item ((result-name test-bad))
	    '(((what-test test-1)(role original-add))))
	 (item ((result-name test-passes))
	    '(((what-test test-1)(role replace))
		((what-test test-2))))
	 (item ((result-name gone))
	    '(((what-test test-1)(role remove-previous)))))
      
      ;;$$IMPROVE ME Add a mapping from role&test to name tag.  Then
      ;;the mapping `map:name->role&test' would be cted from it.
      ;;Reports would use this to build themselves from parts.

      ;;Largely exists because "gone" won't have a view-node
      (group
	 ((type has-viewnode-p))
	 (type-must-be () bool)
	 (item ((result-name test-bad))
	    t)
	 (item ((result-name test-passes))
	    t)
	 (item ((result-name gone))
	    nil))

      (group
	 ;; Report
	 ((type report))
	 (type-must-be () emt:testral:report)
	 (item
	    ((name empty))
	    (emt:testral:make-report
	       :testrun-id (emtg (type testrun-id)(role original-add))
	       :tester-id "0"
	       :test-id-prefix ()
	       :suites (list)))

	 ;;Just-test-runner report
	 (item
	    ((name just-test-runner))
	    (emt:testral:make-report
	       :testrun-id (emtg (type testrun-id)(role original-add))
	       :tester-id "0"
	       :test-id-prefix ()
	       :suites (list
			  (list 
			     (emtg (type explorable)(name just-test-runner))
			     () 
			     (emtg (type test-runner-info))))))

	 ;;Report one suite, no children
	 (item
	    (
	       (role original-add)
	       (what-test test-1))
	    (emt:testral:make-report
	       :testrun-id (emtg (type testrun-id)(role original-add))
	       :tester-id "0"
	       :test-id-prefix ()
	       :suites (list
			  (list 
			     (emtg (type explorable)(what-test test-1))
			     ()
			     (emtg (type suite)(what-test test-1)(role original-add))))))
      
	 ;;A second report overriding the first
	 (item
	    (
	       (role replace)
	       (what-test test-1))
	    (emt:testral:make-report
	       :testrun-id (emtg (type testrun-id)(role replace))
	       :tester-id "0" 
	       :test-id-prefix ()
	       :suites (list
			  (list
			     (emtg (type explorable)(what-test test-1))
			     ()
			     (emtg (type suite)(what-test test-1)(role replace))))))

	 ;;A report removing the first report
	 (item
	    (
	       (role remove-previous)
	       (what-test test-1))
	    (emt:testral:make-report
	       :testrun-id (emtg (type testrun-id)(role remove-previous))
	       :tester-id "0" 
	       :test-id-prefix ()
	       :suites (list
			  (list
			     (emtg (type explorable)(what-test test-1))
			     ()
			     (emtg (type suite)(what-test test-1)(role remove-previous))))))
      

	 ;;A report adding to the first (same testrun-id)
	 (item
	    (
	       (role original-add)
	       (what-test test-2))
	    (emt:testral:make-report
	       :testrun-id (emtg (type testrun-id)(role original-add))
	       :tester-id "0"
	       :test-id-prefix ()
	       :suites (list
			  (list 
			     (emtg (type explorable)(what-test test-2))
			     ()
			     (emtg (type suite)(what-test test-2))))))

	 ;;Maybe add:
	 ;;A report using a prefix, otherwise same as the first.
	 ;;A report with a different tester-id (same testrun-id?)
	 )
   


      ))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/testral-types/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; testral-types.el ends here
