;;;_ common/testral-types/testhelp.el --- Examples of TESTRAL types
;;$$MOVE ME to ~/projects/emtest/lisp/testral-types/testhelp.el
;;Change all callers
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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

(require 'common/result-types)
(require 'tester/testhelp/eg)
(require 'tester/testhelp/match)
(require 'tester/testral)

;;;_. Body
;;;_ , Preliminary
;;;_  . Pattern ctor for emt:view:suite

;;Obsolescent
(emtm:define-struct-governor emt:view:suite
   ;;We leave out `display-info' since we can't expect to compare
   ;;against it meaningfully.
   name full-id unique-id suite child-type children sum-badnesses )

(emtm:define-struct-governor emt:view:testral
   ;;We leave out `display-info' since we can't expect to compare
   ;;against it meaningfully.
   main end args children child-type)

(emtm:define-struct-governor emtvr:suite-newstyle
   id
   how-to-run
   presentation-path
   testrun-id
   suite)

;;;_ , Examples
(emt:eg:define xmp:4c2dde89-a458-4cc7-a148-3725d8e7d692
   ((project emtest)(sub-project testral)(library types))
   ;;I don't like having `name' here but it is currently needed.  It
   ;;will get in the way when name parallelism is wanted for some
   ;;things later.
   (transparent-tags () (type subtype name role what-test))
   (group
      ;; Notes alone
      ((type note)(subtype alone))
      (type-must-be () emt:testral:alone)
      (item ((name error-1))
	 (make-emt:testral:error-raised
	    :badnesses '(ungraded)
	    :err
	    ;;We ct it from the actual error form, so we can't get out
	    ;;of sync with it.
	    (emt:eg:see-err
	       (emt:eg:value 
		:narrow ((name error-1)(type test-form))
		:ignore-tags (subtype))))))
   (group
      ;; Scoped notes
      ((type note)(subtype scoped))
      (group ((what checks))  ;;Want better tagname
	 (group ((result pass))  ;;Want better tagname
	    (type-must-be () (or emt:testral:push emt:testral:pop))

	    ;;(item ((id)) 100)
	    ;;(item ((parent-id)) nil)
	    (item ((name push))
	       (make-emt:testral:check:push
		  :id 100
		  :parent-id nil
		  :info ()
		  :fenceposting nil))
	    (item ((name pop))
	       (make-emt:testral:check:pop
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
	 (emt:eg (type test-form)(name error-1)))

      )

   (group
      ;; TESTRAL
      ((type testral-note-list)(subtype unconformed))
      (type-must-be () emt:testral:note-list)
      (item
	 ((name empty))
	 (make-emt:testral:note-list
	    :notes
	    (list)))
      
      ;;Maybe alias this as (name error-1)
      (item
	 ((name one-node))
	 (make-emt:testral:note-list
	    :notes
	    (list
	       (emt:eg (type note)(subtype alone)(name error-1))))))
   
   ;;OBSOLETE This design has changed
   ;;Conformed lists: They are bookended by stages.  Top level is
   ;;stages.  Perhaps other properness conformances.
;;    (group
;;       ;; TESTRAL
;;       ((type testral-note-list)(subtype conformed))
;;       (type-must-be () (repeat emt:testral:base))
;;       ;;TBD what we do in case of an empty list.
;; ;;       (item
;; ;; 	 ((name empty))
;; ;; 	 (list))
      
;;       (item
;; 	 ((name one-node))
;; 	 (list
;; 	    (make-emt:testral:stage:push :name "main")
;; 	    (emt:eg (type note)(subtype alone)(name error-1))
;; 	    (make-emt:testral:stage:pop :badnesses ()))))
   
;;    ;;OBSOLETE This design has changed
;;    (group
;;       ((type testral-tree))
      
;;       ;;`emt:view:testral' trees corresponding to TESTRAL lists.  The
;;       ;;`name' tag corresponds.

;;       ;;$$CHANGEME These should be patterns so they don't try to match
;;       ;;on the display field.

;;       ;;To be used in TESTRAL-expanded view trees below
;;       (group
;; 	 ;;Fully-expanded representation
;; 	 ((expansion full))
;; 	 ;;(transparent-tags () (expansion))
;; 	 (item
;; 	    ((name one-node))
;; 	    (let
;; 	       ((note-list 
;; 		   (emt:eg:value 
;; 		      :narrow 
;; 		      ((type testral-note-list)(subtype conformed))
;; 		      :ignore-tags (expansion))))
;; 	       (make-emt:view:testral
;; 		  :main (nth 0 note-list)
;; 		  :end  (nth 2 note-list)
;; 		  :args ()
;; 		  :child-type 'scoped
;; 		  :children 
;; 		  (list
;; 		     (make-emt:view:testral
;; 			:main (nth 1 note-list)
;; 			:end   nil
;; 			:args ()
;; 			:child-type nil
;; 			:children ()))))))
;;       (group
;; 	 ;;Representation expanded just one ply
;; 	 ((expansion 1))
;; 	 ;;(transparent-tags () (expansion))
;; 	 (item
;; 	    ((name one-node))
;; 	    (let
;; 	       ((note-list 
;; 		   (emt:eg:value 
;; 		      :narrow 
;; 		      ((type testral-note-list)(subtype conformed))
;; 		      :ignore-tags (expansion))))
;; 	       (make-emt:view:testral
;; 		  :main (nth 0 note-list)
;; 		  :end  (nth 2 note-list)
;; 		  :args ()
;; 		  :child-type 'tails
;; 		  :children 
;; 		  (list
;; 		     (nthcdr 1 note-list))))))
;;       )

   (group
      ;; Suites
      ((type suite))
      (type-must-be () emt:testral:suite)
      (item
	 ((name test-bad))
	 (make-emt:testral:suite
	    :contents (emt:eg 
			 (type testral-note-list)
			 (subtype unconformed)
			 (name one-node))
	    :badnesses '(ungraded)
	    :info ()))
      
      (item
	 ((name test-passes))
	 (make-emt:testral:suite
	    :contents (emt:eg 
			 (type testral-note-list)
			 (subtype unconformed)
			 (name empty))
	    :badnesses '()
	    :info ()))
      
      ;;Suite reporting child suites (here, child is just an indexed
      ;;clause)
      (item
	 ((name has-children-1))
	 (make-emt:testral:suite
	    :contents
	    ;;$$CHANGED recently
	    (make-emt:testral:runform-list
	       :els
	       (list 
		  (make-emt:test-ID:e-n:indexed-clause
		     :suite-sym 'first-suite
		     :clause-index 0)))
	    :badnesses '(ungraded)
	    :info ()))

      ;;For the sequence-of-reports tests (These alias other suite items)
      (item
	 ((what-test test-1)(role original-add))
	 (emt:eg (type suite)(name test-bad)))
      (item
	 ((what-test test-1)(role replace))
	 (emt:eg (type suite)(name test-passes)))

      (item
	 ((what-test test-1)(role remove-previous))
	 (make-emt:testral:suite
	    :contents (emt:eg 
			 (type testral-note-list)
			 (subtype unconformed)
			 (name empty))
	    ;;$$REVIEWME This representation is tentative.
	    :badnesses (list
			  '(bad-before-test not-found))
	    :info ()))
      
      (item
	 ((what-test test-2))
	 (emt:eg (type suite)(name test-passes))))
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
	    (emt:eg (type presentation-name)(what-test test-1))))
      (item
	 ((what-test test-2))
	 (cons "0"
	    (emt:eg (type presentation-name)(what-test test-2)))))

   (group
      ;; Test-runner info
      ((type test-runner-info))
      (type-must-be () emt:testral:test-runner-info)
      (item
	 ()
	 (make-emt:testral:test-runner-info
	    :name "My test runner")))
   (group
      ((type how-to-run))
      (item ((name test-1) ;;`name' seems redundant here now.
	       (what-test test-1))
	 (make-emt:test-ID:e-n:suite :suite-ID 'test-1))
      (item ((name test-2)
	       (what-test test-2))
	 (make-emt:test-ID:e-n:suite :suite-ID 'test-2)))
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
   
   (group
      ;; Report
      ((type report))
      (type-must-be () emt:testral:report)
      (item
	 ((name empty))
	 (make-emt:testral:report
	    :testrun-id (emt:eg (type testrun-id)(role original-add))
	    :tester-id "0"
	    :test-id-prefix ()
	    :suites (list)))

      ;;Just-test-runner report
      (item
	 ((name just-test-runner))
	 (make-emt:testral:report
	    :testrun-id (emt:eg (type testrun-id)(role original-add))
	    :tester-id "0"
	    :test-id-prefix ()
	    :suites (list
		       (list 
			  (make-emt:test-ID:e-n:hello)
			  () 
			  (emt:eg (type test-runner-info))))))

      ;;Report one suite, no children
      (item
	 (
	    (role original-add)
	    (what-test test-1))
	 (make-emt:testral:report
	    :testrun-id (emt:eg (type testrun-id)(role original-add))
	    :tester-id "0"
	    :test-id-prefix ()
	    :suites (list
		      (list 
			 (emt:eg (type how-to-run)(what-test test-1))
			 (emt:eg (type presentation-name)(what-test test-1))
			 (emt:eg (type suite)(what-test test-1)(role original-add))))))
      
      ;;A second report overriding the first
      (item
	 (
	    (role replace)
	    (what-test test-1))
	 (make-emt:testral:report
	    :testrun-id (emt:eg (type testrun-id)(role replace))
	    :tester-id "0" 
	    :test-id-prefix ()
	    :suites (list
		       (list
			  (emt:eg (type how-to-run)(what-test test-1))
			  (emt:eg (type presentation-name)(what-test test-1))
			  (emt:eg (type suite)(what-test test-1)(role replace))))))

      ;;A report removing the first report
      (item
	 (
	    (role remove-previous)
	    (what-test test-1))
	 (make-emt:testral:report
	    :testrun-id (emt:eg (type testrun-id)(role remove-previous))
	    :tester-id "0" 
	    :test-id-prefix ()
	    :suites (list
		       (list
			  (emt:eg (type how-to-run)(what-test test-1))
			  (emt:eg (type presentation-name)(what-test test-1))
			  (emt:eg (type suite)(what-test test-1)(role remove-previous))))))
      

      ;;A report adding to the first (same testrun-id)
      (item
	 (
	    (role original-add)
	    (what-test test-2))
	 (make-emt:testral:report
	    :testrun-id (emt:eg (type testrun-id)(role original-add))
	    :tester-id "0"
	    :test-id-prefix ()
	    :suites (list
		      (list 
			 (emt:eg (type how-to-run)(what-test test-2))
			 (emt:eg (type presentation-name)(what-test test-2))
			 (emt:eg (type suite)(what-test test-2))))))

      ;;Maybe add:
      ;;A report using a prefix, otherwise same as the first.
      ;;A report with a different tester-id (same testrun-id?)
      )
   
   
   (group
      ((type receive-alist-item))
      (type-must-be () (emtm:pattern emtvr:suite-newstyle))
      (item
	 ( (role original-add)
	    (what-test test-1))
	 (emtm:make-pattern
	    (make-emtvr:suite-newstyle
	       :suite 
	       (eval
		  '(emt:eg (type suite)(what-test test-1)(role original-add)))
	       :how-to-run
	       (eval 
		  '(emt:eg (type how-to-run)(name test-1)))
	       :id
	       (eval 
		  '(emt:eg (type how-to-run)(name test-1)))
	       :presentation-path
	       (eval 
		  '(emt:eg (type presentation-path)(what-test test-1)))
	       :testrun-id 
	       (eval 
		  '(emt:eg (type testrun-id)(role original-add))))))

      (item
	 ( (role replace)
	    (what-test test-1))
	 (emtm:make-pattern
	    (make-emtvr:suite-newstyle
	       :suite 
	       (eval
		  '(emt:eg (type suite)(what-test test-1)(role replace)))
	       :how-to-run
	       (eval 
		  '(emt:eg (type how-to-run)(name test-1)))
	       :id
	       (eval 
		  '(emt:eg (type how-to-run)(name test-1)))
	       :presentation-path
	       (eval 
		  '(emt:eg (type presentation-path)(what-test test-1)))
	       :testrun-id 
	       (eval 
		  '(emt:eg (type testrun-id)(role replace))))))

      )

   )

;;;_. Footers
;;;_ , Provides

(provide 'common/testral-types/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; testral-types.el ends here
