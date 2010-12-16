;;;_ emtest/common/persist/tests.el --- Test code for emt-persist

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: maint

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

(require 'emtest/common/persist/testhelp)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/deep-type-checker)
(require 'el-mock)

;;;_. Body
;;;_ , Interface for using results
;;;_  . emt:db:view:extract+placeholder
;;;_  . emt:db:view:extract

;;;_   , Tests

;;;_  . emtdb:view:view-obj

;;;_   , Tests
;;Still in emt-persist

;;;_  . emt:db:view:accept-correct

;;;_   , Tests
;;Still in emt-persist


;;;_ , Interface for testing with persists
;;;_  . Example objects

;;(type placeholder)(subtype archive)
;;;_  . Error type emt:already-handled

;;;_  . emt:funcall-handle-persistence-x


;;;_ , Interface for tester helpers
;;;_  . emt:db:get-versions
;;;_   , Tests
(emt:deftest-3 emt:db:get-versions
   (nil
      (progn
	 (emt:doc "Situation: ID has no versions")
	 (emt:doc "")
	 (emt:doc "Response: Return the empty list.")
	 (emtg:with emt:persist:thd:examples
	    ((project emtest)
	       (library persist)
	       (count 0))
	    (with-mock
	       (stub emt:db:by-ix:get-record =>
		  (emtg
		     (type emt:db:persist-archive)))
	       (equal
		  (emt:db:get-versions
		     (emtg
			(type id)))
		  (emtg
		     (type versions)))))))
   (nil
      (progn
	 (emt:doc "Situation: ID has 1 version")
	 (emt:doc "")
	 (emt:doc "Response: Return a list of that version.")
	 (emtg:with emt:persist:thd:examples
	    ((project emtest)
	       (library persist)
	       (count 1))
	    (with-mock
	       (stub emt:db:by-ix:get-record =>
		  (emtg
		     (type emt:db:persist-archive)))
	       (equal
		  (emt:db:get-versions
		     (emtg
			(type id)))
		  (emtg
		     (type versions))))))))

;;;_  . emt:db:get-all-values
;;;_   , Tests
(emt:deftest-3 emt:db:get-all-values
   (nil
      (progn
	 (emt:doc "Situation: ID has no versions")
	 (emt:doc "")
	 (emt:doc "Response: Return the empty list.")
	 (emtg:with emt:persist:thd:examples
	    ((project emtest)
	       (library persist)
	       (count 0))
	    (with-mock
	       (stub emt:db:by-ix:get-record =>
		  (emtg
		     (type emt:db:persist-archive)))
	       (equal
		  (emt:db:get-all-values
		     (emtg
			(type id)))
		  (emtg
		     (type values)))))))
   (nil
      (progn
	 (emt:doc "Situation: ID has 1 version")
	 (emt:doc "")
	 (emt:doc "Response: Return a list of that version.")
	 (emtg:with emt:persist:thd:examples
	    ((project emtest)
	       (library persist)
	       (count 1))
	    (with-mock
	       (stub emt:db:by-ix:get-record =>
		  (emtg
		     (type emt:db:persist-archive)))
	       (equal
		  (emt:db:get-all-values
		     (emtg
			(type id)))
		  (emtg
		     (type values))))))))

;;;_  . emt:db:get-value
;;;_   , Tests
;;It's direct.
;;;_  . emt:db:set

;;;_   , Tests
(emt:deftest-3 emt:db:set
   (nil
      (progn
	 (emt:doc "Operation: In a known database, save a value, then read it.")
	 (emt:doc "Response: The object has been added.
It has the correct value.")
	 (emt:db:set:th
	    (:initial-db nil)
	    ((id "1"))
	    (emt:db:set id 'correct-answer 12)
	    ;;Check database's type
	    (emty:check
	       (emt:db:internal:get-all id)
	       emt:db:whole)
	 ;;Can't use `emt:db:get-value' because that wants to know a
	 ;;version id.  But there's only one entry, so see it.
	    (let
	       ((value-list
		   (emt:db:get-all-values id 'correct-answer)))
	    ;;It was empty and now has one element.  Single element
	    ;;would not be removed (say by expiration).
	       (emt:assert
		  (=
		     (length value-list)
		     1))
	       (emt:assert
		  (=
		     (car value-list)
		     12))
	       t)))))


;;;_  . emt:db:decategorize-all
;;;_   , Tests
(emt:deftest-3 emt:db:decategorize-all
   '(nil
       (progn
	  (emt:doc "Situation: There are none in the category.")
	  (emt:doc "Response: Database does not change.")
	  (progn)))
   '(nil
       (progn
	  (emt:doc "Situation: There is one in the category")
	  (emt:doc "Response: It gets demoted to the parent.")
	  (progn)))
   '(nil
       (progn
	  (emt:doc "Situation: There is one in a subcategory")
	  (emt:doc "Response: It gets demoted to the parent.")
	  (progn))))

;;;_  . emt:db:change-cat
;;;_   , Tests
(emt:deftest-3 emt:db:change-cat
   '(nil
       (progn
	  (emt:doc "Situation: The respective version has some category")
	  (emt:doc "Response: It has the new category.")
	  (progn))))


;;;_ , Use-categories interface
;;;_  . emt:db:use:subtype-of
;;;_   , Tests
(emt:deftest-3 emt:db:use:subtype-of
   (nil
      (progn
	 (emt:doc "Params: A child and its parent")
	 (emt:doc "Response: Return non-nil.")
	 (and
	    (emt:db:use:subtype-of 'correct-answer 'correct-type)
	    t)))
   (nil
      (progn
	 (emt:doc "Params: A parent and its child")
	 (emt:doc "Response: Return nil.")
	 (not
	    (emt:db:use:subtype-of 'correct-type 'correct-answer)))))


;;;_  . emt:db:use:parent-of
;;;_   , Tests
;;It's direct

;;;_ , Persist object types interface

;;;_  . Singletons
;;;_   , emt:db:single:get-value
;;;_    . Tests

(emt:deftest-3 emt:db:single:get-value
   ;;Right now it's direct.  Later we may support indirection thru
   ;;`find-value', and then will need tests for both situations.
   '(nil
       (progn
	  (emt:doc "Situation: find-value field is `nil'")
	  (emt:doc "Response: Just return the value field.")
	  (emtg:with emt:persist:thd:examples
	     ((project emtest)
		(library persist)
		(count 0))
	     ;;$$REWRITE ME Use testpoint instead.
	     (with-mock
		(stub emt:db:by-ix:get-record =>
		   (emtg
		      (type emt:db:persist-archive))))))))



;;;_   , emt:db:single:get-notes
;;;_   , emt:db:single:get-category
;;;_   , emt:db:single:set-category
;;;_   , emt:db:single:ctor
;;;_  . persist-archives
;;;_   , emt:db:persist-archive:ctor
;;;_ , singletons wrt the whole database
;;;_  . emt:db:single:create

;;;_ , persist-archives as containers
;;;_  . emt:db:persist-archive:get-single

;;;_  . emt:db:persist-archive:ctor
;;;_  . emt:db:persist-archive:map
;;;_  . emt:db:persist-archive:create-single
;;;_  . emt:db:persist-archive:update-single
;;;_ , persist-archives in the alist
;;These just wrap "emt:db:whole" functions
;;;_  . emt:db:by-ix:get-record
;;;_  . emt:db:by-ix:create-record (Obsolete)
;;;_  . emt:db:by-ix:update-record (Obsolete)
;;;_ , Functions about records in the db
;;;_  . emt:db:whole:add-record
;;;_  . emt:db:whole:get-record
;;;_  . emt:db:whole:update-record
;;;_ , Access to the database
;;;_  . emt:db:internal:with-db
;;;_  . Test supporter
;;Test supporter does not belong in t file.  It exists for callers'
;;testing benefit.
;;;_   , emt:db:internal:ts:mock
;;;_ , The database itself
;;;_  . emt:db:internal:get-all
;;;_  . emt:db:internal:set-all
;;;_  . emt:db:internal:acquire-lock
;;;_  . emt:db:internal:release-lock


;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/persist/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/persist/tests.el ends here
