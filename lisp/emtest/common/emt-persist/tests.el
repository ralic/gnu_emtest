;;;_ emtest/common/emt-persist/tests.el --- Test code for emt-persist

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

(require 'emtest/common/emt-persist/testhelp)
(require 'emtest/testhelp/eg)
(require 'emtest/testhelp/deep-type-checker)
(require 'el-mock)

;;;_. Body
;;;_ , Interface for using results
;;;_  . emt:extract-got+placeholder
;;;_   , Tests
'  ;;It's obsolete
(rtest:deftest emt:extract-got+placeholder

   ;;See below for the example definitions

   (  "Param:  A diag call to a comparison that used persist.
Response: Returns the (got placeholder) pair."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (let*
	    ((db-index 
		(emt:eg (type version-placeholder)))
	       (diag-call
		  (make-emt:result:diag:call
		     :info-about
		     ()
;; 		     (list
;; 			(make-emt:result:info-about:tried-persist
;; 			   :unique t
;; 			   :placeholder db-index
;; 			   :placeholder-ix 2))
		     :tried
		     (list
			(make-emt:result:diag:tried-persist-version.
			   :placeholder db-index))
		     :status    t
		     :call-sexp (list 'equal 1 db-index))))
	    (assert
	       (equal
		  (emt:extract-got+placeholder diag-call)
		  (list 1 db-index))
	       t)
	    t)))
   
   
   (  "Param: A diag call to a comparison that did not use persist -
the `tried' field is an empty list.
Response: Returns nil."
      (let*
	 (
	    (diag-call
	       (make-emt:result:diag:call
		  :status    nil
		  :call-sexp (list 'equal 1 2))))
	 (assert
	    (equal
	       (emt:extract-got+placeholder diag-call)
	       nil)
	    t)
	 t))
   
   (  "Param:  A diag call to a comparison that used persist.
The functor is something other than #'equal
Response: (For now.  This will change) Error."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (let*
	    ((db-index (emt:eg (type version-placeholder)))
	       (diag-call
		  (make-emt:result:diag:call
		     :info-about
		     (list
			(make-emt:result:info-about:tried-persist
			   :unique t
			   :placeholder db-index
			   :placeholder-ix 2))
		     :status    t
		     :call-sexp (list 'rtest:sets= 1 db-index))))
	    (assert
	       (emt:gives-error
		  (emt:extract-got+placeholder diag-call))
	       t)
	    t)))

   (  "Param: The `result-diag' type from `(section persist-viewer)'
Response: WRITEME."
      (emt:eg:narrow 
	 ;;This would usually have (library persist) but this part
	 ;;merges persist and viewer functionality (though viewer is
	 ;;not yet using these examples)
	 ((project emtest)(section persist-viewer))
	 (emt:db:internal:ts:mock (emt:eg (type versions))
	    (assert
	       (equal
		  (emt:extract-got+placeholder (emt:eg (type result-diag)))
		  (list
		     (emt:eg (type data)(name got))
		     (emt:eg (type archive-placeholder))))
	       t)
	    t)))
   
   )

	    
;;;_  . emt:extract-got

;;;_   , Tests

;;;_  . emt:persist:view-obj

;;;_   , Tests
;;Still in emt-persist

;;;_  . emt:persist:accept-correct

;;;_   , Tests
;;Still in emt-persist


;;;_ , Interface for testing with persists
;;;_  . Example objects

;;(type placeholder)(subtype archive)
;;;_  . Error type emt:already-handled

;;;_  . emt:funcall-handle-persistence-x
;;;_   , Test supporter
;;$$Obsolete.  Change me to use testpoint instead.  Change the sense
;;to conform to the new persist design.
(defmacro emt:funcall-handle-persistence-x:th:values (values &rest body)
   ""
   
   `(with-mock
       (mock
	  (emt:db:get-versions * *) =>
	  (mapcar
	     #'(lambda (v)
		  (make-emt:db:single
		     ;;This helper doesn't try to set the other fields.
		     :value v))
	     ,values))
       ,@body))


;;;_   , Tests
;;OBSOLESCENT
'
(rtest:deftest emt:funcall-handle-persistence-x
   ;;These must stub something else
   (  "Situation: One arg is a persistence placeholder.
It doesn't exist in the database.
One Response: Raises error of type `emt:already-handled'."
      (emt:eg:with emt:persist:with 
	 ((project emtest)(library tester)(section emt:funcall))
	 (emt:funcall-handle-persistence-x:th:values ()
	    (let
	       ((db-index (emt:eg (type archive-placeholder))))
	       (assert
		  (emt:gives-error
		     (emt:funcall #'equal 1 db-index)
		     emt:already-handled))
	       t))))
   


   (  "Situation: One arg is a persistence placeholder.
It doesn't exist in the database.
One response: Reports error.
Stores a diag trace with `unique' field = `none-found'."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (emt:funcall-handle-persistence-x:th:values ()
	    (let
	       ((db-index (emt:eg (type archive-placeholder))))
	       
	       (destructuring-bind 
		  (retval diag &key status info-about tried-list)
		  (emt:funcall:th:3  #'equal 1 db-index)

		  (assert
		     (= (length tried-list) 1) t)
		  (let
		     ((pers (car tried-list)))
		     (check-type pers
			emt:result:diag:tried-persist-archive.))

		  t)))))


   (  "Situation: One arg is a persistence placeholder.
In the database, it gives more than 1 answer marked `correct'.
One response: Raises error of type `emt:already-handled'."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (emt:funcall-handle-persistence-x:th:values '(13 143)
	    (let
	       ((db-index (emt:eg (type archive-placeholder))))
	       (assert
		  (emt:gives-error
		     (emt:funcall #'equal 1 db-index)
		     emt:already-handled))
	       t))))
   
   (  "Situation: One arg is a persistence placeholder.
In the database, it gives more than 1 answer marked `correct'.
Response: Reports error and raises error of type
`emt:already-handled'."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (emt:funcall-handle-persistence-x:th:values '(13 143)
	    (let
	       ((db-index (emt:eg (type archive-placeholder))))

	       (destructuring-bind
		  (retval diag &key status info-about tried-list)
		  (emt:funcall:th:3  #'equal 1 db-index)


		  (let
		     ()
		     (assert
			(= (length tried-list) 1) t)

		     (let
			((pers (car tried-list)))
			(check-type pers
			   emt:result:diag:tried-persist-archive.)))

		  t)))))




   (  "Situation: One arg is a persistence placeholder.
It exists in the database and the only value matches
Response: Reports success and returns non-nil.
Stores a diag trace with `unique' field = `t'."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (emt:funcall-handle-persistence-x:th:values '(1)
	    (let
	       ((db-index (emt:eg (type archive-placeholder))))
	    
	       (destructuring-bind 
		  (retval diag &key status info-about tried-list)
		  (emt:funcall:th:3  #'equal 1 db-index)


		  (let
		     ()
		     (assert
			(= (length tried-list) 1) t)
		     (let
			((pers (car tried-list)))
			(check-type pers
			   emt:result:diag:tried-persist-version.)))

		  t)))))
   
   
   (  "Situation: One arg is a persistence placeholder.
It exists in the database and the only value mismatches
Response: Reports failed and returns nil.
Stores a diag trace with `unique' field = `t'."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (emt:funcall-handle-persistence-x:th:values '(13)
	    (let
	       ((db-index (emt:eg (type archive-placeholder))))
	    
	       (destructuring-bind 
		  (retval diag &key status info-about tried-list)
		  (emt:funcall:th:3  #'equal 1 db-index)

		  (let
		     ()
		     (assert
			(= (length tried-list) 1) t)

		     (let
			((pers (car tried-list)))
			(check-type pers
			   emt:result:diag:tried-persist-version.)))

		  t)))))

   ;;$$Add: The persist arg is somewhere other than position B.


   ;;After here, not enabled yet.  These need a stronger test-helper
   ;;that reacts to use-category.
   
   '
   (  "Situation: One arg is a persistence placeholder.
It exists in the database. 
The only value matches but relates as `wrong-answer'
Response: Reports failure and returns nil."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (with-mock
	    (mock (emt:db:get-all-values * 'wrong-answer) => '(1))

	    (let
	       ((db-index (emt:eg (type archive-placeholder))))
	    
	       (destructuring-bind (retval report)
		  (emt:funcall:th () emt:report-control:thd:report-all
		     (rtest:gives-error
			(emt:funcall #'equal 1 db-index)))
		  ;;(should null retval)
		  (equal 
		     report
		     (list
			(make-emt:result:diag:call
			   :status    t
			   :call-sexp (list 'equal 1 db-index)))))))))
   
   '
   (  "Situation: One arg is a persistence placeholder.
It exists in the database.
The only value mismatches but relates as `wrong-answer'
Response: Reports error and throws error."
      (emt:eg:with emt:persist:thd:funcall-examples
	 ((project emtest)(library tester)(section emt:funcall))
	 (with-mock
	    (mock (emt:db:get-all-values * 'wrong-answer) => '(13))

	    (let
	       ((db-index (emt:eg (type archive-placeholder))))
	    
	       (destructuring-bind (retval report)
		  (emt:funcall:th () emt:report-control:thd:report-all
		     (rtest:gives-error
			(emt:funcall #'equal 1 db-index)))
		  ;;(should error-p-this-predicate-doesnt-exist retval)
		  (equal 
		     report
		     (list
			(make-emt:result:diag:call
			   :status    error
			   :call-sexp (list 'equal 1 db-index)))))))))
   
   
   '
   (  "Situation: One arg is a persistence placeholder.
Several values in the database.  
One relates as `correct-answer'.  It matches.
Response: Reports success and returns non-nil."
      (progn) ;;Test-form
      )

   '
   (  "Situation: One arg is a persistence placeholder.
Several values in the database.  
One relates as `correct-answer'.  It mismatches.
Response: Reports failure and returns nil."
      (progn) ;;Test-form
      )

   '
   (  "Situation: One arg is a persistence placeholder.
Several values in the database.  
All relate as `wrong-answer'.  One matches.
Response: Reports failure and returns nil."
      (progn) ;;Test-form
      )

   '
   (  "Situation: One arg is a persistence placeholder.
Several values in the database.  
All relate as `wrong-answer'.  None matches.
Response: Reports error and throws error."
      (progn) ;;Test-form
      )

   )


;;;_ , Interface for tester helpers
;;;_  . emt:db:get-versions
;;;_   , Tests
(rtest:deftest emt:db:get-versions

   (  "Situation: ID has no versions
Response: Return the empty list."
      (emt:eg:with emt:persist:thd:examples
	 ((project emtest)(library persist)(count 0))
	 (with-mock
	    (stub emt:db:by-ix:get-record =>
	       (emt:eg (type emt:db:persist-archive)))
	    (equal
	       (emt:db:get-versions (emt:eg (type id)))
	       (emt:eg (type versions))))))
   
   (  "Situation: ID has 1 version
Response: Return a list of that version."
      (emt:eg:with emt:persist:thd:examples
	 ((project emtest)(library persist)(count 1))
	 (with-mock
	    (stub emt:db:by-ix:get-record =>
	       (emt:eg (type emt:db:persist-archive)))
	    (equal
	       (emt:db:get-versions (emt:eg (type id)))
	       (emt:eg (type versions))))))

   ;;Want more tests. Add after emt:eg:define is stronger

   )
;;;_  . emt:db:get-all-values
;;;_   , Tests
(rtest:deftest emt:db:get-all-values

   (  "Situation: ID has no versions
Response: Return the empty list."
      (emt:eg:with emt:persist:thd:examples
	 ((project emtest)(library persist)(count 0))
	 (with-mock
	    (stub emt:db:by-ix:get-record =>
	       (emt:eg (type emt:db:persist-archive)))
	    (equal
	       (emt:db:get-all-values (emt:eg (type id)))
	       (emt:eg (type values))))))
   
   (  "Situation: ID has 1 version
Response: Return a list of that version."
      (emt:eg:with emt:persist:thd:examples
	 ((project emtest)(library persist)(count 1))
	 (with-mock
	    (stub emt:db:by-ix:get-record =>
	       (emt:eg (type emt:db:persist-archive)))
	    (equal
	       (emt:db:get-all-values (emt:eg (type id)))
	       (emt:eg (type values))))))
   
   )
;;;_  . emt:db:get-value
;;;_   , Tests
;;It's direct.
;;;_  . emt:db:set

;;;_   , Tests
(rtest:deftest emt:db:set
   (  "Operation: In a known database, save a value, then read it.
Response: The object has been added.
It has the correct value."

      (emt:db:set:th (:initial-db ())
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
	    (assert (= (length value-list) 1))
	    (assert
	       (= (car value-list) 12))
	    t)))
   
   )

;;;_  . emt:db:decategorize-all
;;;_   , Tests
(rtest:deftest emt:db:decategorize-all
   ;;Mock db seer and putter and read db after
   '
   (  "Situation: There are none in the category.
Response: Database does not change."
      (progn) ;;Test-form
      )
   '
   (  "Situation: There is one in the category
Response: It gets demoted to the parent."
      (progn) ;;Test-form
      )
   '
   (  "Situation: There is one in a subcategory
Response: It gets demoted to the parent."
      (progn) ;;Test-form
      )

   )
;;;_  . emt:db:change-cat
;;;_   , Tests
(rtest:deftest emt:db:change-cat
   ;;Mock db seer and putter and read db after, and then fetch the
   ;;category of that version-id
   '
   (  "Situation: The respective version has some category
Response: It has the new category."
      (progn) ;;Test-form
      )
   
   )

;;;_ , Use-categories interface
;;;_  . emt:db:use:subtype-of
;;;_   , Tests
(rtest:deftest emt:db:use:subtype-of

   (  "Params: A child and its parent
Response: Return non-nil."
      (and (emt:db:use:subtype-of 'correct-answer 'correct-type) t))
   
   (  "Params: A parent and its child
Response: Return nil."
      (not (emt:db:use:subtype-of 'correct-type 'correct-answer)))
      
   )

;;;_  . emt:db:use:parent-of
;;;_   , Tests
;;It's direct

;;;_ , Persist object types interface

;;;_  . Singletons
;;;_   , emt:db:single:get-value
;;;_    . Tests

(rtest:deftest emt:db:single:get-value
   ;;Right now it's direct.  Later we may support indirection thru
   ;;`find-value', and then will need tests for both situations.
   ;;
   '
   (  "Situation: find-value field is `nil'
Response: Just return the value field."
      (emt:eg:with emt:persist:thd:examples
	 ((project emtest)(library persist)(count 0))
	 (with-mock
	    (stub emt:db:by-ix:get-record =>
	       (emt:eg (type emt:db:persist-archive)))
	    ;;$$WRITE TEST BODY
	    
	    ))))


;;;_   , emt:db:single:get-notes
;;;_   , emt:db:single:get-category
;;;_   , emt:db:single:set-category
;;;_   , emt:db:single:ctor
;;;_  . persist-archives
;;;_   , emt:db:persist-archive:ctor
;;;_ , singletons wrt the whole database
;;;_  . emt:db:single:create
;;;_   , Tests
;;Tested thru `emt:db:set'

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

(provide 'emtest/common/emt-persist/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/emt-persist/tests.el ends here
