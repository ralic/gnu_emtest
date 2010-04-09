;;;_ emtest/common/emt-persist.el --- Persistence for emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest    (&rest dummy))
    (defmacro rtest:if-avail   (&rest dummy)))

;;The only backend supported at this time is persist.el.  
(require 'tinydb/persist)
(require 'emtest/common/result-types)

;;;_. Body
;;;_ , Structures
;;;_  . Placeholder objects
;;Moved [[file:~/projects/emtest/lisp/result-types.el::_%20NEW%20diagnostic%20info][here]]

;;;_  . Database objects
;;;_   , Db object, as in the database
(defstruct (emt:db:single
	      (:conc-name emt:db:single->))
   
   "One version of an object, as stored in this database"
   version-id ;;Same as `version-id' in `emt:db:version-index.'
   value      ;;Of arbitrary type
   find-value ;;Indicates any special means of finding the real value.
   (use-category () :type emt:persist:use-category)
   notes ;;explanation, goldenness, etc
   creation-time
   )

;;;_   , All use-versions of an object
(defstruct (emt:db:persist-archive
	      (:conc-name emt:db:persist-archive->))
   "All versions of an object, as stored in this database."
   (list () :type (repeat emt:db:single)))

;;;_  . Group index
(defstruct emt:db:group
   "An index to a group of records, as stored in this database."
   ;;Contents are TBD
   )

;;;_  . emt:db:record Database record
(deftype emt:db:record () 
   "This is the type of the record's cdr."
   '(or emt:db:persist-archive emt:db:group))
;;;_  . emt:db:record-alist
;;Just an alist element: (id . VALUE)
;;ID is the same type as `id' in `emt:db:id-index.'
(deftype emt:db:record-alist () 
   "Record alist in a database"
   '(repeat (cons * emt:db:record)))
;;;_  . The database, as seen by callers.
(defstruct (emt:db:whole
	      (:conc-name emt:db:whole->))
   "The database, as seen by callers"
   (list () :type emt:db:record-alist)
   (dirty () :type bool)
   mode)

;;;_ , Interface for using results
;;;_  . emt:extract-got

(defun emt:extract-got (diag-call arg-ix)
   ""
   (check-type diag-call emt:result:diag:call)
   (check-type arg-ix integer)
   (let
      (

	 (call-sexp
	    (emt:result:diag:call-call-sexp diag-call)))
      ;;For now, only handle `equal'
      (unless
	 (eq (car call-sexp) 'equal)
	 (error "Unrecognized functor %s" (car call-sexp)))
      (case arg-ix
	 (1 (third call-sexp))
	 (2 (second call-sexp))
	 (t (error "Argument %s is not a comparand"
	       arg-ix)))))

;;;_   , Tests
;;These are now the real tests
(put 'emt:extract-got 'rtest:test-thru
   'emt:persist:accept-correct)

;;;_  . emt:extract-got+exp-pair-list
'
(defun emt:extract-got+exp-pair-list (diag-call)
   ""
   (check-type diag-call emt:result:diag:call)
   (let
      ((call-sexp
	  (emt:result:diag:call-call-sexp diag-call)))
      ;;For now, only handle `equal'
      (unless
	 (eq (car call-sexp) 'equal)
	 (error "Unrecognized functor %s" (car call-sexp)))
      (list
	 (list (second call-sexp) (third call-sexp))
	 (list (third call-sexp) (second call-sexp)))))

;;;_   , Tests
'
(put 'emt:extract-got+exp-pair-list 'rtest:test-thru
   'emt:extract-got+placeholder)

;;;_  . emt:extract:pair-list->got
'
(defun emt:extract:pair-list->got (pair-list placeholder)
   ""
   (check-type placeholder emt:db:version-index.)

   (let*
      ((cell (assoc placeholder pair-list)))
      (when cell
	 (second cell))))

;;;_   , Tests
;;It's direct
;;;_  . emt:persist:view-obj
;;;###autoload
(defun emt:persist:view-obj (tried)
   ""
   (interactive
      (list 
	 (get-text-property (point) 'emt:diag:tried)))

   (unless
      (typep tried 'emt:result:diag:tried-persist-version.)
      (error "There is no object to view"))
   
   (let*
      ((buf
	  (generate-new-buffer "*Emtest view persist*"))
	 (placeholder
	    (emt:result:diag:tried-persist-version.-placeholder
	       tried))
	 (value
	    (emt:db:get-value placeholder)))
      (unless (stringp value) 
	 (error "Right now only supported for strings"))
      (with-current-buffer buf
	 (insert value))
      (pop-to-buffer buf)))

;;;_   , Tests
;;These are now the real tests

(rtest:deftest emt:persist:view-obj


   '
   (  "Manual test of `emt:persist:view-obj'."
      (emt:eg:narrow 
	 ((project emtest)(library persist)(section persist-viewer))
	 (emt:db:internal:ts:mock (emt:eg (type whole-db))
	    (emt:persist:view-obj
	       (emt:eg (type tried) (foundp t))))))
   ;;Very obsolete now.
   '
   (  "Manual test of `emt:persist:view-obj' ~in situ~."
      (emt:eg:narrow 
	 ((project emtest)(library persist)(section persist-viewer))
	 (require 't/emt-persist "t/emt-persist.el")
	 (require 'plain-viewer)
	 (emt:db:internal:ts:mock (emt:eg (type whole-db))
	    (emt:viewer:plain:ts
	       (emt:eg (type result-diag)(foundp t))
	       'emt:diag:tried

	       ;;Test body
	       (call-interactively #'emt:persist:view-obj)))))
   
   
   )
;;;_  . emt:persist:accept-correct
;;;###autoload
(defun emt:persist:accept-correct (call tried)
   ""

   (interactive
      (list
	 (get-text-property (point) 'emt:diag:call)
	 (get-text-property (point) 'emt:diag:tried)))
   
   (check-type call emt:result:diag:call)
   (check-type tried 
      (or 
	 emt:result:diag:tried-persist-version.
	 emt:result:diag:tried-persist-archive.))

   (let
      (
	 ;;`emt:result:diag:tried.' is the base type of either type of
	 ;;tried
	 (arg-ix 
	    (emt:result:diag:tried.-arg-ix tried))
	 (id 
	    (etypecase tried
	       (emt:result:diag:tried-persist-version.
		  (emt:db:version-index.-id-index
		     (emt:result:diag:tried-persist-version.-placeholder
			tried)))
	       
	       (emt:result:diag:tried-persist-archive.
		  (emt:result:diag:tried-persist-archive.-placeholder
		     tried)))))
      (check-type id emt:db:id-index.)

      (emt:db:set id 'correct-answer 
	 (emt:extract-got call arg-ix))))


;;;_   , Tests
;;These are now the real tests
(rtest:deftest emt:persist:accept-correct

   ;;Very obsolete now.
   (  "Test of `emt:persist:accept-correct' ~in situ~."
      (emt:eg:narrow 
	 ((project emtest)(library persist)(section persist-viewer))
	 (require 't/emt-persist "t/emt-persist.el")
	 (require 'plain-viewer)
	 (emt:db:internal:ts:mock ()
	    (emt:viewer:plain:ts
	       (emt:eg (type result-diag)(foundp nil))
	       'emt:diag:tried

	       (call-interactively #'emt:persist:accept-correct)
	       (assert
		  (equal
		     (emt:db:get-all-values 
			(emt:eg (type archive-placeholder) )
			'correct-answer)
		     (list
			(emt:eg (type data)(name got)))))
	       t))))
   
   )

;;;_ , Interface for testing with persists
;;;_  . Example objects
'
(emt:eg:define xmp:af39c81f-229e-4d30-84a3-7842123fba35
   ((project emtest)(library tester)(section emt:funcall))
   (transparent-tags () (type subtype))
   (item
      ((type placeholder)(subtype archive))
      (make-emt:db:id-index.
	 :id "a"
	 :backend '(persist "Dummy")))
   (item
      ((type placeholder)(subtype version))
      (make-emt:db:version-index.
	 :id-index (emt:eg (type placeholder)(subtype archive))
	 :version-id "v.1"))
   

   ;;A bit wobbly - careful of the symbol vs function-quoted symbol
   ;;distinction.
   (item
      ((type call-sexp))
      (list #'equal 
	 1 
	 (emt:eg (type placeholder)(subtype archive))))
   )


;;;_  . emt:persist:value

;;Let's keep `emt:persist' outside of here for the moment.  Also, Any
;;fancy rewrite-and-retry processing will occur outside of here,
;;possibly in a condition-case that knows which predicates that is
;;meaningful for.
(defun emt:persist:value (persist-id)
   ""
   (let
      (
	 (versions
	    (emt:db:get-versions persist-id 'correct-answer)))
      ;;Maybe make a note in any case.  Arg position will have already
      ;;been noted.
      (case
	 (length versions)
	 (0 
	    ;;For now, just errors.  
	    (error "No such persist was found: %s" persist-id))
	 
	 (1 
	    ;;Push another note indicating the value that we used
	    (emt:db:single:get-value (car versions)))
	 (t 
	    (error "Too many persist items were found: %s" persist-id)))))


;;;_  . emt:funcall-handle-persistence-x
;;OBSOLESCENT
(defun emt:funcall-handle-persistence-x (args)
   ""
   (catch 'emt:funcall-handle-persistence
      (let
	 ((persist-arg
	     (find nil args 
		;;This helper function doesn't look at its first argument
		:test
		#'(lambda (dummy x)
		     (emt:db:id-index.-p x)))))

	 (unless persist-arg 
	    (throw 'emt:funcall-handle-persistence
	       (list 'correct-answer args)))
	 
	 (let
	    (
	       (placeholder-ix
		  (1+
		     (position persist-arg args :test #'eq)))
	       (versions
		  (emt:db:get-versions persist-arg 'correct-answer)))
	    
	    (case
	       (length versions)
	       (0 
		  (push
		     (make-emt:result:diag:tried-persist-archive.
			:arg-ix       placeholder-ix
			:placeholder  persist-arg
			:use-category 'correct-answer
			:reason       'none-found)
		     emt:trace:tried)
		  (signal 'emt:already-handled ()))
	       (1 
		  (push
		     (make-emt:result:diag:tried-persist-version.
			:arg-ix       placeholder-ix
			:placeholder  (car versions))
		     emt:trace:tried)
		  (let
		     (
			(new-args
			   (substitute 
			      (emt:db:single:get-value (car versions)) 
			      persist-arg args)))
		     (throw 'emt:funcall-handle-persistence
			(list
			   'correct-answer 
			   new-args))))
	       (t 
		  (push
		     (make-emt:result:diag:tried-persist-archive.
			:arg-ix       placeholder-ix
			:placeholder  persist-arg
			:use-category 'correct-answer
			:reason       'too-many-found)
		     emt:trace:tried)
		  (signal 'emt:already-handled ())))))))


;;;_   , Tests
;;In t/emt-persist.el

;;;_ , Interface for tester helpers
;;;_  . emt:db:get-versions
(defun emt:db:get-versions (id &optional filter)
   ""
   ;;$$REPLACE ME
   ;;Use `tinydb-alist-update' with this filter as functionality.
   (let*
      ((x (emt:db:by-ix:get-record id t)))
      (remove*
	 filter
	 (emt:db:persist-archive->list x)
	 :test-not
	 #'(lambda (filter obj)
	      (emt:db:use:subtype-of
		 (emt:db:single->use-category obj)
		 filter)))))

;;;_   , Tests
'
(rtest:deftest emt:db:get-versions

   (  "Situation: ID has no versions
Response: Return the empty list."
      (emt:eg:narrow 
	 ((project emtest)(library persist)(count 0))
	 (with-mock
	    (stub emt:db:by-ix:get-record =>
	       (emt:eg (type emt:db:persist-archive)))
	    (equal
	       (emt:db:get-versions (emt:eg (type id)))
	       (emt:eg (type versions))))))
   
   (  "Situation: ID has 1 version
Response: Return a list of that version."
      (emt:eg:narrow 
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
;;Now mostly just a test helper
(defun emt:db:get-all-values (id &optional filter)
   ""
   (mapcar
      #'emt:db:single:get-value
      (emt:db:get-versions id filter)))

;;;_   , Tests
'
(rtest:deftest emt:db:get-all-values

   (  "Situation: ID has no versions
Response: Return the empty list."
      (emt:eg:narrow 
	 ((project emtest)(library persist)(count 0))
	 (with-mock
	    (stub emt:db:by-ix:get-record =>
	       (emt:eg (type emt:db:persist-archive)))
	    (equal
	       (emt:db:get-all-values (emt:eg (type id)))
	       (emt:eg (type values))))))
   
   (  "Situation: ID has 1 version
Response: Return a list of that version."
      (emt:eg:narrow 
	 ((project emtest)(library persist)(count 1))
	 (with-mock
	    (stub emt:db:by-ix:get-record =>
	       (emt:eg (type emt:db:persist-archive)))
	    (equal
	       (emt:db:get-all-values (emt:eg (type id)))
	       (emt:eg (type values))))))
   
   )
;;;_  . emt:db:get-value
(defun emt:db:get-value (version-placeholder)
   "Gets the value corresponding to VERSION-PLACEHOLDER."

   (let* 
      ((db-index
	  (emt:db:version-index.-id-index version-placeholder))
	 (version-id
	    (emt:db:version-index.-version-id version-placeholder))
	 (persist-archive
	    (emt:db:by-ix:get-record db-index)))
      (emt:db:single:get-value
	 (emt:db:persist-archive:get-single 
	    persist-archive
	    version-id))))



;;;_   , Tests
;;Tested thru `emt:persist:view-obj' (Tests are manual)



;;;_  . emt:db:set
(defun emt:db:set (id category object-value)
   ""
   (check-type category emt:persist:use-category)
   (check-type id emt:db:id-index.)

   (let
      ((obj 
	  (emt:db:single:ctor
	     :value object-value
	     :find-value nil  ;;For now, always `nil'
	     :use-category category
	     :notes ()  ;;For now, always empty.
	     :creation-time (current-time))))
      (emt:db:single:create obj id)))

;;;_   , Test helpers

;;With a mock database and with id bound neatly.
'
(defmacro* emt:db:set:th ((&keys initial-db) ((id-sym id)) &rest body)
   "
ID-SYM will be bound to a `emt:db:id-index.' as if by `let'"
   
   `(let
       ((,id-sym 
	   (make-emt:db:id-index.
	      :id ,id
	      :backend 'dummy)))

       (emt:db:internal:ts:mock ,initial-db
	  ,@body)))


;;;_   , Tests
'
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
(defun emt:db:decategorize-all (id filter)
   ""
   
   (let*
      ()
      
      ))
;;;_   , Tests
'
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
(defun emt:db:change-cat (version-id new-cat)
   ""
   
   (let*
      ()
      
      ))
;;;_   , Tests
'
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
(defun emt:db:use:subtype-of (a b)
   "Return non-nil if A is a subtype of B
A and B must be use-categories"
   (check-type a emt:persist:use-category)
   (check-type b emt:persist:use-category)
   (case a
      (correct-answer (memq b '(correct-answer correct-type nil)))
      (correct-type   (memq b '(correct-type nil)))
      (wrong-answer   (memq b '(wrong-answer nil)))
      (nil (eq b nil))))

;;;_   , Tests
'
(rtest:deftest emt:db:use:subtype-of

   (  "Params: A child and its parent
Response: Return non-nil."
      (and (emt:db:use:subtype-of 'correct-answer 'correct-type) t))
   
   (  "Params: A parent and its child
Response: Return nil."
      (not (emt:db:use:subtype-of 'correct-type 'correct-answer)))
      
   )

;;;_  . emt:db:use:parent-of
(defun emt:db:use:parent-of (a)
   "Return the parent use-category of use-category A"
   (check-type a emt:persist:use-category)
   (case a
      (correct-answer 'correct-type)
      (correct-type nil)
      (wrong-answer nil)
      (nil nil)))

;;;_   , Tests
;;It's direct

;;;_ , Persist object types interface

;;;_  . Singletons
;;;_   , emt:db:single:get-value

(defalias 'emt:db:single:get-value 'emt:db:single->value)
;;;_    . Tests

'
(rtest:deftest emt:db:single:get-value
   ;;Right now it's direct.  Later we may support indirection thru
   ;;`find-value', and then will need tests for both situations.
   ;;
   )


;;;_   , emt:db:single:get-notes
(defalias 'emt:db:single:get-notes 'emt:db:single->notes)
;;;_   , emt:db:single:get-category
(defalias 'emt:db:single:get-category 'emt:db:single->use-category)

;;;_   , emt:db:single:set-category
(defun emt:db:single:set-category (obj new-category)
   ""
   (setf (emt:db:single->use-category obj) new-category))

;;;_   , emt:db:single:ctor
(defalias 'emt:db:single:ctor 'make-emt:db:single)
;;;_  . persist-archives
;;;_   , emt:db:persist-archive:ctor
(defalias 'emt:db:persist-archive:ctor 'make-emt:db:persist-archive)
;;;_ , Test data
;;Moved

;;;_ , singletons wrt the whole database
;;;_  . emt:db:single:create
(defun emt:db:single:create (obj index)
   ""
   (check-type obj emt:db:single)
   (check-type index emt:db:id-index.)
   
   (emt:db:internal:with-db 
      (db (emt:db:id-index.-backend index)) 
      'read/write
      (let*
	 ;;$$REPLACE ME
	 ;;We'd do this processing in the handler.  So use
	 ;;`tinydb-alist-update' and lose almost everything else.
	 ((persist-archive
	     (emt:db:whole:get-record
		db
		(emt:db:id-index.-id index) 
		t))
	    (new-persist-archive
	       (emt:db:persist-archive:create-single
		  persist-archive obj)))
	    
	 (emt:db:whole:update-record 
	    new-persist-archive 
	    db 
	    (emt:db:id-index.-id index)))))



;;;_   , Tests
;;Tested thru `emt:db:set'

;;;_ , persist-archives as containers
;;;_  . emt:db:persist-archive:get-single
(defun emt:db:persist-archive:get-single (persist-archive version-id)
   ""
   (check-type persist-archive emt:db:persist-archive)
   (let
      ((list
	  (emt:db:persist-archive->list persist-archive)))
      
      (find version-id list :key #'emt:db:single->version-id)))

;;;_   , Tests
;;Tested thru `emt:persist:view-obj'

;;;_  . emt:db:persist-archive:ctor
;;;_  . emt:db:persist-archive:map
;;;_  . emt:db:persist-archive:create-single

(defun emt:db:persist-archive:create-single (persist-archive single)
   ""
   (check-type single emt:db:single)
   (check-type persist-archive emt:db:persist-archive)
   
   ;;See [[id:d291192a-e37e-49ce-905f-4841aea19bc4]]
   ;;`org-id-new' makes an almost certainly unique ID
   (setf (emt:db:single->version-id single) (org-id-new))
    
   (push
      single
      (emt:db:persist-archive->list persist-archive))
   persist-archive)

;;;_  . emt:db:persist-archive:update-single
;;;_ , persist-archives in the alist
;;These just wrap "emt:db:whole" functions
;;;_  . emt:db:by-ix:get-record
;;This is mocked as part of tests, so it can't easily disappear.
(defun emt:db:by-ix:get-record (index &optional create-p)
   "Return the PERSIST-ARCHIVE associated with INDEX.
If CREATE-P is non-nil, create it if it doesn't exist."
   (check-type index emt:db:id-index.)
   (emt:db:internal:with-db
      (db (emt:db:id-index.-backend index)) 
      (if create-p 'read/write 'read)
      (emt:db:whole:get-record
	 db (emt:db:id-index.-id index) create-p)))


;;;_  . emt:db:by-ix:create-record

;;Obsolete.  Just wraps `emt:db:whole:update-record'
'
(defun emt:db:by-ix:create-record (index persist-archive)
   ""
   (check-type persist-archive emt:db:persist-archive)

   (emt:db:internal:with-db
      (db (emt:db:id-index.-backend index)) 
      'read/write
      (emt:db:whole:add-record 
	 persist-archive
	 db 
	 (emt:db:id-index.-id index))))


;;;_  . emt:db:by-ix:update-record
;;Obsolete.  Would just wrap
;;`emt:db:whole:update-record' anyways
'
(defun emt:db:by-ix:update-record (persist-archive index)
   ""
   (check-type persist-archive emt:db:persist-archive)
   (let*
      ((db (emt:db:internal:get-all (emt:db:id-index.-backend index)))
	 (arc-id (emt:db:id-index.-id index)))
      (check-type db emt:db:whole)

      (setf
	 (emt:db:whole->list db)
	 (cons
	    persist-archive
	    (remove* arc-id (emt:db:whole->list db) 
	       :key #'car
	       ;;:key #'emt:db:persist-archive->id
	       ))
	 )
      (emt:db:internal:set-all 
	 (emt:db:id-index.-backend index)
	 db)))


;;;_ , Functions about records in the db
;;$$RETHINK ME  This all could just treat it as an alist, which is
;;functionality that's already provided.  Except for find/create,
;;which is really `tinydb-alist-update'
;;;_  . emt:db:whole:add-record

(defun emt:db:whole:add-record (persist-archive db arc-id)
   ""
   (push
      (cons arc-id persist-archive)
      (emt:db:whole->list db))
   (setf (emt:db:whole->dirty db) t))


;;;_  . emt:db:whole:get-record
(defun emt:db:whole:get-record (db arc-id &optional create-p)
   ""
   (check-type db emt:db:whole)
   (let*
      (  
	 ( cell
	    (find ;;This is just assoc.
	       arc-id 
	       (emt:db:whole->list db) 
	       :key #'car
	       :test #'equal)))
      (if cell
	 (cdr cell)
	 (if create-p
	    (let
	       ((persist-archive
		   (emt:db:persist-archive:ctor
		      :list ())))
	       (emt:db:whole:add-record persist-archive db arc-id)
	       persist-archive)
	    (error "No such element: %s" arc-id)))))

;;;_  . emt:db:whole:update-record

(defun emt:db:whole:update-record (persist-archive db arc-id)
   ""
   (check-type persist-archive emt:db:persist-archive)
   (check-type db emt:db:whole)

   (let
      ((record
	  (cons arc-id persist-archive)))
      
      (setf
	 (emt:db:whole->list db)
	 (cons
	    record
	    (remove* arc-id (emt:db:whole->list db) 
	       :key #'car))))

   (setf (emt:db:whole->dirty db) t))

;;;_ , Access to the database
;;;_  . emt:db:internal:with-db

;;Mode can be {read, read/write}
;;$$MOVE ME earlier
(defmacro* emt:db:internal:with-db ((id backend) mode &rest body)
   ""
   (declare (debug ((&define symbolp symbolp) symbolp &rest form)))
   `
   (let
      (  
	 (,id (emt:db:internal:get-all ,backend)))
      (prog1
	 (progn ,@body)
	 (when
	    (emt:db:whole->dirty ,id)
	    (emt:db:internal:set-all ,backend ,id)))))

;;;_ , The database itself
;;;_  . emt:db:internal:tq-alist
(defvar emt:db:internal:tq-alist 
   '()
   "Alist from absolute filenames to file tqs" )
;;;_  . Make the queues - one for each distinct filename
;;$$NEW, untested, to be used in the functions below (uncomment it and
;;replace other code with it)
(defun emt:db:internal:name->tq (filename)
   ""
   (or
      (let
	 ((cell (assoc filename emt:db:internal:tq-alist)))
	 (second cell))
      (let 
	 ((filetq
	     (tinydb-persist-make-q filename '() nil #'listp)))
	 (push (list filename filetq) emt:db:internal:tq-alist)
	 filetq)))



;;;_  . emt:db:internal:get-all
(defun emt:db:internal:get-all (backend)
   ""
   ;;For now, always use persist.el as the backend
   (let
      ((filename  ;;Is this an index or the list to one?
	  (second backend)))
      '
      (make-emt:db:whole
	 :list
	 (tehom-persist-buffer-as-const-obj filename x () #'listp
	    x))
      (tinydb-get-obj (emt:db:internal:name->tq filename))))

;;;_  . emt:db:internal:set-all
(defun emt:db:internal:set-all (backend arg)
   ""
   
   ;;For now, always use persist.el as the backend
   (let
      ((filename
	  (second backend))
	 (obj
	    (emt:db:whole->list arg)))
      '
      (tehom-update-persist-buffer filename obj nil #'listp)
      (tinydb-set-obj (emt:db:internal:name->tq filename) obj)))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/emt-persist)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/emt-persist.el ends here
