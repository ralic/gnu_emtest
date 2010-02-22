;;;_ viewers/plain-viewer.el --- Plain viewer for emtest

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
(require 'viewers/formatter)
;;;_  . Test requirements
(rtest:if-avail
   (require 'common/result-types/testhelp)
   (require 'tester/testhelp/eg))


;;;_. Body
;;;_ , emtest:viewer:plain:report-buffer
(defconst emtest:viewer:plain:report-buffer "*Emtest Report*")

;;;_ , (Here for now) Format an inspection-test of string comparison

;;;_  . Viewer (puts in buffers, launches ediff)
;;Except if one part doesn't exist, then it just shows what's there.

;;;_  . Command to accept the output
;;;_  . (Other commands)
;;;_  . Keymaps for it, nothing yet
;;They'll inherit from a base result-buffer keymap.

;;;_  . Testage
;;Example comparison
;;Launch the viewer

;;;_  . Associated tester function
;;Tester library function to fill out that form.
;;And mockbuf already does some things along these lines.  


;;;_ , Exploratory code.  This works, as far as it goes.
;;;_  . emt:plain-viewer:top
'  ;;NOT obsolete - shared with formatter clients.  Moved into
;;formatter and renamed.
(defun emt:plain-viewer:top (result-obj)
   ""
   
   (let
      ((buf
	  (get-buffer-create emtest:viewer:plain:report-buffer)))
      (with-current-buffer buf
	 (erase-buffer)
	 (emt:plain-viewer:print-in-buffer result-obj))
      (pop-to-buffer buf)))

(require 'ewoc)

(defun emt:plain-viewer:make-result-list ()
   ""
   ())

(defvar emtt:results (emt:plain-viewer:make-result-list)
   "List of top-level result objects, each of type `emt:result-group'" )
(defvar emt:plain-viewer:ewoc nil "" )

(defun emt:plain-viewer:top (result-obj)
   ""
   (unless emt:plain-viewer:ewoc
      (with-current-buffer 
	 (get-buffer-create emtest:viewer:plain:report-buffer)
	 (erase-buffer)
	 (setq emt:plain-viewer:ewoc
	    (ewoc-create #'emt:plain-viewer:print-in-buffer 
	       "Ewoc-style Emtest report"))))
   (emt:plain-viewer:top-x result-obj emt:plain-viewer:ewoc)
   (pop-to-buffer (ewoc-buffer emt:plain-viewer:ewoc)))

(defun emt:plain-viewer:top-x (result-obj ewoc)
   "Buffer-agnostic worker.
Assumes ewoc has been set up.  Doesn't care about making
the buffer visible."

   (emt:plain-viewer:delete-old-result result-obj ewoc)
   (emt:plain-viewer:add-result result-obj ewoc))


(defun emt:plain-viewer:delete-old-result (result-obj ewoc)
   ""
   (let* 
      (  (test-id (emt:result-group-grouping result-obj))
	 (old-one 
	    (find test-id emtt:results 
	       :test
	       #'(lambda (test-id elem)
		    (equal
		       test-id
		       (emt:result-group-grouping elem))))))
      
      (when old-one
	 ;;Remove it from ewoc
	 (ewoc-filter ewoc
	    #'(lambda (node old-one)
		 (not (equal old-one node)))
	    old-one)
	 
	 ;;Remove it from results
	 (setq emtt:results
	    (delete*
	       test-id emtt:results 
	       :test
	       #'(lambda (test-id elem)
		    (equal
		       test-id
		       (emt:result-group-grouping elem))))))))


(defun emt:plain-viewer:add-result (result-obj ewoc)
   ""

   ;;Push the new one on the list.  For now, just push it at top
   ;;level.
   (push result-obj emtt:results)
   
   ;;Activate it.  For now, this just adds the node at the beginning.
   (ewoc-enter-first ewoc result-obj))


;;;_   , Test helper

(defmacro emt:plain-viewer:top:th (&rest body)
   ""
   
   `(let
       ((emtt:results (emt:plain-viewer:make-result-list)))
       (with-mock
	  (stub ewoc-enter-first)
	  (stub ewoc-filter)
	  ,@body)))


;;;_   , Tests

(rtest:deftest emt:plain-viewer:top

   (  "Proves: Adding a result to an empty list creates it somewhere."
      (emt:plain-viewer:top:th
	 (let
	    ((result
		(emt:eg 
		   (project emtest)
		   (library result-types)
		   (type result-group)
		   (name simple)))
	       (ewoc nil))
	    (emt:plain-viewer:add-result result ewoc)
	    (assert
	       (emt:somewhere-in-tree #'equal
		  emtt:results result)
	       t)
	    t)))
   

   (  "Proves: Removing a result, gives a list that does
not contain it (but is not neccessarily empty)"
      (emt:plain-viewer:top:th
	 (let
	    ((result
		(emt:eg 
		   (project emtest)
		   (library result-types)
		   (type result-group)
		   (name simple)))
	       (ewoc nil))
	    (emt:plain-viewer:add-result result ewoc)
	    (emt:plain-viewer:delete-old-result result ewoc)
	    (assert
	       (not
		  (emt:somewhere-in-tree #'equal
		     emtt:results result)))
	    t)))

   (  "Proves: Removing a result does not remove a different result
Situation: One result has been added.  A different result is
pseudo-removed (though it wasn't on the list - which is OK)
Response: The list still contains the first result."
      (emt:plain-viewer:top:th
	 (let
	    ((result-1
		(emt:eg 
		   (project emtest)
		   (library result-types)
		   (type result-group)
		   (name simple)))
	       (result-2
		  (emt:eg 
		     (project emtest)
		     (library result-types)
		     (type result-group)
		     (name another)))
	       (ewoc nil))
	    (emt:plain-viewer:add-result result-1 ewoc)
	    (emt:plain-viewer:delete-old-result result-2 ewoc)
	    (assert
	       (emt:somewhere-in-tree #'equal
		  emtt:results result-1))
	    t)))
   

   



   '  ;;Usability tests.  Manual forever, since they require seeing
   ;;that the buffer in fact pops up, etc.

   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	  ((name simple))
	 (emt:plain-viewer:top (emt:eg (type event-list)))))

   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	  ((name aborted-after-events))
	 (emt:plain-viewer:top (emt:eg (type event-list)))))
   

   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	  ((name has-child-event-groups))
	 (emt:plain-viewer:top (emt:eg (type event-list))))
      )
   
   )

;;;_  . emt:plain-viewer:print-in-buffer
(defun emt:plain-viewer:print-in-buffer (object)
   "Print OBJECT in buffer as a test-result
OBJECT may also be just a part of a test-result, for testing purposes."
   
   (outline-minor-mode t)
   (local-set-key [tab] #'outline-cycle) ;;Only if available.
   
   (emt:plain-viewer:insert
      (emt:plain-viewer:stage2->stage3
	 (emt:plain-viewer:->format-stage2
	    (emt:plain-viewer:->format-tree 
	       (emt:plain-viewer:->annotate-results
		  object))
	    emt:plain-viewer:stage2-initial-state))))

;;;_   , Tests
;;Tested thru emt:plain-viewer:top, such as it is tested

;;;_  . emt:viewer:plain:ts
(defmacro emt:viewer:plain:ts (object property &rest body)
   ""
   
   `(with-temp-buffer
       (emt:plain-viewer:print-in-buffer ,object)
       (goto-char
	  (next-single-property-change 1 ,property))
       ,@body))

;;;_   , Tests
;;Tested by using it for the persist tests

;;;_  . emt:plain-viewer:->annotate-results
;;For now, does nothing.
(defalias 'emt:plain-viewer:->annotate-results 'identity)
;;Create a `emt:result:info-about:summary:event-list-status' for each
;;`emt:result:event:group' from its events.

;;;_   , Tests
(rtest:deftest emt:plain-viewer:->annotate-results

   ;;Use the various examples.  Expect 
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (let
	 ((annotated
	     (emt:plain-viewer:->annotate-results
		;;Must make deep copy so we don't change the original.
		(copy-tree
		   (emt:eg 
		      (project emt)
		      (library result-types)
		      (type result-group)
		      (name simple))
		   t))))
	 ;;Destructure it and test that it contains an annotation
	 ;;Expect `some-fail', because there was one of each.
	 ;;And perhaps marked aborted also?
	 '()

	 ;;Another is (name another).
	 ;;Expect 'cant-tell

	 ;;Could bundle the expected annotations with them.  At least,
	 ;;the simple ones.
	 ) 
      )
   
   )

;;;_  . emt:plain-viewer:->format-tree
(require 'cust-print)  ;;$$May be removed after object-formatting is
;;delegated to next stage.
;;Receiving an object, return a format tree
(defun emt:plain-viewer:->format-tree (obj)
   ""
   (typecase obj
      (emt:result:event:group
	 (let*
	    ((name-info
		(find nil 
		   (emt:result:event:group-info-about obj)
		   :test 
		   #'(lambda (dummy x)
			(emt:result:info-about:name-p x))))
	       
	       (name
		  (if name-info 
		     (emt:result:info-about:name-name name-info)
		     "Unnamed event-group")))
	    
	 `(w/headline
	     (:weight 2)
	     ;;Can't use this, because doesn't always have a name.
	     ;;May need to rearrange into named groups, or to always
	     ;;place a name in it first, creating it if needed.
	     ;;,(emt:result:event:group-name obj)
	     ,name

	     ,(when (emt:result:event:group-aborted-p obj)
		 '(section
		     (:weight 2 :style bad)
		     "This stage aborted before it could finish"))

	     ;;Print the info-about just as they occur.
	     
	     ,(let
		 ((num-children
		     (emt:result:event:group-children obj)))
		 
		 
		 (case num-children
		    (0
		       '(section
			   (:weight 2 :style dormant)
			   "Nothing happened"))
		    (1
		       `(section
			   (:weight 2)
			   "Just one event"
			   ,(emt:plain-viewer:->format-tree 
			      (car
				 (emt:result:event:group-children
				    obj)))))
		    
		    (t
		       `(w/headline () "Events"
			   
			   ;;Iterate over children.
			   ,@(mapcar 
				#'emt:plain-viewer:->format-tree 
				(emt:result:event:group-children
				   obj))))))
	     


	     ,(when (emt:result:event:group-aborted-p obj)
		 `(section ()
		     ,name " aborted at this point"))
       

	     )))
      (emt:result-group
	 `(w/headline (:weight 2)
	     "Results"
	     ;;-grouping is the test ID, a datum.
	     ;;-info is info.  It could be used.
	     ;;Explore its status
	     ,(emt:plain-viewer:->format-tree
		 (emt:result-group-status obj)
		 )
	     )
	 )

      (emt:result:status:was-run
	 ;;Handle a emt:result:status:was-run
	 `(w/headline (:weight 2)
	    "Events"
	     ;;
	    ,(emt:plain-viewer:->format-tree 
		(emt:result:status:was-run-events obj)))
	 )

      

      (emt:result:event:grade
	 `(w/headline (:weight 1)
	     ,(case
		 (emt:result:event:grade-grade obj)
		 (pass
		    "Passed")
		 (fail
		    "Failed")
		 (ungraded
		    "Could not grade")
		 (dormant
		    "Dormant")
		 (t 
		    "Internal error!  No such grade!")
		 )
	     
	     ,@(mapcar
		  #'emt:plain-viewer:->format-tree 
		  (emt:result:event:grade-diagnostic-info obj))))
      
      
      (emt:result:diag:call
	 (let*
	    ((form-string
		;;$$Should delegate this to a later stage via
		;; `(data-persist-used ,(emt:result:diag:call-call-sexp
		;; obj) ()) ;;Or whatever props. 
		(custom-prin1-to-string
		   (emt:result:diag:call-call-sexp obj)))

	       (name "This form"))
	    
	    
	    `(w/headline (:weight 1)
		,name

		;;Remember, this just described its contribution to the
		;;grade.  Later this info may not be the most significant
		;;text..

		,(case
		    (emt:result:diag:call-status obj)
		    ((t) "succeeded")
		    ((nil) "failed")
		    ((error) "had an error"))
		"\n"  ;;Should be (sep 3), but that's not allowed yet
		,form-string

		,@(let
		     ((tried-list
			 ;;If it returns `nil', there's no
			 ;;placeholder.
			 (emt:result:diag:call-tried obj)))

		     (mapcar
			#'(lambda (tried)
			     `(data-persist-used 
				 ;;The keymap name (not used yet)
				 view-persist-comparison-map
				 ;;The text property list
				 (  emt:diag:tried ,tried
				    emt:diag:call  ,obj)))
			tried-list)
		     
		     
		     ))))
      

      (t
	 `(w/headline (:weight 1)
	     "Unrecognized object"
	     "Got an unrecognized object:"
	     ;;$$Should delegate this to a later stage via
	     ;; `(data-persist-used ,obj ()) ;;Or whatever props.
	     ;;May want `custom-prin1-to-string' instead.
	     ,(pp-to-string obj)))))

;;;_   , Tests

(rtest:if-avail
   (require 'common/result-types/testhelp))
(require 'tester/define)

(emt:deftest-2 emt:plain-viewer:->format-tree
   ;;Inspection tests - and experimenting with persist and emtest.

   (props 
      (db-id 
	 '(persist 
	     "~/projects/emtest/lisp/t/data/plain-viewer-persisting-data")))

   ( "Situation: WRITEME.
Response: WRITEME."
      ;;Fix, make loop.
      (emt:eg:narrow 
	 ((name simple))
	 (should
	    (equal
	       (emt:plain-viewer:->format-tree
		  (emt:eg (type event-list)))
	       (emt:persist
		  "dbid:5e14830f-e495-4d41-8fab-0fc9a1a9c4e9")
	       

	       ))))
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name aborted-after-events))
	 (should
	    (equal
	       (emt:plain-viewer:->format-tree
		  (emt:eg (type event-list)))
	       (emt:persist
		  "dbid:0b9a3d2a-f73b-4cf2-a39d-4f937111a606")

	       ))))
   )

(emt:deftest-3 
   ;;Inspection tests - and experimenting with persist and emtest.

   (  (of 'emt:plain-viewer:->format-tree)
      (db-id 
	 '(persist 
	     "~/projects/emtest/lisp/t/data/plain-viewer-persisting-data")))

   (
      ;;Fix, make loop.
      (emt:eg:narrow 
	 ((name simple))
	 '(doc "Situation: WRITEME.")
	 '(doc "Response: WRITEME")
	 (should
	    (equal
	       (emt:plain-viewer:->format-tree
		  (emt:eg (type event-list)))
	       (emt:persist
		  "dbid:5e14830f-e495-4d41-8fab-0fc9a1a9c4e9")
	       

	       ))))
   (  
      (emt:eg:narrow 
	 ((name aborted-after-events))
	 '(doc "Situation: WRITEME.")
	 '(doc "Response: WRITEME")
	 (should
	    (equal
	       (emt:plain-viewer:->format-tree
		  (emt:eg (type event-list)))
	       (emt:persist
		  "dbid:0b9a3d2a-f73b-4cf2-a39d-4f937111a606")

	       ))))
   )

'  ;;Would like to can just write, covering all of the examples:
(emt:eg:loop (name)
   (let
      ((persist-group ;;But want loop over archive members here too.
	  (emt:persist "dbid:c2821b69-043f-4792-9b68-7136581a7f60")))
      ;;Could check output type too
      (should
	 (equal
	    (emt:plain-viewer:->format-tree
	       (emt:eg (type event-list)))
	    (emt:persist:extract persist-group (emt:eg:arg name))))))


;;Will include essentially these tests later.
'
(


   
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	  ((name has-child-event-lists))
	 (emt:plain-viewer:->format-tree (emt:eg (type event-list)))))

   ;;Of grade results
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name pass))
	 (emt:plain-viewer:->format-tree 
	    (emt:eg (type grade-event)))))
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name fail))
	 (emt:plain-viewer:->format-tree 
	    (emt:eg (type grade-event)))))
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name ungraded))
	 (emt:plain-viewer:->format-tree 
	    (emt:eg (type grade-event)))))

   ;;Of full results.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (emt:eg:narrow 
	 ((name simple))
	 (emt:plain-viewer:->format-tree 
	    (emt:eg (type result-group)(name simple))))

      
      )

   ;;Of diagnostic traces
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      ;;Also see fail and ungraded
      (emt:eg:narrow 
	 ((name pass))
	 (emt:plain-viewer:->format-tree 
	    (emt:eg (type result-diag))))

      )
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      ;;Show a very long call.
      (emt:eg:narrow 
	 ((name long-call))
	 (emt:plain-viewer:->format-tree 
	    (emt:eg (type result-diag))))

      )

   ;;A diagnostic trace with a placeholder

   '
   (  "Situation: The diag call is annotated that it tried to use a
persist. 
Response: Result contains (a `data-persist-used' object that mentions)
the respective version placeholder."
      (emt:eg:narrow 
	 ;;This would usually have (library persist) but this part
	 ;;merges persist and viewer functionality (though viewer is
	 ;;not yet using these examples)
	 ((project emtest)(section persist-viewer))
	 (require 't/emt-persist "t/emt-persist.el")
	 (emt:db:internal:ts:mock (emt:eg (type versions))
	    (let
	       ((result
		   (emt:plain-viewer:->format-tree 
		      (emt:eg (type result-diag)(foundp t)))))

	       ;;The result contains the "tried" somewhere in the
	       ;;tree.  Don't test to match data object - it will
	       ;;change soon.
	       (assert
		  (emt:somewhere-in-tree
		     #'eq
		     result
		     (emt:eg (type tried)(foundp t)))
		  t)
	       ;;...and result contains the diag-call somewhere in the
	       ;;tree.
	       (assert
		  (emt:somewhere-in-tree
		     #'eq
		     result
		     (emt:eg (type result-diag)(foundp t)))
		  t)

	       ;;This is here just for viewability.  Later it will
	       ;;itself be a persist object.
	       result
	       )))
      
      )


   )

;;;_   , Explore persisting tests

'
(require 'emt-persist "persistence/emt-persist")

(defconst plain-viewer:th:persist-filename
      (rtest:expand-filename-by-load-file 
	 "../t/data/plain-viewer-persisting-data")
      "File where persisting data is." )

'
(emt:eg:narrow 
   ((name simple))
   (emt:funcall
      #'equal
      (emt:plain-viewer:->format-tree (emt:eg (type event-list)))
      (emt:persist "viewer-345dnhp2mgp"
	 (list 'persist plain-viewer:th:persist-filename))))

;;;_. Footers
;;;_ , Provides

(provide 'viewers/plain-viewer)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/plain-viewer.el ends here
