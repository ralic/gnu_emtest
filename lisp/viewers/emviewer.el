;;;_ emviewer.el --- Chewie-based viewer for Emtest results

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

;; This file coordinates the use of `receive' and `chewie' to give
;; emtest a dynamic viewer.

;;;_ , Requires

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

(require 'chewie)
(require 'receive)

;;;_. Body
;;;_ , Config

(defconst emtest:viewer:emviewer:report-buffer-name 
   "*Emtest Report (chewie)*")
;;;_ , Globals
;;Using globals because I don't anticipate needing more than 1 active
;;emtest viewer.
(defconst emtest:viewer:emviewer:report-buffer nil 
   "" )
;;result object from receive.  Now it should live here.
(defconst emtest:viewer:emviewer:result-root nil 
   "" )
(defconst emtest:viewer:emviewer:chewie nil 
   "" )
(defconst emtest:viewer:emviewer:receiver 
   nil ;;Should be made by setup.  Of type `emt:receive:data'
   "" )
;;;_ , emtest:viewer:receive-cb
(defun emtest:viewer:receive-cb (presentation-path cell)
   ""
   ;;This interface really does need its own callback.
   (emt:pathtree:add/replace-node
      ;;The pathtree root
      emtest:viewer:emviewer:result-root 
      ;;The path
      presentation-path
      ;;The data
      (make-emt:view:suite-newstyle :cell cell)
      ;;The refresh callback
      #'(lambda (obj)
	   (chewie:freshen-obj
	      emtest:viewer:emviewer:chewie 
	      obj))))

;;;_ , Setup emtest:viewer:setup-if-needed
(defun emtest:viewer:setup-if-needed ()
   ""
   (unless emtest:viewer:emviewer:result-root
      (setq emtest:viewer:emviewer:result-root
	 (emt:pathtree:make-empty-tree-newstyle)))

   (unless emtest:viewer:emviewer:chewie
      (unless emtest:viewer:emviewer:report-buffer
	 (setq 
	    emtest:viewer:emviewer:report-buffer 
	    (generate-new-buffer
	       emtest:viewer:emviewer:report-buffer-name)))
	 
      (with-current-buffer emtest:viewer:emviewer:report-buffer
	 (erase-buffer)
	 (setq emtest:viewer:emviewer:chewie
	    (chewie:setup-root
	       emtest:viewer:emviewer:result-root
	       '()
	       #'emtest:viewer:fmt:top
	       emtest:viewer:emviewer:report-buffer))))
   (unless 
      emtest:viewer:emviewer:receiver
      (setq emtest:viewer:emviewer:receiver
	 (make-emt:receive:data
	    :alist ()
	    :tree-insert-cb #'emtest:viewer:receive-cb
	    ;;:tree-remove-cb Not yet
	    ))))


;;;_ , emtest:viewer:receive

;;This is the callback for emtest to use.  For now, this is always the
;;receiver callback that emtest uses.  Later, customization could
;;control what is used.
(defun emtest:viewer:receive (report)
   ""
   (emtest:viewer:setup-if-needed)
   (emt:receive:newstyle emtest:viewer:emviewer:receiver report)
   (pop-to-buffer emtest:viewer:emviewer:report-buffer))
;;;_ , Utilities
;;;_  . defstruct**
;;Use `emt-match:define-struct-governor' as the skeleton of a general
;;struct parser?  It would take a callback.
(defmacro defstruct** (name fields)
   ""
   ;;Parse name for :conc-name and :predicate
   ;;Store that info too.
   ;;Particulars?  Name?
   `(defstruct ,name ,@fields
       
       ))
;;;_  . A specialized definer for TESTRAL note types (Not yet)

;;;_  . etypecase-w/accessor (Impossible right now)
;;Impossible because conc-name is not available.  Could define a
;;defstruct variant to set it in the information (And predicate name)
;;And maybe formatter for here.
(defmacro etypecase-w/accessor (obj accessor-sym &rest cases)
   "
ACCESSOR-SYM will be available as a macro in each case, bound to the
respective object."
   
   `(etypecase ,obj
       ,@(mapcar
	    #'(lambda (case)
		 `(macrolet 
		     ;;$$FIXME Don't let field-sym be captured
		     ((,accessor-sym (field-sym)
			 ,(etypecase-w/accessor-x
			     ;;$$FIXME Don't let obj be evaluated twice.
			     case 'field-sym ,obj)))
		     ,case))
	    cases)))
;;;_   , etypecase-w/accessor-x Helper (Impossible right now)
(defun etypecase-w/accessor-x (case field-sym obj)
   ""
   ;;$$FINISHME
   (let*
      ((type-sym (car case)))
      ;;If type-sym is a symbol and not null or t, proceed
      ;;CT a function call name.
      ;;$$FIXME:  Would use conc-name but it's not available.  So this
      ;;can't work.  Maybe another defstruct* macro to store that
      ;;information?  Check whether that exists anywhere.
      `(,(intern (concat (symbol-name type-sym) "-" (symbol-name field-sym)))
	  ,obj)))
;;;_   , Tests
;;$$WRITEME

;;;_ , Format functions

;;;_  . emtest:viewer:fmt:top
(defun emtest:viewer:fmt:top (view-node data-list)
   "
SUITE must be an emt:view:pathtree-node-data.
DATA-LIST must be a list of alists."

   (check-type view-node emt:view:pathtree-node)
   ;;Temporary hack.  This will really entwine with pathtree dirtiness
   ;;and updates.  Or wookie dirtiness?
   (emt:receive:sum-node-badnesses view-node)

   ;;WRITEME Get depth from data-list, build a new one that we'll pass
   ;;down.
   (let
      ((suite (emt:view:pathtree-node-data view-node))
	 (name
	    (emt:view:pathtree-node-name view-node))
	 (children
	    (emt:view:pathtree-node-children view-node))
	 (depth
	    (chewie:loal-val 'depth data-list 0)))
      (etypecase suite
	 (null
	    ;;For the top level.
	    (list*
	       "Emtest" "\n"
	       (chewie:formatmap 
		  ;;Formatting for each child
		  #'(lambda (obj data &rest d)
		       (list
			  "\n*\n"
			  `(dynamic ,obj 
			      ,(chewie:loal-acons 'depth (1+ depth) data)
			      ,#'emtest:viewer:fmt:top)))
		  children
		  :separator '("\n")
		  :data-lol data-list)))
	 

	 (emt:view:suite-newstyle
	    (let*
	       (
		  (cell (emt:view:suite-newstyle-cell suite))
		  ;;$$RENAME ELSEWHERE - Naming is muddled here.
		  ;;"suite" appears with two different meanings.
		  (object
		     (emt:receive:suite-newstyle-suite cell)))
	       (etypecase object
		  (emt:testral:test-runner-info
		     (list*
			"Suites tested in " name "\n"
			(chewie:formatmap 
			   ;;Formatting for each child
			   #'(lambda (obj data &rest d)
				(list
				   "\n"
				   (make-string (1+ depth) ?*)
				   "\n"
				   `(dynamic ,obj 
				       ,(chewie:loal-acons 'depth (1+ depth) data)
				       ,#'emtest:viewer:fmt:top)))
			   children
			   :data-lol data-list
			   :separator '("\n"))))
		  
		  (emt:testral:suite
		     (append
			(list
			   "Results for suite " name "\n")

			;;(emt:receive:suite-newstyle-how-to-run cell)
			;;`how-to-run' informs a button.

			;;(emt:testral:suite-badnesses object) is only
			;;about badnesses arising directly from this
			;;suite, eg "Couldn't even run it".  It's
			;;separate.

			(emtest:viewer:fmt:sum-badnesses
			   (emt:view:suite-newstyle-sum-badnesses suite)
			   data)


			;;Info shows nothing for now.  It has no
			;;canonical fields yet.
			;;Use `emtest:viewer:fmt:info'

			(etypecase (emt:testral:suite-contents object)
			   (emt:testral:runform-list
			      (chewie:formatmap 
				 ;;Formatting for each child
				 #'(lambda (obj data &rest d)
				      (list
					 "\n"
					 (make-string (1+ depth) ?*)
					 "\n"
					 `(dynamic ,obj 
					     ,(chewie:loal-acons 
						 'depth (1+ depth) data)
					     ,#'emtest:viewer:fmt:top)))
				 children
				 :data-lol data-list
				 :separator '("\n")
				 :els=0 '("No child suites")))
			   (emt:testral:note-list
			      (chewie:formatmap
				 #'emtest:viewer:fmt:TESTRAL
				 (emt:testral:note-list-notes
				    (emt:testral:suite-contents object))
				 :data-lol data-list
				 :separator '("\n")
				 :els=0 '("No notes")))
			   (null
			      '("No known contents")))

			)))))

	 ;;For the various TESTRAL expansions.
	 ;;For now, these aren't even relevant yet.
	 (emt:view:TESTRAL
	    '("Testral data"))
	 
	 (emt:view:TESTRAL-unexpanded
	    '("Unexpanded TESTRAL data"))
	 
	 )))


;;;_  . emtest:viewer:fmt:TESTRAL (TESTRAL note formatter)
(defun emtest:viewer:fmt:TESTRAL (obj data &rest d)
   ""
   
   (let*
      ()
      (etypecase obj
	 ;;This is the only one that will actually carry over in the
	 ;;long term, the others just inform structure.
	 (emt:testral:alone
	    (typecase obj

	       (emt:testral:error-raised
		  `("\n"
		      "Error raised: "
		      ,(prin1-to-string
			 (emt:testral:error-raised-err obj))
		      "\n"))
	       (t '("A TESTRAL note (alone)""\n"))))
	 
	 (emt:testral:push
	    '("Begin a TESTRAL span"))
	 
	 (emt:testral:pop
	    '("End a TESTRAL span"))
	 (emt:testral:separate
	    '("Separate args")))))

;;;_  . emtest:viewer:fmt:info (Suite info formatter)
(defun emtest:viewer:fmt:info (obj data &rest d)
   ""
   
   (let*
      ()
      '("Information: None" "\n")
      ))

;;;_  . emtest:viewer:fmt:sum-badnesses
(defun emtest:viewer:fmt:sum-badnesses (obj data &rest d)
   ""
   
   (let*
      ()
      (if obj
	 ;;Punt for now.
	 '("Something failed""\n")
	 '("All OK""\n"))))


;;;_. Footers
;;;_ , Provides

(provide 'emviewer)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emviewer.el ends here
