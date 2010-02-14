;;;_ emformat.el --- Formatting functions specific to Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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
(require 'pathtree) ;;The first-arg type
(require 'loal)     ;;The second-arg type

;;$$DESIGNME - maybe require chewie.  Ie, this isn't under chewie
;;control-wise, chewie is under this.  That must wait until after the
;;changeover, if any.

;;;_. Body

;;;_ , Format functions
;;;_  . emtest:viewer:fmt:top
(defun emtest:viewer:fmt:top (view-tree data-list)
   ""
   
   (check-type view-tree emt:view:pathtree)
   (list
      "Emtest" "\n"
      `(dynamic 
	  ,(emt:view:pathtree-root view-tree)
	 ,(loal:acons 'depth 0 data-list)
	 ,#'emtest:viewer:fmt:node)))


;;;_  . emtest:viewer:fmt:node
(defun emtest:viewer:fmt:node (view-node data-list)
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
	    (loal:val 'depth data-list 0)))
      (append
	 (list
	    ;;Headline groups with the item itself, not with its parent.
	    ;;Even blank items will print one.
	    "\n"
	    (make-string (1+ depth) ?*) 
	    " " name
	    " ") 
	 (emtest:viewer:fmt:sum-badnesses
	    (emt:view:presentable-sum-badnesses suite) 
	    data-list) 
	 (list "\n\n")
	 (etypecase suite
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
			   (hiformat:map 
			      ;;Formatting for each child
			      #'(lambda (obj data &rest d)
				   (list
				      `(dynamic ,obj 
					  ,(loal:acons 'depth (1+ depth) data)
					  ,#'emtest:viewer:fmt:node)))
			      children
			      :data-loal data-list
			      :separator '("\n"))))
		  
		     (emt:testral:suite
			(append
			   (list
			      "Results for suite " name "\n")

			   ;;(emt:receive:suite-newstyle-how-to-run cell)
			   ;;`how-to-run' informs a button.

			   ;;Info shows nothing for now.  It has no
			   ;;canonical fields yet.
			   ;;Use `emtest:viewer:fmt:info'

			   (etypecase (emt:testral:suite-contents object)
			      (emt:testral:runform-list
				 (hiformat:map 
				    ;;Formatting for each child
				    #'(lambda (obj data &rest d)
					 (list
					    `(dynamic ,obj 
						,(loal:acons 
						    'depth (1+ depth) data)
						,#'emtest:viewer:fmt:node)))
				    children
				    :data-loal data-list
				    :separator '("\n")
				    :els=0 '("No child suites")))
			      (emt:testral:note-list
				 (hiformat:map
				    #'emtest:viewer:fmt:TESTRAL
				    (emt:testral:note-list-notes
				       (emt:testral:suite-contents object))
				    :data-loal data-list
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

	    ;;Base type, for blank nodes.
	    (emt:view:presentable
	       (list*
		  "\n"
		  (hiformat:map 
		     ;;Formatting for each child
		     #'(lambda (obj data &rest d)
			  (list
			     `(dynamic ,obj 
				 ,(loal:acons 'depth (1+ depth) data)
				 ,#'emtest:viewer:fmt:node)))
		     children
		     :separator '("\n")
		     :data-loal data-list)))

	    ;;`nil' should not come here.
	    ))))


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
	       (emt:testral:doc
		  `("\n"
		      ,(emt:testral:doc-str obj)
		      "\n"))
	       (t '("A TESTRAL note (alone)""\n"))))
	 ;;Temporary (Probably)
	 (emt:testral:check:push
	    '("Begin a TESTRAL check"))
	 
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

(provide 'emformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emformat.el ends here
