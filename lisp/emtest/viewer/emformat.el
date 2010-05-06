;;;_ emtest/viewer/emformat.el --- Formatting functions specific to Emtest

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

(require 'utility/pathtree) ;;The view-node type
(require 'utility/loal)     ;;The data-list type
(require 'viewers/hiformat)
(require 'emtest/viewer/view-types)

;;;_. Body

;;;_ , Format functions
;;;_  . emtvf:top

(defun emtvf:top (view-node data-list)
   ""

   (check-type view-node emtvp-node)
   (list*
      "Emtest" "\n"
      (emtvf:node view-node data-list)))

;;;_  . emtvf:node
(defun emtvf:node (view-node data-list)
   "
SUITE must be an emtvp-node-data.
DATA-LIST must be a list of alists."

   (check-type view-node emtvp-node)
   ;;Temporary hack.  This will really entwine with pathtree dirtiness
   ;;and updates.  Or wookie dirtiness?
   ;;(emtvr:sum-node-badnesses view-node)

   ;;WRITEME Get depth from data-list, build a new one that we'll pass
   ;;down.
   (let
      ((suite (emtvp-node-data view-node))
	 (name
	    (emtvp-node-name view-node))
	 (children
	    (emtvp-node-children view-node))
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
	 (emtvf:sum-badnesses
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
			(emtvr:suite-newstyle-suite cell)))
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
					  ,#'emtvf:node)))
			      children
			      :data-loal data-list
			      :separator '("\n"))))
		  
		     (emt:testral:suite
			(append
			   (list
			      "Results for suite " name "\n")

			   ;;(emtvr:suite-newstyle-how-to-run cell)
			   ;;`how-to-run' informs a button.

			   ;;Info shows nothing for now.  It has no
			   ;;canonical fields yet.
			   ;;Use `emtvf:info'

			   (etypecase (emt:testral:suite-contents object)
			      (emt:testral:runform-list
				 (hiformat:map 
				    ;;Formatting for each child
				    #'(lambda (obj data &rest d)
					 (list
					    `(dynamic ,obj 
						,(loal:acons 
						    'depth (1+ depth) data)
						,#'emtvf:node)))
				    children
				    :data-loal data-list
				    :separator '("\n")
				    :els=0 '("No child suites")))
			      (emt:testral:note-list
				 (hiformat:map
				    #'emtvf:TESTRAL
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
				 ,#'emtvf:node)))
		     children
		     :separator '("\n")
		     :data-loal data-list)))

	    ;;`nil' should not come here.
	    ))))


;;;_  . emtvf:TESTRAL (TESTRAL note formatter)
(defun emtvf:TESTRAL (obj data &rest d)
   ""
   
   (let*
      ()
      (etypecase obj
	 ;;This is the only one that will actually carry over in the
	 ;;long term, the others just inform structure.
	 (emt:testral:alone
	    (typecase obj

	       (emt:testral:error-raised
		  `((nl-if-none)
		      "Error raised: "
		      ,(prin1-to-string
			 (emt:testral:error-raised-err obj))
		      "\n"))
	       (emt:testral:doc
		  `((nl-if-none)
		      ,(emt:testral:doc-str obj)
		      "\n"))
	       (t '((nl-if-none )
		      "A TESTRAL note (alone)"
		      "\n"))))
	 ;;Temporary (Probably)
	 (emt:testral:check:push
	    '("Begin a TESTRAL check"))
	 
	 (emt:testral:push
	    '("Begin a TESTRAL span"))
	 
	 (emt:testral:pop
	    '("End a TESTRAL span"))
	 (emt:testral:separate
	    '("Separate args")))))

;;;_  . emtvf:info (Suite info formatter)
(defun emtvf:info (obj data &rest d)
   ""
   
   (let*
      ()
      '("Information: None" "\n")
      ))

;;;_  . emtvf:sum-badnesses
(defun emtvf:sum-badnesses (obj data &rest d)
   ""
   
   (let*
      ()
      (if obj
	 ;;Punt for now.
	 '("Something failed""\n")
	 '("All OK""\n"))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emformat)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emformat.el ends here
