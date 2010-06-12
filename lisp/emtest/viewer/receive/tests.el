;;;_ emtest/viewer/receive/tests.el.el --- Tests for receive module

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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

(require 'emtest/viewer/receive)
(require 'emtest/viewer/receive/testhelp)
(require 'emtest/viewer/view-types/testhelp)
(require 'emtest/common/testral-types/testhelp)
(require 'emtest/testhelp/match)

;;;_. Body
;;;_  . emtvr:one-newstyle
(put 'emtvr:one-newstyle 'emt:test-thru 'emtvr:newstyle)

;;;_  . Tests

(emt:deftest-3 emtvr:newstyle
   (nil
      (progn
	 (emtg:with emtvr:thd nil
	    (emtvr:th:with-usual-receiver receiver nodes-freshened
	       (let*
		  (
		     (report
			(emtg
			   (project emtest)
			   (sub-project testral)
			   (library types)
			   (type report)
			   (name empty))))
		  (emt:doc "Param: Empty report.")
		  (emtvr:newstyle receiver report)
		  (emt:doc "Response: List still contains nothing.")
		  (assert
		     (equal
			(emtvr:data->alist receiver)
			'nil)
		     t)
		  (assert
		     (emtm nodes-freshened (list))
		     t)
		  t)))))
   (nil
      (progn
	 (emtg:with emtvr:thd nil
	    (emtvr:th:with-usual-receiver receiver nodes-freshened
	       (let*
		  ()
		  (emt:doc "Param: A report w/1 entry")
		  (emtvr:newstyle 
		     receiver 
		     (emtg
			(project emtest)
			(sub-project testral)
			(library types)
			(type report)
			(role original-add)
			(what-test test-1)))
		  (emt:doc "Response: List contains that one entry.")
		  (assert
		     (emtm
			(emtvr:data->alist receiver)
			(list
			   (eval
			      '(emtg
				  (type emtvr:alist-item-pattern)
				  (role original-add)
				  (what-test test-1)))))
		     t)
		  (assert
		     (=
			(length
			   (emtvr:data->alist receiver))
			1)
		     t)
		  (emt:doc "Response: One callback happened")
		  (emtg:narrow ((what-test test-1)(role original-add))
		     (emtvr:th:assert-the-1-right-node 
			nodes-freshened))
		  t)))))
   (nil
      (progn
	 (emtg:with emtvr:thd nil
	    (emtvr:th:with-usual-receiver receiver nodes-freshened
	       (let*
		  ()
		  (emtvr:newstyle receiver
		     (emtg
			(project emtest)
			(sub-project testral)
			(library types)
			(type report)
			(what-test test-1)
			(role original-add)))
		  (emt:doc "Situation: Have added a report w/1 entry.")
		  (emt:doc "Situation: Callback list is emptied")
		  (setq nodes-freshened 'nil)
		  (emt:doc "Param:  Another report with a different result for the same test.")
		  (emtvr:newstyle receiver
		     (emtg
			(project emtest)
			(sub-project testral)
			(library types)
			(type report)
			(what-test test-1)
			(role replace)))
		  (emt:doc "Response: List contains just that one
   entry, not duplicated.") 
		  (assert
		     (emtm
			(emtvr:data->alist receiver)
			(list
			   (eval
			      '(emtg
				  (type emtvr:alist-item-pattern)
				  (role replace)
				  (what-test test-1)))))
		     t)
		  (assert
		     (=
			(length
			   (emtvr:data->alist receiver))
			1)
		     t)
		  (emt:doc "Response: One NEW callback happened")
		  ;;$$RETHINK ME - not clear this particular check is
		  ;;still correct. 
		  (emtg:narrow ((what-test test-1)(role replace))
		     (emtvr:th:assert-the-1-right-node 
			nodes-freshened))
		  t)))))
   (nil
      (progn
	 (emtg:with emtvr:thd ()
	    (emtvr:th:with-usual-receiver receiver nodes-freshened
	       (let*
		  ()
		  (emtvr:newstyle receiver
		     (emtg
			(type report)
			(role original-add)
			(what-test test-1)))
		  (emt:doc "Situation: Have added a report w/1 entry.")
		  (emt:doc "Param: Report removes previous report.")
		  (emtvr:newstyle receiver
		     (emtg
			(type report)
			(role remove-previous)
			(what-test test-1)))
		  (emt:doc "Response: List no longer contains that
   entry; it is empty.") 
		  (assert
		     (equal
			(emtvr:data->alist receiver)
			'nil)
		     t)
		  t)))))
   (nil
      (progn
	 (emtg:with emtvr:thd ()
	    (emtvr:th:with-usual-receiver receiver nodes-freshened
	       (let*
		  ()
		  (emtvr:newstyle receiver
		     (emtg
			(type report)
			(role original-add)
			(what-test test-1)))
		  (emt:doc "Situation: Have added a report w/1 entry.")
		  (emt:doc "Situation: Callback list is emptied")
		  (setq nodes-freshened 'nil)
		  (emt:doc "Param: A second report of a different test")
		  (emtvr:newstyle receiver
		     (emtg
			(type report)
			(what-test test-2)))
		  (emt:doc "Response: One NEW callback happened")
		  (emt:doc "Response: List contains both entries.")
		  (emtg:narrow ((what-test test-2))
		     (emtvr:th:assert-the-1-right-node 
			nodes-freshened))
		  t)))))
   '(nil
       (progn
	  (emtg:with emtvr:thd nil
	     (emtvr:th:with-usual-receiver receiver nodes-freshened
		(let*
		   ()
		   ;;$$WRITE ME
		   (emt:doc "Situation: Empty tree.")
		   (emt:doc "Operation: Add a report w/2 entries.")
		   (emt:doc "Response: List contains both entries."))))))

   ;;$$ADD ME Add tests of aliasing:  
   ;;One with aliases makes multiple entries.
   ;;Another with aliases replaces those entries it is alias of

   )



;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/receive/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/receive/tests.el ends here
