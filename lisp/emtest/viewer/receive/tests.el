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
(require 'emtest/types/testral-types/testhelp)
(require 'emtest/testhelp/match)

;;;_. Body
;;;_  . emt:r:receive-one
(put 'emt:r:receive-one 'emt:test-thru 'emt:r:receive)

;;;_  . Tests

(emt:deftest-3 emt:r:receive
   (nil
      (progn
	 (emt:doc "Situation: Empty report.")
	 (emt:doc "Response: List still contains nothing.")
	 (emtg:with emt:r:thd nil
	    (let*
	       ((nodes-freshened 'nil)
		  (remember-freshened-node
		     #'(lambda
			  (x y)
			  (push
			     (list x y)
			     nodes-freshened)))
		  (receiver
		     (emt:r:make-empty-alist remember-freshened-node #'ignore))
		  (report
		     (emtg
			(project emtest)
			(sub-project testral)
			(library types)
			(type report)
			(name empty))))
	       (emt:r:receive receiver report)
	       (emt:assert
		  (equal
		     (emt:r:data->alist receiver)
		     'nil))
	       (emt:assert
		  (emtm nodes-freshened (list)))
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: Have added a report w/1 entry")
	 (emt:doc "Response: List contains that one entry.")
	 (emtg:with emt:r:thd nil
	    (let*
	       ((nodes-freshened 'nil)
		  (remember-freshened-node
		     #'(lambda
			  (x y)
			  (push
			     (list x y)
			     nodes-freshened)))
		  (receiver
		     (emt:r:make-empty-alist remember-freshened-node #'ignore))
		  (report
		     (emtg
			(project emtest)
			(sub-project testral)
			(library types)
			(type report)
			(role original-add)
			(what-test test-1))))
	       (emt:r:receive receiver report)
	       (emt:assert
		  (emtm
		     (emt:r:data->alist receiver)
		     (list
			(eval
			   '(emtg
			       (type emt:r:alist-item-pattern)
			       (role original-add)
			       (what-test test-1))))))
	       (emt:assert
		  (=
		     (length
			(emt:r:data->alist receiver))
		     1))
	       (emt:doc "Response: One callback happened")
	       (emtg:narrow ((what-test test-1)(role original-add))
		  (emt:r:th:assert-the-1-right-node 
		     nodes-freshened))
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: Have added a report w/1 entry.")
	 (emt:doc "Operation:  Another report with a different result for the same test.")
	 (emt:doc "Response: List contains just that one entry, not duplicated.")
	 (emtg:with emt:r:thd nil
	    (let*
	       ((nodes-freshened 'nil)
		  (remember-freshened-node
		     #'(lambda
			  (x y)
			  (push
			     (list x y)
			     nodes-freshened)))
		  (receiver
		     (emt:r:make-empty-alist remember-freshened-node #'ignore)))
	       (emt:r:receive receiver
		  (emtg
		     (project emtest)
		     (sub-project testral)
		     (library types)
		     (type report)
		     (what-test test-1)
		     (role original-add)))
	       (emt:doc "Situation: Callback list is emptied")
	       (setq nodes-freshened 'nil)
	       (emt:r:receive receiver
		  (emtg
		     (project emtest)
		     (sub-project testral)
		     (library types)
		     (type report)
		     (what-test test-1)
		     (role replace)))
	       (emt:assert
		  (emtm
		     (emt:r:data->alist receiver)
		     (list
			(eval
			   '(emtg
			       (type emt:r:alist-item-pattern)
			       (role replace)
			       (what-test test-1))))))
	       (emt:assert
		  (=
		     (length
			(emt:r:data->alist receiver))
		     1))
	       (emt:doc "Response: One NEW callback happened")
	       (emtg:narrow ((what-test test-1)(role replace))
		  (emt:r:th:assert-the-1-right-node 
		     nodes-freshened))
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: Have added a report w/1 entry.")
	 (emt:doc "Operation: Report removes previous report.")
	 (emt:doc "Response: List no longer contains that entry; it is empty.")
	 (emtg:with emt:r:thd
	    ((project emtest)
	       (sub-project testral)
	       (library types))
	    (let*
	       ((nodes-freshened 'nil)
		  (remember-freshened-node
		     #'(lambda
			  (x y)
			  (push
			     (list x y)
			     nodes-freshened)))
		  (receiver
		     (emt:r:make-empty-alist remember-freshened-node #'ignore)))
	       (emt:r:receive receiver
		  (emtg
		     (type report)
		     (role original-add)
		     (what-test test-1)))
	       (emt:r:receive receiver
		  (emtg
		     (type report)
		     (role remove-previous)
		     (what-test test-1)))
	       (emt:assert
		  (equal
		     (emt:r:data->alist receiver)
		     'nil))
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: Have added a report w/1 entry.")
	 (emt:doc "Operation: Add a second report")
	 (emt:doc "Response: List contains both entries.")
	 (emtg:with emt:r:thd
	    ((project emtest)
	       (sub-project testral)
	       (library types))
	    (let*
	       ((nodes-freshened 'nil)
		  (remember-freshened-node
		     #'(lambda
			  (x y)
			  (push
			     (list x y)
			     nodes-freshened)))
		  (receiver
		     (emt:r:make-empty-alist remember-freshened-node #'ignore)))
	       (emt:r:receive receiver
		  (emtg
		     (type report)
		     (role original-add)
		     (what-test test-1)))
	       (emt:doc "Situation: Callback list is emptied")
	       (setq nodes-freshened 'nil)
	       (emt:r:receive receiver
		  (emtg
		     (type report)
		     (what-test test-2)))
	       (emt:doc "Response: One NEW callback happened")
	       (emtg:narrow ((what-test test-2))
		  (emt:r:th:assert-the-1-right-node 
		     nodes-freshened))
	       t))))
   '(nil
       (progn
	  (emt:doc "Situation: Empty tree.")
	  (emt:doc "Operation: Add a report w/2 entries.")
	  (emtg:with emt:r:thd nil
	     (let*
		((nodes-freshened 'nil)
		   (remember-freshened-node
		      #'(lambda
			   (x y)
			   (push
			      (list x y)
			      nodes-freshened)))
		   (receiver
		      (emt:r:make-empty-alist 
			 remember-freshened-node 
			 #'ignore)))
		;;$$WRITE ME
		(emt:doc "Response: List contains both entries.")))))

   ;;$$ADD ME Add tests of aliasing:  
   ;;One with aliases makes multiple entries.
   ;;Another with aliases replaces those entries it is alias of

   )



;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/receive/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/receive/tests.el ends here
