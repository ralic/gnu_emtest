;;;_ rtest.el --- Tests for receive module

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
(put 'emtvr:one-newstyle 'rtest:test-thru
   'emtvr:newstyle)
;;;_  . emtvr:suite test helpers
'  ;;$$OBSOLETE
(emtm:define-struct-governor
   emtvr:suite
   id how-to-run presentation-path testrun-id suite)

;;;_  . Tests

(rtest:deftest emtvr:newstyle

   ;;Add reports.  Lists should contain what's expected.
   (  "Situation: Empty report.
Response: List still contains nothing."
      (emtg:with emt:testral:thd:examples ()
	 (let*
	    ((nodes-freshened '())
	       (remember-freshened-node
		  #'(lambda (x y)
		       (push (list x y) nodes-freshened)))
	       (receiver (emtvr:make-empty-alist remember-freshened-node #'ignore))
	       (report (emtg
			  (project emtest)
			  (sub-project testral)
			  (library types)
			  (type report)
			  (name empty))))
	    (emtvr:newstyle receiver report)
	    ;;Still an empty list
	    (assert
	       (equal
		  (emtvr:data->alist receiver)
		  '())
	       t)
	    ;;No callbacks happened
	    (assert
	       (emtm
		  nodes-freshened
		  (list))
	       t)
	    t)))
   
   
   ;;Report w/1 entry
   (  "Situation: Have added a report w/1 entry
Response: List contains that one entry."
      (emtg:with emt:testral:thd:examples ()
	 (let*
	    ((nodes-freshened '())
	       (remember-freshened-node
		  #'(lambda (x y)
		       (push (list x y) nodes-freshened)))
	       (receiver (emtvr:make-empty-alist remember-freshened-node #'ignore))
	       (report (emtg
			  (project emtest)
			  (sub-project testral)
			  (library types)
			  (type report)
			  (role original-add)
			  (what-test test-1))))
	    (emtvr:newstyle receiver report)

	    ;;A list with just that entry
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
	       (= (length (emtvr:data->alist receiver)) 1)
	       t)
	    (emtg:narrow ((what-test test-1)(role original-add))
		  (emtvr:th:assert-the-1-right-node 
		     nodes-freshened))
	    ;;
	    t)))
   

   ;;Add successive reports on the same test.
   (  "Situation: Have added a report w/1 entry.
Operation:  Another report with a different result for the same test.
Response: List contains just that one entry, not duplicated."
      (emtg:with emt:testral:thd:examples ()
	 (let*
	    ((nodes-freshened '())
	       (remember-freshened-node
		  #'(lambda (x y)
		       (push (list x y) nodes-freshened)))
	       (receiver (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	    (emtvr:newstyle receiver 
	       (emtg
		  (project emtest)
		  (sub-project testral)
		  (library types)
		  (type report)
		  (what-test test-1)
		  (role original-add)))
	 
	    (setq nodes-freshened '())
	    ;;Add a report that just overrides the original
	    (emtvr:newstyle receiver 
	       (emtg
		  (project emtest)
		  (sub-project testral)
		  (library types)
		  (type report)
		  (what-test test-1)
		  (role replace)))

	    ;;A list with just that entry
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
	       (= (length (emtvr:data->alist receiver)) 1)
	       t)

	    ;;One (new) callback happened
	    (emtg:narrow ((what-test test-1)(role replace))
	       (emtvr:th:assert-the-1-right-node 
		  nodes-freshened))
	    t)))

   ;;Remove a no-longer-existing runnable.
   (  "Situation: Have added a report w/1 entry.
Operation: Report removes previous report.
Response: List no longer contains that entry; it is empty."
      (emtg:with emt:testral:thd:examples
	 ((project emtest)(sub-project testral)(library types))
	 (let*
	    ((nodes-freshened '())
	       (remember-freshened-node
		  #'(lambda (x y)
		       (push (list x y) nodes-freshened)))
	       (receiver 
		  (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	    (emtvr:newstyle receiver 
	       (emtg
		  (type report)
		  (role original-add)
		  (what-test test-1)))

	    (emtvr:newstyle receiver 
	       (emtg
		  (type report)
		  (role remove-previous)
		  (what-test test-1)))

	    ;;An empty list again
	    (assert
	       (equal
		  (emtvr:data->alist receiver)
		  '())
	       t)

	    ;;Not yet doing callbacks wrt this.

	    t)))

   (  "Situation: Have added a report w/1 entry.
Operation: Add a second report
Response: List contains both entries."
      (emtg:with emt:testral:thd:examples
	 ((project emtest)(sub-project testral)(library types))
	 (let*
	    ((nodes-freshened '())
	       (remember-freshened-node
		  #'(lambda (x y)
		       (push (list x y) nodes-freshened)))
	       (receiver 
		  (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	    (emtvr:newstyle receiver 
	       (emtg
		  (type report)
		  (role original-add)
		  (what-test test-1)))

	    ;;Empty the callback list
	    (setq nodes-freshened '())
	    (emtvr:newstyle receiver 
	       (emtg (type report)(what-test test-2)))

	    ;;Test that we have the right contents.  Skip for now
	    ;;because emtm doesn't have a set matcher yet.

	    ;;One (new) callback happened
	    (emtg:narrow ((what-test test-2))
	       (emtvr:th:assert-the-1-right-node 
		  nodes-freshened))

	    t)))


   ;;Report w/2 entries
   ;;
   '
   (  "Situation: Empty tree.
Operation: Add a report w/2 entries.
Response: List contains both entries."
      (emtg:with emt:testral:thd:examples ()
	 (let*
	    ((nodes-freshened '())
	       (remember-freshened-node
		  #'(lambda (x y)
		       (push (list x y) nodes-freshened)))
	       (receiver (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	    ;;List length.

	    ;;Matches the pattern - but we don't have a set-match pattern
	    ;;yet
	 
	    )))
   )

;;;_. Footers
;;;_ , Provides

(provide 'rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; rtest.el ends here
