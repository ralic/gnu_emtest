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
(require 'emtest/common/testral-types/testhelp)
(require 'emtest/testhelp/match)

;;;_. Body
;;;_  . emtvr:one-newstyle
(put 'emtvr:one-newstyle 'emt:test-thru 'emtvr:newstyle)

;;;_  . emtvr:suite-newstyle test helpers

(emtm:define-struct-governor
   emtvr:suite-newstyle
   id how-to-run presentation-path testrun-id suite)

;;;_  . Tests

(emt:deftest-3 emtvr:newstyle
   (nil
      (progn
	 (emt:doc "Situation: Empty report.")
	 (emt:doc "Response: List still contains nothing.")
	 (emt:eg:with emt:testral:thd:examples nil
	    (let*
	       ((nodes-freshened 'nil)
		  (remember-freshened-node
		     #'(lambda
			  (x y)
			  (push
			     (list x y)
			     nodes-freshened)))
		  (receiver
		     (emtvr:make-empty-alist remember-freshened-node #'ignore))
		  (report
		     (emt:eg
			(project emtest)
			(sub-project testral)
			(library types)
			(type report)
			(name empty))))
	       (emtvr:newstyle receiver report)
	       (assert
		  (equal
		     (emtvr:data-alist receiver)
		     'nil)
		  t)
	       (assert
		  (emtm nodes-freshened
		     (list))
		  t)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: Have added a report w/1 entry")
	 (emt:doc "Response: List contains that one entry.")
	 (emt:eg:with emt:testral:thd:examples nil
	    (let*
	       ((nodes-freshened 'nil)
		  (remember-freshened-node
		     #'(lambda
			  (x y)
			  (push
			     (list x y)
			     nodes-freshened)))
		  (receiver
		     (emtvr:make-empty-alist remember-freshened-node #'ignore))
		  (report
		     (emt:eg
			(project emtest)
			(sub-project testral)
			(library types)
			(type report)
			(role original-add)
			(what-test test-1))))
	       (emtvr:newstyle receiver report)
	       (assert
		  (emtm
		     (emtvr:data-alist receiver)
		     (list
			(eval
			   '(emt:eg
			       (type receive-alist-item)
			       (role original-add)
			       (what-test test-1)))))
		  t)
	       (assert
		  (=
		     (length
			(emtvr:data-alist receiver))
		     1)
		  t)
	       (assert
		  (emtm nodes-freshened
		     (list
			(list
			   (eval
			      '(emt:eg
				  (type presentation-path)
				  (what-test test-1)))
			   (make-emtvr:suite-newstyle 
			      :presentation-path
			      (eval
				 '(emt:eg
				     (type presentation-path)
				     (what-test test-1)))
			      :result
			      (eval
				 '(emt:eg
				     (type suite)
				     (what-test test-1)
				     (role original-add)))))))
		  t)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: Have added a report w/1 entry.")
	 (emt:doc "Operation:  Another report with a different result for the same test.")
	 (emt:doc "Response: List contains just that one entry, not duplicated.")
	 (emt:eg:with emt:testral:thd:examples nil
	    (let*
	       ((nodes-freshened 'nil)
		  (remember-freshened-node
		     #'(lambda
			  (x y)
			  (push
			     (list x y)
			     nodes-freshened)))
		  (receiver
		     (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	       (emtvr:newstyle receiver
		  (emt:eg
		     (project emtest)
		     (sub-project testral)
		     (library types)
		     (type report)
		     (what-test test-1)
		     (role original-add)))
	       (setq nodes-freshened 'nil)
	       (emtvr:newstyle receiver
		  (emt:eg
		     (project emtest)
		     (sub-project testral)
		     (library types)
		     (type report)
		     (what-test test-1)
		     (role replace)))
	       (assert
		  (emtm
		     (emtvr:data-alist receiver)
		     (list
			(eval
			   '(emt:eg
			       (type receive-alist-item)
			       (role replace)
			       (what-test test-1)))))
		  t)
	       (assert
		  (=
		     (length
			(emtvr:data-alist receiver))
		     1)
		  t)
	       (assert
		  (emtm nodes-freshened
		     (list
			(list
			   (eval
			      '(emt:eg
				  (type presentation-path)
				  (what-test test-1)))
			   (make-emtvr:suite-newstyle 
			      :presentation-path
			      (eval
				 '(emt:eg
				     (type presentation-path)
				     (what-test test-1)))
			      :result
			      (eval
				 '(emt:eg
				     (type suite)
				     (what-test test-1)
				     (role replace)))))))
		  t)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: Have added a report w/1 entry.")
	 (emt:doc "Operation: Report removes previous report.")
	 (emt:doc "Response: List no longer contains that entry; it is empty.")
	 (emt:eg:with emt:testral:thd:examples
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
		     (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	       (emtvr:newstyle receiver
		  (emt:eg
		     (type report)
		     (role original-add)
		     (what-test test-1)))
	       (emtvr:newstyle receiver
		  (emt:eg
		     (type report)
		     (role remove-previous)
		     (what-test test-1)))
	       (assert
		  (equal
		     (emtvr:data-alist receiver)
		     'nil)
		  t)
	       t))))
   (nil
      (progn
	 (emt:doc "Situation: Have added a report w/1 entry.")
	 (emt:doc "Operation: Add a second report")
	 (emt:doc "Response: List contains both entries.")
	 (emt:eg:with emt:testral:thd:examples
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
		     (emtvr:make-empty-alist remember-freshened-node #'ignore)))
	       (emtvr:newstyle receiver
		  (emt:eg
		     (type report)
		     (role original-add)
		     (what-test test-1)))
	       (setq nodes-freshened 'nil)
	       (emtvr:newstyle receiver
		  (emt:eg
		     (type report)
		     (what-test test-2)))
	       (assert
		  (emtm nodes-freshened
		     (list
			(list
			   (eval
			      '(emt:eg
				  (type presentation-path)
				  (what-test test-2)))
			   (make-emtvr:suite-newstyle 
			      :presentation-path
			      (eval
				 '(emt:eg
				     (type presentation-path)
				     (what-test test-2)))
			      :result
			      (eval
				 '(emt:eg
				     (type suite)
				     (what-test test-2)))))))
		  t)
	       t))))
   '(nil
       (progn
	  (emt:doc "Situation: Empty tree.")
	  (emt:doc "Operation: Add a report w/2 entries.")
	  (emt:doc "Response: List contains both entries.")
	  (emt:eg:with emt:testral:thd:examples nil
	     (let*
		((nodes-freshened 'nil)
		   (remember-freshened-node
		      #'(lambda
			   (x y)
			   (push
			      (list x y)
			      nodes-freshened)))
		   (receiver
		      (emtvr:make-empty-alist remember-freshened-node #'ignore))))))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/receive/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/receive/tests.el ends here
