;;;_ emtest/viewer/view-types/testhelp.el --- Testhelp file for view-types

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

(require 'emtest/viewer/view-types)
(require 'emtest/testhelp/match)
(require 'emtest/testhelp/tagnames)
(require 'emtest/types/testral-types/testhelp)
;;;_. Body
;;;_ , Match struct governors

;;;_  . emt:view:make-presentable
(emtm:define-struct-governor
   (emt:view:presentable
      (:constructor emt:view:make-presentable)
      (:conc-name emt:view:presentable->)
      (:include emtvp:node))
   children ;;Included
   sum-grades list)


;;;_  . emt:view:suite
(emtm:define-struct-governor 
   (emt:view:suite
      (:constructor emt:view:make-suite)
      (:conc-name emt:view:suite->)
      (:include emt:view:presentable))
   children ;;Included
   explorable
   presentation-path
   testrun-id
   result)

;;;_ , emtvr:thd
(defconst emtvr:thd
   (append
      emt:testral:thd:examples
      (emtg:define+ ()
	 (transparent-tags () (type role what-test))
	 (group
	    ((type emtvr:alist-item-pattern))
	    (type-must-be () (emtm:pattern emt:view:suite))
	    (item
	       ( (role original-add)
		  (what-test test-1))
	       (emtg:with 
		  (append emt:testral:thd:examples emtg:all-examples)
		  ()
		  (emtm:make-pattern
		     (emt:view:make-suite
			:result 
			(eval
			   '(emtg (type suite)(what-test test-1)(role original-add)))
			:explorable
			(eval 
			   '(emtg (type explorable)(what-test test-1)))
			;; 			:id
			;; 			(eval 
			;; 			   '(emtg (type how-to-run)(what-test test-1)))
			:presentation-path
			(eval 
			   '(emtg (type presentation-path)(what-test test-1)))
			:testrun-id 
			(eval 
			   '(emtg (type testrun-id)(role original-add)))))))

	    (item
	       ( (role replace)
		  (what-test test-1))
	       (emtg:with 
		  (append emt:testral:thd:examples emtg:all-examples)
		  ()
		  (emtm:make-pattern
		     (emt:view:make-suite
			:result 
			(eval
			   '(emtg (type suite)(what-test test-1)(role replace)))
			:explorable
			(eval 
			   '(emtg (type explorable)(what-test test-1)))
			;; 			:id
			;; 			(eval 
			;; 			   '(emtg (type how-to-run)(what-test test-1)))
			:presentation-path
			(eval 
			   '(emtg (type presentation-path)(what-test test-1)))
			:testrun-id 
			(eval 
			   '(emtg (type testrun-id)(role replace))))))))

	 (group
	    ((type emtvr:alist-item))
	    (type-must-be () emt:view:suite)
	    (item
	       ( (role original-add)
		  (what-test test-1))
	       (emtg:with 
		  (append emt:testral:thd:examples emtg:all-examples)
		  ()

		  (emt:view:make-suite
		     :result 
		     (eval
			'(emtg (type suite)(what-test test-1)(role original-add)))
		     :explorable
		     (eval 
			'(emtg (type explorable)(what-test test-1)))
		     :id
		     (eval 
			'(emtg (type how-to-run)(what-test test-1)))
		     :presentation-path
		     (eval 
			'(emtg (type presentation-path)(what-test test-1)))
		     :testrun-id 
		     (eval 
			'(emtg (type testrun-id)(role original-add))))))

	    (item
	       ( (role replace)
		  (what-test test-1))
	       (emtg:with 
		  (append emt:testral:thd:examples emtg:all-examples)
		  ()

		  (emt:view:make-suite
		     :result 
		     (eval
			'(emtg (type suite)(what-test test-1)(role replace)))
		     :explorable
		     (eval 
			'(emtg (type explorable)(what-test test-1)))
		     :id
		     (eval 
			'(emtg (type how-to-run)(what-test test-1)))
		     :presentation-path
		     (eval 
			'(emtg (type presentation-path)(what-test test-1)))
		     :testrun-id 
		     (eval 
			'(emtg (type testrun-id)(role replace)))))))

	 ))
   "View-types examples plus TESTRAL report examples."
   )


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/view-types/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/view-types/testhelp.el ends here
