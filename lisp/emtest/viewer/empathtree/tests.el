;;;_ emtest/viewer/empathtree/tests.el --- Tests for empathtree

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

(require 'emtest/viewer/empathtree)

;;;_. Body
;;;_ , emtvr:badnesses:get-own
(emt:deftest-3 emtvr:badnesses:get-own
   ;;Irrelevant for the moment because no example suites have
   ;;intrinsic badnesses yet.
   ;;$$WRITE MY EXAMPLES
   '
   (nil
      (emtg:with emtvr:thd ()
	 (emtg:narrow ((role original-add) (what-test test-1))
	    (emt:doc "Param: A known view-node.")
	    (emt:doc "Operation: Get subtree badnesses.")
	    (emt:doc "Response: The sum of badnesses is as expected.")
	    (assert
	       (equal
		  (emtvr:badnesses:get-own
		     (emtg 
			(type emtvr:alist-item)))
		  (emtg (type suite-own-badness-list)))
	       t)))))

;;;_ , emtvr:combine-badnesses
;;$$WRITE MY EXAMPLES

;;;_ , emtvr:get-subtree-badnesses
(emt:deftest-3 emtvr:get-subtree-badnesses
   (nil
      (emtg:with emtvr:thd ()
	 ;;$$IMPROVE ME Might split suite examples etc by indexing
	 ;;mode, so we can iterate over suites.
	 (emtg:narrow ((role original-add) (what-test test-1))
	    (emt:doc "Param: A known view-node.")
	    (emt:doc "Operation: Get subtree badnesses.")
	    (emt:doc "Response: The sum of badnesses is as expected.")
	    (assert
	       (equal
		  (emtvr:get-subtree-badnesses
		     (emtg 
			(type emtvr:alist-item)))
		  (emtg (type suite-badness-list)))
	       t)))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/empathtree/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/empathtree/tests.el ends here
