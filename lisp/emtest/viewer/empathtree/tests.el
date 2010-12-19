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
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/tagnames)
(require 'emtest/runner/define)
(require 'emtest/viewer/view-types/testhelp)

;;;_. Body
;;;_ , emtvr:badnesses:get-own
(emt:deftest-3 emtvr:badnesses:get-own
   (nil
      (emtg:with emtvr:thd ()
	 (emtg:map result-name result-name
	    (emt:doc "Param: A known view-node.")
	    (emt:doc "Operation: Get own badnesses.")
	    (emt:doc "Response: The result matches what's expected.")
	    (let
	       ((role&test-tags
		   (car (emtg (type map:name->role&test-list)))))
	       (when (emtg (type has-viewnode-p))
		  (emt:assert
		     (equal
			(emtvr:get-subtree-badnesses
			   (emtg:value+
			      `((type emtvr:alist-item) ,@role&test-tags)))
			(emtg:value+
			   `((type suite-own+notes-badness-list)
			       (name ,result-name)))))))))))

;;;_ , emtvr:combine-badnesses
;;$$WRITE MY EXAMPLES

;;;_ , emtvr:get-subtree-badnesses
(emt:deftest-3 emtvr:get-subtree-badnesses
   (nil
      (emtg:with emtvr:thd ()
	 (emtg:map result-name result-name
	    (emt:doc "Param: A known view-node.")
	    (emt:doc "Operation: Get subtree badnesses.")
	    (emt:doc "Response: The sum of badnesses is as expected.")
	    (let
	       ((role&test-tags
		   (car (emtg (type map:name->role&test-list)))))
	       (when (emtg (type has-viewnode-p))
		  (emt:assert
		     (equal
			(emtvr:get-subtree-badnesses
			   (emtg:value+
			      `((type emtvr:alist-item) ,@role&test-tags)))
			(emtg:value+
			   `((type suite-own+notes-badness-list)
			       (name ,'test-bad)))))))))))

;;;_ , emtvr:collect-testral
;;Tree is a parameter so it needn't be specially insulated.

;;Examples.



;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/empathtree/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/empathtree/tests.el ends here
