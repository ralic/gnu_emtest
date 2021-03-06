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
(require 'emtest/main/define)
(require 'emtest/viewer/view-types/testhelp)

;;;_. Body
;;;_ , emt:pth:grd:node-proper
(emt:deftest-3 emt:pth:grd:node-proper
   (nil
      (emtg:with emt:r:thd ()
	 (emtg:map result-name result-name
	    (emt:doc "Param: A known view-node.")
	    (emt:doc "Operation: Get own grade.")
	    (emt:doc "Response: The result matches what's expected.")
	    (let
	       ((role&test-tags
		   (car (emtg (type map:name->role&test-list)))))
	       (when (emtg (type has-viewnode-p))
		  (emt:assert
		     (equal
			(emt:pth:grd:subtree
			   (emtg:value+
			      `((type emt:r:alist-item) ,@role&test-tags)))
			(emtg:value+
			   `((type suite-own+notes-badness-list)
			       (name ,result-name)))))))))))

;;;_ , emt:grd:combine
;;$$WRITE MY EXAMPLES

;;;_ , emt:pth:grd:subtree
(emt:deftest-3 emt:pth:grd:subtree
   (nil
      (emtg:with emt:r:thd ()
	 (emtg:map result-name result-name
	    (emt:doc "Param: A known view-node.")
	    (emt:doc "Operation: Get subtree grade.")
	    (emt:doc "Response: The sum of grade is as expected.")
	    (let
	       ((role&test-tags
		   (car (emtg (type map:name->role&test-list)))))
	       (when (emtg (type has-viewnode-p))
		  (emt:assert
		     (equal
			(emt:pth:grd:subtree
			   (emtg:value+
			      `((type emt:r:alist-item) ,@role&test-tags)))
			(emtg:value+
			   `((type suite-own+notes-badness-list)
			       (name ,'test-bad)))))))))))

;;;_ , emt:pth:collect-testral
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
