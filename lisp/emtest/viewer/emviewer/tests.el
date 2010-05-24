;;;_ emtest/viewer/emviewer/tests.el --- Emtest tests for Emviewer

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

(require 'emtest/runner/define)
(require 'emtest/viewer/emviewer)
(require 'emtest/viewer/emviewer/testhelp)
(require 'emtest/common/testral-types/testhelp)
(require 'emtest/testhelp/persist)
(require 'emtest/testhelp/eg)
(require 'emtest/testhelp/mocks/filebuf)

;;;_. Body
;;;_ , emtest/viewer/emviewer
(emt:deftest-3
   ((of 'emtest/viewer/emviewer)
      (db-id
	 `(persist ,emtve:td:dir))
      (:surrounders
	 '((emt:eg:with emt:testral:thd:examples ()))))
   
   ;;Tests of just the viewer.
   (()
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emt:eg (type report)(name just-test-runner)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist 
	       "dbid:yygeryl0jwe0"))))
   
   (()
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emt:eg (type report)(role original-add)(what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:mf660gq0jwe0"))))


   (()
      (emtve:ts:with-mock-viewer
	 (emtve:tester-cb
	    (emt:eg (type report)(what-test test-2)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:l7w6gjq0jwe0"))))
   
   ;;End to end test
   (()

      ;;And capture the intermediate result stuff - gotta rewrite,
      ;;basically like above or like rewriting "receive" and also
      ;;testing it with set=.
      (emtve:ts:with-mock-viewer
	 (emtve:ts:run-test
	    '("Situation: testing an example" 
		(error "An example error")))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:6y9kxjq0jwe0")))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emviewer/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer/tests.el ends here
