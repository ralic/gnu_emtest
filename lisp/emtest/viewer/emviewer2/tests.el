;;;_ emtest/viewer/emviewer2/tests.el --- Tests for emviewer2

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

(require 'emtest/main/define)
(require 'emtest/viewer/emviewer2)
(require 'emtest/viewer/emviewer2/testhelp)
(require 'emtest/testhelp/persist)
(require 'emtest/testhelp/tagnames)
(require 'emtest/types/testral-types/testhelp)

;;;_. Body
;;;_ , emt:vw:top:td:dir
;;$$CHECK ME  This was moved to here, needs rechecking.
(defconst emt:vw:top:td:dir
   (emtb:expand-filename-by-load-file "persist")
   "The file where the database is located" )

;;;_ , emtest/viewer/emviewer2
(emt:deftest-3
   ((of 'emtest/viewer/emviewer2)
      (db-id
	 `(persist ,emt:vw:top:td:dir))
      (:surrounders
	 '((emtg:with emt:testral:thd:examples ()))))
   
   ;;Tests of just the viewer.
   (()
      (emt:vw:top:ts:with-mock-viewer
	 (emt:vw:top:tester-cb
	    (emtg (type report)(name just-test-runner)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist 
	       "dbid:yygeryl0jwe0"))))
   
   (()
      (emt:vw:top:ts:with-mock-viewer
	 (emt:vw:top:tester-cb
	    (emtg (type report)(role original-add)(what-test test-1)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:mf660gq0jwe0"))))


   (()
      (emt:vw:top:ts:with-mock-viewer
	 (emt:vw:top:tester-cb
	    (emtg (type report)(what-test test-2)))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:l7w6gjq0jwe0"))))
   
   ;;End to end test
   (()

      ;;And capture the intermediate result stuff - gotta rewrite,
      ;;basically like above or like rewriting "receive" and also
      ;;testing it with set=.
      (emt:vw:top:ts:with-mock-viewer
	 (emt:vw:top:ts:run-test
	    '("Situation: testing an example" 
		(error "An example error")))

	 (emt:emviewer:th:check-buffer-string
	    (emt:persist "dbid:6y9kxjq0jwe0")))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emviewer2/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer2/tests.el ends here
