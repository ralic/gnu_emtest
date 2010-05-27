;;;_ emtest/testhelp/mocks/libversion/qexamples.el --- quoted examples for libversion testing

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

;; Quoted examples for libversion, because it is not easy to automate
;; testing it with emtest, which it would be simultaneously assisting.


;;;_ , Requires

(require 'emtest/testhelp/mocks/libversion/tests)
(require 'emtest/testhelp/mocks/libversion)

;;;_. Body


;;An overall test of emtest/testhelp/mocks/libversion co-operating
;;with tester.

;;This is unprotected, but I can unload a feature if the global state
;;gets messed up.

;;If we automate this, use `emtts:th:insulate' from surrounders
;;testhelp and `emtmv:th:surrounders' from libversion testhelp.  And a
;;test-definition insulator?
(progn
   ;;Set up a module (Using the examples we already have)
   (emtg:with emtmv:th:data ()
      (emtmv:th:load))


   ;; Define a test:
   ;; * It expects the `old' values, though it will be run in (globally)
   ;;   `new' mode.
   (emt:deftest-3 
      ((of 'emtest/testhelp/mocks/libversion:th:suite)
	 (:surrounders 
	    '((emtg:with emtmv:th:data ()))))
      (nil
	 (progn
	    (emt:doc "Situation: Test is called in state `new' but we
have set up tester to use old version in test forms.")
	    (emt:doc "Response: Has the values of old version.")
	    (emtg:narrow ((which old))
	       (emtmv:th:check-all)))))

   ;;Set tester to use that module.
   ;;$$IMPROVE ME This should be provided by libversion.  It should
   ;;also emt:doc that old version is being used.
   (emtts:set-surrounder
      '(emtmv:with-version 'old nil))
   

   ;;Run tester, launch in `new' mode. (For now this is manual)
   (emtl:run-suite 'emtest/testhelp/mocks/libversion:th:suite)

   ;;Expect (via tester itself) the `old' results.
   )

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/libversion/qexamples)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/libversion/qexamples.el ends here
