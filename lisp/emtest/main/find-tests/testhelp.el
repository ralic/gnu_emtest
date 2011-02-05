;;;_ tester/tester/testhelp.el --- Testhelp for tester/tester

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
(require 'emtest/main/find-tests)
(require 'emtest/testhelp/deep-type-checker)

;;;_. Body
;;;_ , emt:test-finder:pending-list:check-type
(defun emt:test-finder:pending-list:check-type ()
   ""
   (emty:check
      emt:test-finder:pending-list
      (list emtt:pending)))


;;;_ , Test helpers
(defun emtt:th:explore-one (form callback &optional testrun-id)
   ""
   ;;Test thru top call for now.
   (emt:lch:run
      `(form ,form)
      (list "form")
      callback
      (or testrun-id "0")))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/find-tests/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/tester/testhelp.el ends here
