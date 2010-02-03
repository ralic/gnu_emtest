;;;_ emviewer/testhelp.el --- Emviewer testhelp

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: 

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(require 'emviewer)
(require 'tester/tester)

;;;_. Body

(defun emtest:ts:run-test (form)
   ""
   
   ;;First validate that form is right.  There should be a type for
   ;;this, in test-support for emt-define.  For now, expect a string.
   (check-type form
      (list string t))
   
   (emt:test:ts:run-test form #'emtest:ts:run-test:callback))

;;;_ , The callback 
;;Maybe should be anonymous
(defun emtest:ts:run-test:callback (report)
   ""
   
   (check-type report emt:testral:report)
   (emtest:viewer:receive report))

;;;_. Footers
;;;_ , Provides

(provide 'emviewer/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emviewer/testhelp.el ends here
