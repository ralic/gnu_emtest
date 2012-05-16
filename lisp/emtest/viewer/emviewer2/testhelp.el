;;;_ emtest/viewer/emviewer2/testhelp.el --- Testhelp for emviewer2

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

(require 'emtest/viewer/emviewer2)
(require 'emtest/launch/all/testhelp)


;;;_. Body

(defun emt:vw:top:ts:run-test (form)
   ""
   
   (emtt:ts:run-test form #'emt:vw:top:ts:run-test:callback))

;;;_ , The callback 
(defun emt:vw:top:ts:run-test:callback (report)
   ""
   
   (check-type report emt:testral:report)
   (emt:vw:top:tester-cb report))

;;;_ , emt:vw:top:ts:with-mock-viewer

(defmacro emt:vw:top:ts:with-mock-viewer (&rest body)
   ""
   
   `(with-temp-buffer
       (let
	  (
	     emt:vw:top:receiver
	     emt:vw:top:result-root
	     (emt:vw:top:report-buffer
		(current-buffer)))

	  ,@body)))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emviewer2/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer2/testhelp.el ends here
