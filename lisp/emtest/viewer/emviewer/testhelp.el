;;;_ emtest/viewer/emviewer/testhelp.el --- Emviewer testhelp

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


(require 'emtest/viewer/emviewer)
'(require 'emtest/runner/tester) ;;Not needed.  We go thru tester/launch.
(require 'emtest/runner/launch)

;;;_. Body
;;$$RENAME ME emtt:ts:run-test-x ?
(defun emtest:ts:run-test (form)
   ""
   
   ;;First validate that form is right.  There should be a type for
   ;;this, in test-support for emt-define.  For now, expect a string.
   (check-type form (list string t))
   
   (emtt:ts:run-test form #'emtest:ts:run-test:callback))

;;;_ , The callback 
(defun emtest:ts:run-test:callback (report)
   ""
   
   (check-type report emt:testral:report)
   (emtest:viewer:receive report))

;;;_ , emtve:ts:with-mock-viewer

(defmacro emtve:ts:with-mock-viewer (&rest body)
   ""
   
   `(with-temp-buffer
       (let
	  (
	     emtve:receiver
	     emtve:chewie
	     emtve:result-root
	     (emtve:report-buffer
		(current-buffer)))

	  ,@body)))

;;;_ , emt:emviewer:th:check-buffer-string
(defun emt:emviewer:th:check-buffer-string (id)
   "Check that current buffer's contents matches what ID identifies."
   ;;Can set the current result to persist by evalling this in buffer.
   ;;$$FIXME:  This still inserts each object twice.
   '(emt:db:set id 'correct-answer (buffer-string))

   ;;This works, after having set the persisting object.
   (let
      ((contents-matches-p
	  (equal
	     (buffer-string)
	     (condition-case err
		(emt:persist:value id)
		;;For now, can't be more specific than `error'
		(error
		   (message "Couldn't get persisting value")
		   (recursive-edit))))))

      (unless contents-matches-p
	 (message "Buffer string does not match")
	 ;;This is just for my manual handling.
      
	 ;;Font-locking via here doesn't work.
	 (recursive-edit))
   
      ;;Would like this + definition to be the whole form, but for now
      ;;we can't.
      (assert
	 (progn contents-matches-p)
	 t))
   t)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/emviewer/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer/testhelp.el ends here
