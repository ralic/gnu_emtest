;;;_ emtest/runners/plain.el --- Runner for "plain" test-cases

;;;_. Headers
;;;_ , License
;; Copyright (C) 2011  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp,maint,internal

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

(require 'emtest/types/testral-types)
(require 'emtest/testhelp/standard)
(require 'emtest/main/surrounders)
(require 'emtest/main/notes)

;;;_. Body
;;;_ , emt:runner:plain
;;;###autoload
(defun emt:runner:plain (props form report-f)
   "Run a plain test-case and report the result."
   (emtt:testral:with-context props
      (let
	 ( 
	    (form-1
	       (emt:sur:add-surrounders 
		  (car form) 
		  (emt:sur:get-surrounders props)
		  props)))
	 (emth:protect&trap
	    aborted-p
	    (eval form-1)
	    (progn
	       (when aborted-p
		  (emtt:testral:add-note
		     "problem"
		     'ungraded 
		     'error-raised
		     aborted-p))
	       (funcall report-f
		  (emt:testral:make-suite
		     :contents
		     (emtt:testral:note-list)
		     :grade 
		     (if aborted-p 
			'ungraded 
			'test-case))))))))

;;;_ , Register it
;;;###autoload (eval-after-load 'emtest/main/all-runners
;;;###autoload '(emt:runner:add 'nil #'emt:runner:plain
;;;###autoload   "Plain test-case runner"))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runners/plain)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runners/plain.el ends here
