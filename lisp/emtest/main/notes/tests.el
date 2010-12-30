;;;_ emtest/main/notes/tests.el --- Tests for emtest/main/notes

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

(require 'emtest/main/notes)

;;;_. Body
;;;_ , Insulation
(defconst emtest/main/notes:th:surrounders 
   '((let 
	()))
   
   "The normal surrounders for emtest/main/notes tests" )
;;;_ , emtt:testral:continued-with
(emt:deftest-3
   ((of 'emtt:testral:continued-with)
      (:surrounders emtest/main/notes:th:surrounders))
   
   (nil
      (let*
	 ((obj 
	     (progn
		(emt:doc "Operation: Make a continuing object.")
		(emtt:testral:make-continuing)))
	    (note-list
	       (progn
		  	 
		  (emt:doc "Operation: Use it once, adding one note.")
		  (emtt:testral:continued-with obj
		     (emtt:testral:add-note
			"example" nil 'example 12))
		  
	 
		  (emt:doc "Operation: Use it again, adding another note.")
		  (emtt:testral:continued-with obj
		     (emtt:testral:add-note
			"example" nil 'example 144)
		     (emtt:testral:get-notes)))))
	 
	 (emt:assert
	    (emth:sets= 
	       (mapcar #'emt:testral:note->value note-list)
	       (list '(12) '(144)))))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/notes/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/main/notes/tests.el ends here
