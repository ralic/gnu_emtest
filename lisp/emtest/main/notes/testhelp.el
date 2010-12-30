;;;_ emtest/main/notes/testhelp.el --- Testhelp for testral notes

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

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

(require 'emtest/main/notes)

;;;_. Body
;;;_ , emtt:testral:th:abbrev-note-list
(defun emtt:testral:th:abbrev-note-list (note-list)
   ""
   
   (let*
      ((note-list
	  (if (emt:testral:note-list-p note-list)
	     (emt:testral:note-list->notes note-list)
	     note-list)))
      (mapcar
	 #'(lambda (note)
	      (list 
		 (emt:testral:note->prestn-path note)
		 (emt:testral:note->parent-id   note)
		 (emt:testral:note->id          note)
		 (emt:testral:note->governor    note)))
	 note-list)))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/notes/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/main/notes/testhelp.el ends here
