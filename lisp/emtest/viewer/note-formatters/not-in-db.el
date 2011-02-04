;;;_ emtest/viewer/note-formatters/not-in-db.el --- TESTRAL formatter for not-in-db

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

(require 'emtest/viewer/emformat)


;;;_. Body
;;;_ , emt:vw:note:accept-in-db
(defun emt:vw:note:accept-in-db (button)
   "Accept a value in db, as given by BUTTON."
   (let
      ((arg (button-get button 'accept-in-db-args)))
      (emdb:set-value
	 ',backend
	 ',id
	 ',value
	 'correct-answer)))
;;;_ , emt:vw:note:not-in-db
;;;###autoload
(defun emt:vw:note:not-in-db (note value id backend)
   "Formatter for TESTRAL note governed by `not-in-db'"
   (emtvf:outline-item-emformat
      "ID not in database "
      `(
	  ,(emtvf:outline-item-emformat
	      (list
		 "Value "
		 `(button "[Accept]"
		     action ,#'emt:vw:note:accept-in-db
		     help-echo "Accept this value"
		     accept-in-db-args (,backend ,id ,value correct-answer)))
	      (if
		 (stringp value)
		 ;;Indent it so it can't affect outline
		 ;;structure. 
		 `(indent 4 ,value)
		 `(object ,value nil))
	      ;;Sometimes fold it.  Say, if it's not a string or is a
	      ;;long string.
	      (or
		 (not (stringp value))
		 (> (length value) 100))))
      
      'emtvf:face:ungraded))
;;;_. Footers
;;;_ , Register it
;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'not-in-db 
;;;###autoload    #'emt:vw:note:not-in-db))


;;;_ , Provides

(provide 'emtest/viewer/note-formatters/not-in-db)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/note-formatters/not-in-db.el ends here
