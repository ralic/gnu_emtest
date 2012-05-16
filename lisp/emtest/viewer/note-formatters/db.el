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
;;;_ , Helpers
;;;_  . emt:vw:note:accept-in-db
(defun emt:vw:note:accept-in-db (button)
   "Accept a value in db, as given by BUTTON."
   (let
      ((args (button-get button 'accept-in-db-args)))
      (apply #'emt:db:set-value args)))
;;;_  . emt:vw:note:ediff-string-w/persist
;;$$MOVE ME, RENAME ME  It's not specific to this formatter.
(defun emt:vw:note:ediff-string-w/persist (value backend id)
   "Compare VALUE with stored value ID in BACKEND"
   (let 
      ;;Make a buffer for each

      ;;$$IMPROVE ME Give user a means of expiring these buffers,
      ;;short of manually killing each one.  Perhaps `ediff-quit-hook'
      ;;or `ediff-cleanup-hook'.
      ((buf-expected (generate-new-buffer "*Emtest expected*"))
	 (buf-got    (generate-new-buffer "*Emtest got*")))
      ;;Put each into its buffer 
      (with-current-buffer buf-expected
	 (insert (emt:db:get-value backend id 'correct-answer)))
      (with-current-buffer buf-got
	 (insert value))
      ;;Run ediff on those buffers.
      ;;$$IMPROVE ME Provide a means of setting the comparison value
      ;;from here.  `ediff-save-buffer' almost does if only it could call
      ;;an alternative to `save-buffer'
      (ediff-buffers buf-expected buf-got)))
;;;_  . emt:vw:note:bfunc:ediff
;;$$IMPROVE ME Encap the call-with-args action.
(defun emt:vw:note:bfunc:ediff (button)
   "Button action: Compare with stored value"
   
   (let
      ((args (button-get button 'ediff-args)))
      (apply #'emt:vw:note:ediff-string-w/persist args)))

;;;_ , not-in-db
;;;_  . emt:vw:note:not-in-db
;;;###autoload
(defun emt:vw:note:not-in-db (note value id backend)
   "Formatter for TESTRAL note governed by `not-in-db'"
   (emt:fmt:outline:item-emformat
      "ID not in database "
      `(
	  ,(emt:fmt:outline:item-emformat
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
      
      'emt:view:face:ungraded))
;;;_  . Register it
;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'not-in-db 
;;;###autoload    #'emt:vw:note:not-in-db))

;;;_ , comparison-w/persist
;;;_  . emt:vw:note:comparison-w/persist
;;;###autoload
(defun emt:vw:note:comparison-w/persist (note matched-p value backend id)
   "Formatter for TESTRAL note governed by `comparison-w/persist'"

   ;;$$IMPROVE ME Add buttons & command to act on the persisting
   ;;object: If rejected, to accept it (Done).  To edit it and save
   ;;the new version as acceptable.  To diff the value with it.

   (emt:fmt:outline:item-emformat
      (list
	 (if matched-p 
	    "Matched"
	    "Mismatched")
	 " persisting object "
	 (if matched-p 
	    '()
	    `(
		(button "[Accept]"
		   action ,#'emt:vw:note:accept-in-db
		   help-echo "Accept the new value"
		   accept-in-db-args (,backend ,id ,value correct-answer))
		" "
		(button "[Compare]"
		   action ,#'emt:vw:note:bfunc:ediff
		   ediff-args (,value ,backend ,id)
		   help-echo "Compare to the accepted value"))))
      (list
	 (emt:fmt:obj-or-string value)) 
      (if matched-p 
	 'emt:view:face:ok-match
	 'emt:view:face:mismatch)
      matched-p))
;;;_  . Register it
;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'comparison-w/persist 
;;;###autoload    #'emt:vw:note:comparison-w/persist))


;;;_. Footers
;;;_ , Postlude (Hack because we `require' the target file)
;;;###autoload (provide 'emtest/viewer/note-formatters/registrations)

;;;_ , Provides

(provide 'emtest/viewer/note-formatters/not-in-db)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/note-formatters/not-in-db.el ends here
