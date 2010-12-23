;;;_ emtest/viewer/testral/compare-w-persist.el --- TESTRAL formatter for comparison-w/persist

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
;;;_ , emtvf:TESTRAL-gov:comparison-w/persist
;;;###autoload
(defun emtvf:TESTRAL-gov:comparison-w/persist (note matched-p value backend id)
   "Formatter for TESTRAL note governed by `comparison-w/persist'"

   ;;$$IMPROVE ME Add buttons & command to act on the persisting
   ;;object: If rejected, to accept it.  To edit it and save the new
   ;;version as acceptable.  To diff the value with it.

   (emtvf:outline-item
      (list
	 (if matched-p 
	    "Matched"
	    "Mismatched")
	 " persisting object"
	 (if matched-p 
	    '()
	    (emtvf:button " [Accept]"
	       `(lambda ()
		   (interactive)
		   (emdb:set-value
		      ',backend
		      ',id
		      ',value
		      'correct-answer))
	       '(help-echo "Accept the new value"))))

      (list
	 (emtvf:obj-or-string value)) 
      (if matched-p 
	 'emtvf:face:ok-match
	 'emtvf:face:mismatch)
      matched-p))

;;;_. Footers
;;;_ , Register it
;;;_  . Prelude
;;;###autoload (unless (fboundp 'emtvf:TESTRAL:add-gov)
;;;###autoload   (error "emtest/viewer/all-note-formatters must be loaded"))

;;;_  . Registration proper
;;;###autoload (emtvf:TESTRAL:add-gov
;;;###autoload    'comparison-w/persist 
;;;###autoload    #'emtvf:TESTRAL-gov:comparison-w/persist)
;;;_  . Postlude
;;;###autoload (provide 'emtest/viewer/testral/registrations)

;;;_ , Provides

(provide 'emtest/viewer/testral/compare-w-persist)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/testral/compare-w-persist.el ends here
