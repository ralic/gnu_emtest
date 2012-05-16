;;;_ emtest/viewer/note-formatters/fail.el --- TESTRAL formatter for failed and succeeded

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
;;;_ , emt:vw:note:failed
;;$$IMPROVE ME Factor these functions.
;;;###autoload
(defun emt:vw:note:failed (note form)
   "Formatter for TESTRAL note governed by `failed'"
   ;;$$IMPROVE ME  Take assert args as params, print them.
   (emt:fmt:outline-item-emformat
      "Failed assertion"
      (list
	 (emt:fmt:obj-or-string form)
	 (emt:fmt:mapnodes
	    (emtvp:node->children note) 
	    nil))
      'emt:view:face:failed))

;;;_ , emt:vw:note:succeeded
;;;###autoload
(defun emt:vw:note:succeeded (note form)
   "Formatter for TESTRAL note governed by `succeeded'"
   ;;$$IMPROVE ME  Take assert args as params, print them.
   (emt:fmt:outline-item-emformat
      "Assertion succeeded"
      (list
	 (emt:fmt:obj-or-string form)
	 (emt:fmt:mapnodes
	    (emtvp:node->children note) 
	    nil)) 
      'emt:view:face:ok
      t))
;;;_ , emt:vw:note:mismatched
;;;###autoload
(defun emt:vw:note:mismatched (note form)
   "Formatter for TESTRAL note governed by `mismatched'"
   (emt:fmt:outline-item-emformat
      "Mismatched"
      (list
	 (emt:fmt:obj-or-string form)
	 (emt:fmt:mapnodes
	    (emtvp:node->children note) 
	    nil)) 
      'emt:view:face:mismatch))


;;;_ , emt:vw:note:matched
;;;###autoload
(defun emt:vw:note:matched (note form)
   "Formatter for TESTRAL note governed by `matched'"
   (emt:fmt:outline-item-emformat
      "Matched"
      (list
	 (emt:fmt:obj-or-string form)
	 (emt:fmt:mapnodes
	    (emtvp:node->children note) 
	    nil)) 
      'emt:view:face:ok-match
      t))


;;;_. Footers
;;;_ , Register them
;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'failed 
;;;###autoload    #'emt:vw:note:failed))

;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'succeeded
;;;###autoload    #'emt:vw:note:succeeded))

;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'mismatched 
;;;###autoload    #'emt:vw:note:mismatched))

;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'matched 
;;;###autoload    #'emt:vw:note:matched))

;;;_ , Provides

(provide 'emtest/viewer/note-formatters/fail)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/note-formatters/fail.el ends here

