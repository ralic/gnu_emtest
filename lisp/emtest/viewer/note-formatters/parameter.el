;;;_ emtest/viewer/note-formatters/parameter.el --- TESTRAL formatter for parameter

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

(require 'emtest/viewer/emformat)


;;;_. Body
;;;_ , emt:vw:note:parameter
;;;###autoload
(defun emt:vw:note:parameter (note arg val)
   "Formatter for TESTRAL note governed by `parameter'"
   
   (cond
      ((and (emt:fmt:short-obj-p arg) (emt:fmt:short-obj-p val))
	 (emt:fmt:outline:item-emformat
	    `((small-object ,arg nil)
		" => "
		(small-object ,val nil))
	    nil))
      ((emt:fmt:short-obj-p arg)
	 (emt:fmt:outline:item-emformat
	    `(small-object ,arg nil)
	    (emt:fmt:obj-or-string val)))
      
      (t
	 (emt:fmt:outline:item-emformat
	    "Param"
	    `((object ,arg nil)
		" => "
		,(emt:fmt:obj-or-string val))))))


;;;_. Footers
;;;_ , Register it
;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'parameter 
;;;###autoload    #'emt:vw:note:parameter))
;;;_ , Provides

(provide 'emtest/viewer/note-formatters/parameter)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/note-formatters/parameter.el ends here
