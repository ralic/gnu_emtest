;;;_ emtest/viewer/note-formatters/doc.el --- TESTRAL formatter for doc

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
;;;_ , emt:vw:note:doc
;;;###autoload
(defun emt:vw:note:doc (note doc)
   "Formatter for TESTRAL note governed by `doc'"
   ;;$$IMPROVE ME Don't let headlines grow too long.  Long strings and
   ;;long singles-paths are major culprits.
   (cond
      ((not (string-match "\n" doc))
	 (emt:fmt:outline-item-emformat
	     doc nil))
      ((string-match ": " doc)
	 (emt:fmt:outline-item-emformat
	     (substring doc 0 (match-end 0))
	     (substring doc (match-end 0))))
      (t
	 (emt:fmt:outline-item-emformat
	     "Doc" doc))))

;;;_. Footers
;;;_ , Register it
;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'doc 
;;;###autoload    #'emt:vw:note:doc))
;;;_ , Provides

(provide 'emtest/viewer/note-formatters/doc)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/note-formatters/doc.el ends here
