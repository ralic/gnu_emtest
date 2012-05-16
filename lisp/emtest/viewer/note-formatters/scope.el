;;;_ emtest/viewer/note-formatters/scope.el --- TESTRAL formatter for scope

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
;;;_ , emt:vw:note:scope
;;;###autoload
(defun emt:vw:note:scope (note &optional name)
   "Formatter for TESTRAL viewable governed by `scope'"
   (emt:fmt:shortcut-single
      name
      (pathtree:node->children note)
      '()
      (emt:fmt:grade-overall-face
	 (emt:view:presentable->sum-grades note))
      '((sep 2) "No nested notes")
      (emt:fmt:grade-boring (emt:view:presentable->sum-grades note))))



;;;_. Footers
;;;_ , Register it
;;;###autoload (eval-after-load 'emtest/viewer/all-note-formatters
;;;###autoload '(emt:vw:note:add-gov
;;;###autoload    'scope 
;;;###autoload    #'emt:vw:note:scope))
;;;_ , Provides

(provide 'emtest/viewer/note-formatters/scope)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/note-formatters/scope.el ends here
