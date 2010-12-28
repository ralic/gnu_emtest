;;;_ emtest/viewer/testral/scope.el --- TESTRAL formatter for scope

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
;;;_ , emtvf:TESTRAL-gov:scope
;;;###autoload
(defun emtvf:TESTRAL-gov:scope (obj)
   "Formatter for TESTRAL viewable governed by `scope'"
   (emtvf:shortcut-single
      obj
      '()
      (emtvf:grade-overall-face
	 (emt:view:presentable->sum-badnesses obj))
      '((sep 2) "No nested notes")))

;;;_. Footers
;;;_ , Register it
;;;###autoload (emtvf:TESTRAL:add-gov
;;;###autoload    'scope 
;;;###autoload    #'emtvf:TESTRAL-gov:scope)
;;;_ , Provides

(provide 'emtest/viewer/testral/scope)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/testral/scope.el ends here
