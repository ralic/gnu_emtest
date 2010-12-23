;;;_ emtest/viewer/testral/fail.el --- TESTRAL formatter for failed and succeeded

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
;;;_ , emtvf:TESTRAL-gov:failed
;;;###autoload
(defun emtvf:TESTRAL-gov:failed (note form)
   "Formatter for TESTRAL note governed by `failed'"
   ;;$$IMPROVE ME  Take assert args as params, print them.
   (emtvf:outline-item
      "Failed assertion"
      (emtvf:obj-or-string form) 
      'emtvf:face:failed))

;;;_ , emtvf:TESTRAL-gov:succeeded
;;;###autoload
(defun emtvf:TESTRAL-gov:succeeded (note form)
   "Formatter for TESTRAL note governed by `succeeded'"
   ;;$$IMPROVE ME  Take assert args as params, print them.
   (emtvf:outline-item
      "Assertion succeeded"
      (emtvf:obj-or-string form) 
      'emtvf:face:ok
      t))

;;;_. Footers
;;;_ , Register then
;;;###autoload (emtvf:TESTRAL:add-gov
;;;###autoload    'failed 
;;;###autoload    #'emtvf:TESTRAL-gov:failed)

;;;###autoload (emtvf:TESTRAL:add-gov
;;;###autoload    'succeeded
;;;###autoload    #'emtvf:TESTRAL-gov:succeeded)

;;;_ , Provides

(provide 'emtest/viewer/testral/fail)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/testral/fail.el ends here

