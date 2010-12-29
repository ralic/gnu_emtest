;;;_ emtest/viewer/all-note-formatters.el --- Collect registered TESTRAL formatters

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

;;No extra libraries

;;;_. Body
;;;_ , Collecting formatters

;;;_  . emtvf:TESTRAL-gov-alist
(defvar emtvf:TESTRAL-gov-alist 
   '()
   "Alist from governor symbol to format function" )
;;;_  . emtvf:TESTRAL:add-gov
(defun emtvf:TESTRAL:add-gov (gov-symbol formatter)
   ""
   (unless (assq gov-symbol emtvf:TESTRAL-gov-alist)
      (push 
	 (list gov-symbol formatter)
	 emtvf:TESTRAL-gov-alist)))

;;;_  . emtvf:get-TESTRAL-formatter
(defun emtvf:get-TESTRAL-formatter (gov-symbol)
   "Get a relevant formatter function for GOV-SYMBOL.
Should not fail.
GOV-SYMBOL must be a symbol."
   (let* 
      ((cell
	  (assq gov-symbol emtvf:TESTRAL-gov-alist)))
      (if
	 cell
	 (second cell)
	 #'emtvf:TESTRAL-formatter-fallback)))
;;;_ , Special formatters
;;;_  . emtvf:TESTRAL-formatter-fallback
(defun emtvf:TESTRAL-formatter-fallback (obj &rest r)
   ""
   (list "No formatter found for governor "
      (symbol-name 
	 (emt:testral:note->governor (emt:view:TESTRAL->contents obj)))))
;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/all-note-formatters)

;;;_ , Include the autoload list of note formatters
;;Included with our feature already provided, so the load-forms know
;;they are supported.
(require 'emtest/viewer/testral/registrations)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/all-note-formatters.el ends here
