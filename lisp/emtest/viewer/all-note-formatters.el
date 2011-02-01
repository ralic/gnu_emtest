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

(require 'emtest/viewer/view-types)
(require 'emtest/types/testral-types)
(require 'utility/misc)

;;;_. Body
;;;_ , Collecting formatters

;;;_  . emt:vw:note-alist
(defvar emt:vw:note-alist 
   '()
   "Alist from governor symbol to format function" )
;;;_  . emt:vw:note:add-gov
(defun emt:vw:note:add-gov (gov-symbol formatter)
   "Register FORMATTER governed by GOV-SYMBOL."
   (utim:new-apair 
      gov-symbol formatter emt:vw:note-alist))

;;;_  . emt:vw:note-get-formatter
(defun emt:vw:note-get-formatter (gov-symbol)
   "Get a relevant formatter function for GOV-SYMBOL.
Should not fail.
GOV-SYMBOL must be a symbol."
   (utim:assq-value
      gov-symbol 
      emt:vw:note-alist
      #'emt:vw:note-formatter-fallback))
;;;_ , Special formatters
;;;_  . emt:vw:note-formatter-fallback
(defun emt:vw:note-formatter-fallback (obj &rest r)
   ""
   (list "No formatter found for governor "
      (symbol-name 
	 (emt:testral:note->governor (emt:view:note->contents obj)))))
;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/all-note-formatters)

;;;_ , Include the autoload list of note formatters
;;Included with our feature already provided, so the load-forms know
;;they are supported.
(require 'emtest/viewer/note-formatters/registrations)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/all-note-formatters.el ends here
