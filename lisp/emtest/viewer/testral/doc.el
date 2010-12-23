;;;_ emtest/viewer/testral/doc.el --- TESTRAL formatter for doc

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
;;;_ , emtvf:TESTRAL-gov:doc
;;;###autoload
(defun emtvf:TESTRAL-gov:doc (note doc)
   "Formatter for TESTRAL note governed by `doc'"
   (cond
      ((not (string-match "\n" doc))
	 (emtvf:outline-item-2
	     doc nil (1+ depth)))
      ((string-match ": " doc)
	 (emtvf:outline-item-2
	     (substring doc 0 (match-end 0))
	     (substring doc (match-end 0))
	     (1+ depth)))
      (t
	 (emtvf:outline-item-2
	     "Doc" doc (1+ depth)))))

;;;_. Footers
;;;_ , Register it
;;;###autoload (emtvf:TESTRAL:add-gov
;;;###autoload    'doc 
;;;###autoload    #'emtvf:TESTRAL-gov:doc)
;;;_ , Provides

(provide 'emtest/viewer/testral/doc)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/testral/doc.el ends here
