;;;_ emtest/testhelp/testpoint/requirer.el --- Do requires for testpoint

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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

;; Testpoint wants a special require statement to make it less
;; intrusive.  


;;;_ , Requires


;;;_. Body

(defmacro emtp:require ()
   "Make suitable requires for testpoint"

   ;;Should examine compile flags, if we're compiling.
   ;;This is if file was not definitely wanting tp
   '(dont-compile
       (unless (require 'emtest/testhelp/testpoint nil t)
	  ;;And mark the testpoints as being dormant
	  (defmacro emtp (id args &rest rest) 
	     (declare (debug (symbolp (&rest form) body)))
	     `(progn ,@rest)))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/testpoint/requirer)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/testpoint/requirer.el ends here
