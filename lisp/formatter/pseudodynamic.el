;;;_ formatter/pseudodynamic.el --- Fake dynamic formatter

;;;_. Headers
;;;_ , License
;; Copyright (C) 2011  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp,tools

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

;; Fake dynamic printing.  If you use this you have to completely
;; reprint your root object when anything changes.

;;;_ , Requires

(require 'utility/dynvars)

;;;_. Body
;;;_ , emtv2:insert:dynamic
(defun emtv2:insert:dynamic (recurse-f obj func data)
   "Insert (statically) the result of a dynamic spec"
   (let*
      ((fmt-list 
	  (utidyv:with-vars data 
	     (funcall func obj))))
      (funcall recurse-f fmt-list)))

;;;_. Footers
;;;_ , Provides

(provide 'formatter/pseudodynamic)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; formatter/pseudodynamic.el ends here
