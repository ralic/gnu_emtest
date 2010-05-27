;;;_ emtest/testhelp/tagnames/testhelp.el --- Testhelp for tagnames

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, internal

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
(require 'emtest/testhelp/tagnames)
;;;_. Body

;;;_  .  Test helper emtg:th:validate-helper-retval
;;$$Could just use deep-type-check
(defun emtg:th:validate-helper-retval (retval)
   "Validate a helpers' return value as the right type"
   (and
      (emtg:helper-rettype-p retval)
      (let
	 ((value-info (emtg:helper-rettype->value-info retval)))
	 (and
	    (listp value-info)
	    (every
	       #'emtg:valuedef-type-p
	       value-info)))))

;;;_  . Handle adding definitions

;;;_   , Test helper
;;$$CHANGING  The new design barely needs this.
(defmacro* emtg:define:th:with-empty-tagset
   ((&key examples) &rest body)
   ""
   
   `(let
       (  (emtg:all-examples        ,(or examples ()))
	  (emtg:*all-prpty-makers*     ()))
       ,@body))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tagnames/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tagnames/testhelp.el ends here
