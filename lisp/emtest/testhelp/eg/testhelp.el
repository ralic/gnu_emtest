;;;_ emtest/testhelp/eg/testhelp.el --- Testhelp for eg

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

;;;_. Body

;;;_  .  Test helper emt:eg:th:validate-helper-retval
;;$$Could just use deep-type-check
(defun emt:eg:th:validate-helper-retval (retval)
   "Validate a helpers' return value as the right type"
   (and
      (emt:eg:helper-rettype.-p retval)
      (let
	 ((value-info (emt:eg:helper-rettype.-value-info retval)))
	 (and
	    (listp value-info)
	    (every
	       #'emt:eg:valuedef-type.-p
	       value-info)))))

;;;_  . Handle adding definitions

;;;_   , Test helper
(defmacro* emt:eg:define:th:with-empty-tagset
   ((&key examples) &rest body)
   ""
   
   `(let
       (  (emt:eg:all-examples        ,(or examples ()))
	  (emt:eg:all-prpty-makers     ()))
       ,@body))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/eg/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/eg/testhelp.el ends here
