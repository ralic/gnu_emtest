;;;_ tester/emt-funcall/testhelp.el --- Test help for emt-funcall

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

;;;_  . Test helper data

;;No actual value yet.  When we add reporting, change this so that
;;reporting is not a problem.
(defconst emt:report-control:thd:report-all 
   nil
   "" )

;;;_  . Test supporter

;;"should" etc won't work inside the body arg of `emt:funcall:th' but
;;will work outside it.

'  ;;Obsolete.  Use `emt:funcall:th:3'
(defmacro emt:funcall:th (initial-stored report-control &rest body)
   ""
   ;;Can't quite use `emt:trace:protect' for this.  It stores the same
   ;;stuff but it's about protecting with unwind.
   `(let
       ((emt:trace:stored-diag ,initial-stored)
	  (emt:trace:info-about ())
	  ;;Other control such as report-control is not supported yet,
	  ;;so can't control it here.
	  )
       
       (list
	  (progn ,@body)
	  emt:trace:stored-diag)))

' ;;Obsolete.  Use `emt:funcall:th:3'
(defmacro emt:funcall:th:2 (initial-stored report-control &rest body)
   ""
   ;;Can't quite use `emt:trace:protect' for this.  It stores the same
   ;;stuff but it's about protecting with unwind.
   `(let
       ((emt:trace:stored-diag ,initial-stored)
	  ;;These don't actually do anything useful.
	  (emt:trace:info-about ())
	  (emt:trace:tried ())
	  ;;Other control such as report-control is not supported yet,
	  ;;so can't control it here.
	  )
       
       (list
	  (progn ,@body)
	  :stored-diag emt:trace:stored-diag
	  :info-about emt:trace:info-about
	  :tried-list emt:trace:tried)))


(defun emt:funcall:th:3 (&rest args)
   "Call emt:funcall with the given arguments, but returns a
destructurable list (See code).

This mutes errors - to test for errors, just call it inside
`emt:trace:protect' (to stop stray diags)"
   (let*
      ((emt:trace:stored-diag ())
	  ;;Other control such as report-control is not supported yet,
	  ;;so can't control it here.
	 (retval
	    (ignore-errors
	       (apply #'emt:funcall args))))
      (assert
	 (= (length emt:trace:stored-diag) 1) t)
      (let
	 ((diag (car emt:trace:stored-diag)))

	 (list
	    retval 
	    diag
	    :status (emt:result:diag:call-status diag)
	    :info-about (emt:result:diag:call-info-about diag)
	    :tried-list (emt:result:diag:call-tried diag)))))




;;;_. Footers
;;;_ , Provides

(provide 'tester/emt-funcall/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/emt-funcall/testhelp.el ends here
