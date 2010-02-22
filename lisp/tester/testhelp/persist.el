;;;_ tester/testhelp/persist.el --- The testhelp portion of Emtest persist functionality

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
;;Requires the shared persist functionality

;;;_. Body

;;;_ , Persist functions 
;;These must be visible to tests.  They may be moved, but not into
;;emt-persist.el which is implementation.
;;This design is somewhat obsolete now.

;;;_  . emt:persist
;;
;;;###autoload
(defun emt:persist (id &optional backend)
   "Return a persisting object or a placeholder"
   (let
      ((backend
	  (or
	     backend
	     (let
		((cell (assoc 'db-id emt:trace:properties)))
		(when cell
		   (second cell)))
	     ;;Here add any other ways of learning the backend
	     (error "No backend was provided"))))
      (make-emt:db:id-index.
	 :id id
	 :backend backend)))


;;;_. Footers
;;;_ , Provides

(provide 'tester/testhelp/persist)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/testhelp/persist.el ends here
