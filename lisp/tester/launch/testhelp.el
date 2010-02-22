;;;_ tester/launch/testhelp.el --- Testhelp for tester/launch.el

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
(require 'tester/launch)

;;;_. Body
;;;_  . emtt:th:run-suite
(defun emtt:th:run-suite (suite-sym func)
   ""
   
   (emt:test-finder:top 
      (make-emt:test-ID:e-n:suite
	 :suite-ID suite-sym) 
      '()
      "0" 
      func))

;;;_   , emtt:library:th

(defmacro emtt:library:th 
   (+tagset &rest body)
   "Run BODY in an environment with a certain example library defined.
+TAGSET is a tagset narrowing, as for `eg'."
   
   `(emt:eg:narrow ;;Narrow the examples.
       ((project emtest)
	  (library tester)
	  (section entry-points)
	  ,@+tagset)
	 
       (let
	  ((suite-sym-list (emt:eg (type suite-sym-list)))
	     ;;Temporarily bind load-history to known value.
	     (load-history 
		(emt:eg:value 
		   :narrow ((type load-history)) 
		   :ignore-tags (count))))

	  ;;Define the suites (inside a noprops)
	  (let-noprops suite-sym-list
	     (dolist (sym suite-sym-list)
		(eval ,'`(emt:deftest-2 ,sym ())))
		,@body)
	  t)))


;;;_. Footers
;;;_ , Provides

(provide 'tester/launch/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/launch/testhelp.el ends here
