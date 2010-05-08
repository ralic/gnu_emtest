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

(require 'emtest/runner/launch)  ;;launch is not actually used in here.
(require 'emtest/runner/tester)
(require 'emtest/testhelp/misc)
(require 'emtest/testhelp/eg)
(require 'emtest/common/result-types)

;;;_. Body
;;;_  . Constants
;;;_   , emtt:launch:th:examples-dir
(defconst emtt:launch:th:examples-dir
      (emt:expand-filename-by-load-file "examples/find-libs/") 
      "Directory where find-libs examples are" )

;;;_   , emtt:launch:thd:examples
(defconst emtt:launch:thd:examples
   (emt:eg:define+
      ((project emtest)(library tester)(section entry-points))
      (transparent-tags () (type))
      (group
	 ((count 2))
	 (item ((type name)) "example-2")
	 (item ((type count)) 2)
	 (item ((type suite-sym-list)) '(foo bar))
	 (item ((type sym)) (intern-soft (emt:eg (type name))))
	 (item ((type file-load-history))
	    `( ,(concat
		   emtt:launch:th:examples-dir
		   (emt:eg (type name))
		   ".el")
		,@(emt:eg (type suite-sym-list)) 
		(provide . ,(emt:eg (type sym))))))
   
      (group
	 ((count 1))
	 (item ((type name)) "example-1")
	 (item ((type count)) 1)
	 (item ((type suite-sym-list)) '(foo))
	 (item ((type sym)) (intern-soft (emt:eg (type name))))
	 (item ((type file-load-history))
	    `( ,(concat
		   emtt:launch:th:examples-dir
		   (emt:eg (type name))
		   ".el")
		,@(emt:eg (type suite-sym-list)) 
		(provide . ,(emt:eg (type sym))))))


      (item ((type load-path-entry)(num 0))
	 emtt:launch:th:examples-dir)
      (item ((type load-path))
	 (emt:eg:map num nil
	    (emt:eg (type load-path-entry))))
      (item ((type load-history))
	 (emt:eg:map count nil
	    (emt:eg (type file-load-history))))))

;;;_  . Functions
;;;_   , emtt:library:th

(defmacro emtt:library:th 
   (+tagset &rest body)
   "Run BODY in an environment with a certain example library defined.
+TAGSET is a tagset narrowing, as for `eg'."
   
   `(emt:eg:with emtt:launch:thd:examples ,+tagset
       (let
	  ((suite-sym-list (emt:eg (type suite-sym-list)))
	     (load-path (emt:eg:value 
			   :narrow ((type load-path))
			   :ignore-tags (count num)))
	     (load-history 
		(emt:eg:value 
		   :narrow ((type load-history)) 
		   :ignore-tags (count))))

	  ;;Define the suites (protected by a noprops)
	  (let-noprops suite-sym-list
	     (dolist (sym suite-sym-list)
		(eval ,'`(emt:deftest-3 ,sym ())))
	     ;;Now do the tests
	     ,@body)
	  t)))

;;;_  . emtt:th:run-suite
(defun emtt:th:run-suite (suite-sym func)
   ""
   
   (emt:test-finder:top
      ;;$$UPDATE ME - will need to change what it makes
      (make-emt:test-ID:e-n:suite
	 :suite-ID suite-sym) 
      '()
      "0" 
      func))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/launch/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/launch/testhelp.el ends here
