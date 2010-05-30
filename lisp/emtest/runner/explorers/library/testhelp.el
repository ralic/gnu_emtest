;;;_ emtest/runner/explorers/library/testhelp.el --- Testhelp for explorer library

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

(require 'emtest/runner/define)
(require 'emtest/testhelp/misc)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/mocks/filebuf)


;;;_. Body
;;;_ , Constants
;;;_   , emtt:launch:th:examples-dir
;;$$RENAME ME
(defconst emtt:launch:th:examples-dir
      (emtb:expand-filename-by-load-file "examples/find-libs/") 
      "Directory where find-libs examples are" )

;;;_   , emtt:launch:thd:examples
;;$$RENAME ME
(defconst emtt:launch:thd:examples
   (emtg:define+
      ((project emtest)(library tester)(section entry-points))
      (transparent-tags () (type))
      (group
	 ((count 2))
	 (item ((type name)) "example-2")
	 (item ((type lib-path))
	    (concat
		   emtt:launch:th:examples-dir
		   (emtg (type name))
		   ".el"))
	 (item ((type count)) 2)
	 (item ((type suite-sym-list)) '(foo bar))
	 ;;$RENAME ME Maybe rename it feature-sym
	 (item ((type sym)) (intern (emtg (type name))))
	 (item ((type file-load-history))
	    `( ,(emtg (type lib-path))
		,@(emtg (type suite-sym-list)) 
		(provide . ,(emtg (type sym))))))
      (group
	 ((count 1))
	 (item ((type name)) "example-1")
	 (item ((type count)) 1)
	 (item ((type lib-path))
	    (concat
	       emtt:launch:th:examples-dir
	       (emtg (type name))
	       ".el"))
	 (item ((type suite-sym-list)) '(foo))
	 (item ((type sym)) (intern (emtg (type name))))
	 (item ((type file-load-history))
	    `( ,(emtg (type lib-path))
		,@(emtg (type suite-sym-list)) 
		(provide . ,(emtg (type sym))))))


      (item ((type load-path-entry)(num 0))
	 emtt:launch:th:examples-dir)
      (item ((type load-path))
	 (emtg:map num nil
	    (emtg (type load-path-entry))))
      (item ((type load-history))
	 (emtg:map count nil
	    (emtg (type file-load-history))))))



;;;_ , Helper functions
;;;_  . emt:library:th:x
(defmacro emt:library:th:x (+tagset &rest body)
   ""
   
   `(emtg:with emtt:launch:thd:examples ,+tagset
       (let
	  ((suite-sym-list (emtg (type suite-sym-list)))
	     (load-path (emtg:value 
			   :narrow ((type load-path))
			   :ignore-tags (count num)))
	     (load-history 
		(emtg:value 
		   :narrow ((type load-history)) 
		   :ignore-tags (count))))

	  ;;Define the suites (protected by a noprops)
	  (emth:let-noprops suite-sym-list
	     (dolist (sym suite-sym-list)
		(eval ,'`(emt:deftest-3 ,sym ())))
	     ;;Now do the tests
	     ,@body)
	  t)))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/explorers/library/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/explorers/library/testhelp.el ends here
