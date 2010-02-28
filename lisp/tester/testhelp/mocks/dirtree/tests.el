;;;_ tester/testhelp/mocks/dirtree/tests.el --- Tests for mocks/dirtree

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
(require 'tester/testhelp/eg)
(require 'tester/testhelp/mocks/dirtree)
;;;_. Body
;;;_ , Master examples

(defconst emtmd:thd:master-examples 
   (expand-file-name "examples/" load-file-name)
   "The path to the master copy of the examples" )
;;;_ , Data


(emt:eg:define xmp:nfnesor01xe0
   ((project emtest)
    (library mocks))
   (group
      ((topdir "0"))
      (doc () "dir 0 is empty")
      (item ((type nametree))
	 '()))
   
   (group
      ((topdir "1"))
      (doc () "dir 1 has 1 subfile, `a'")
      (item ((type nametree))
	 '(("a"))))
   
   )


;;;_ , emtmd:get-nametree
(emt:deftest-3 emtmd:get-nametree
   ;;Should loop over all examples, topdir being the iteration
   ;;variable.
   (()
      (let
	 ((nametree
	     (emtmd:get-nametree
		(expand-file-name "0" emtmd:thd:master-examples))))
	 (emt:doc "Situation: Using master examples")
	 (emt:doc "Operation: Call emtmd:get-nametree on examples")
	 (emt:doc "Result: Matches what's expected.")
	 (assert
	    (equal nametree 
	       (emt:eg (project emtest)(library mocks)(topdir "0")
		  (type nametree)))
	    t)
	 
	 ))

   ;;More general version
   (()
      (emt:eg:narrow ((project emtest)(library mocks))
	 (emt:eg:map topdir prefix
	    (let
	       ((nametree
		   (emtmd:get-nametree
		      (expand-file-name prefix emtmd:thd:master-examples))))
	       ;;(format "Situation: Using example %S" prefix)
	       (emt:doc "Situation: Using master examples")
	       (emt:doc "Operation: Call emtmd:get-nametree on examples")
	       (emt:doc "Result: Matches what's expected.")
	       (assert
		  (equal nametree 
		     (emt:eg (type nametree)))
		  t)
	 
	       ))))
   )


;;;_. Footers
;;;_ , Provides

(provide 'tester/testhelp/mocks/dirtree/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/testhelp/mocks/dirtree/tests.el ends here
