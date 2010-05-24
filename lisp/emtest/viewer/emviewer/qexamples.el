;;;_ emtest/viewer/emviewer/qexamples.el --- Quoted examples for Emviewer

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

;;;_. Body

;;End-to-end viewing.  
'
(emtt:ts:run-test 
   '(nil
       ((emt:doc "Situation: testing an example") 
	  (error "An example error"))) 
   #'emtve:tester-cb)

'  ;;To show the result
(pp
   (let 
      ((l))
      (emtt:ts:run-test 
	 '(nil
	     ((emt:doc "Situation: testing an example") 
		(error "An example error"))) 
 	 #'(lambda (r)
 	      (push r l)))
      l))


'
(emtve:ts:run-test
   (nil
       ((emt:doc "Situation: testing an example") 
	  (error "An example error"))))


;;;_. Purely testing testing
'
(emt:deftest-3 example-test-0
   ;;Clause 0, empty.
   (()))


'
(emt:deftest-3 example-test-1
   ;;Clause 0, simple
   (()
      (error "An example error for `example-test-1'")
      ))


;;;_. Footers
;;;_ , Provides

;;Nothing.  This is not a library.

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/emviewer/qexamples.el ends here
