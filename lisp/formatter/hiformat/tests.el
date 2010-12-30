;;;_ formatter/hiformat/tests.el --- Tests for hiformat

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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

(require 'formatter/hiformat)

;;;_. Body

;;;_ , Test data
;;;_  . Filename constants
(defconst hiformat:th:examples-dir
   (emt:expand-filename-here "examples") 
   "Directory where examples are" )
(defconst hiformat:th:db-id 
   `(persist ,(expand-file-name "db" hiformat:th:examples-dir))
   "" )
;;;_ , Test helper
(defun hiformat:th:assert-match (result-form dbid contents)
   ""
   (emt:assert
      (emt:eq-persist-p 
	 #'equal  
	 result-form
	 dbid))
   (emtb:with-buf ()
      (loformat:insert
	 result-form emtvf:format-alist)
      (emt:assert
	 (emtb:buf-contents-matches
	    :string contents))))

;;;_ , Tests
(emt:deftest-3 
   ((of 'hiformat:map)
      (db-id hiformat:th:db-id))
   (nil
      (let (result-form)
	 (emt:doc "Situation: Func and separator are trivial, just write 1 or 0.
PARAM: Separator is a formatted list.")

	 (hiformat:th:assert-match
	    (hiformat:map
	       #'(lambda
		    (&rest dummy)
		    '("1"))
	       '(a b c)
	       :separator
	       '("\n"))
	    "dbid:dc469045-cc87-4734-9298-5106c1c62311"
	    "1\n1\n1")))
   
   
   
   (nil
      (let (result-form)
	 (emt:doc "Situation: Func and separator are trivial, just write 1 or 0.
PARAM: Separator is a function.")
	 (hiformat:th:assert-match
	    (hiformat:map
	       #'(lambda
		    (&rest dummy)
		    '("1"))
	       '(a b c)
	       :separator
	       #'(lambda
		    (&rest dummy)
		    '("\n")))
	    "dbid:851b33c6-7469-4ce2-aa32-4f0c049ce8af"
	    "1\n1\n1")))
   
   
   (nil
      (let (result-form)
	 (emt:doc "Situation: Func and separator-f see data.")
	 (hiformat:th:assert-match
	    (hiformat:map
	       #'(lambda
		    (dummy-1 immediate-data-x)
		    (cond
		       ((assq 'first immediate-data-x)
			  '("FIRST"))
		       ((assq 'last immediate-data-x)
			  '("LAST"))
		       (t
			  '("OTHER"))))
	       '(a b c)
	       :separator
	       #'(lambda
		    (&rest dummy)
		    '("\n")))
	    "dbid:4fbdbe03-84e4-4bec-a0d0-069c08fad083"
	    "FIRST\nOTHER\nLAST")))
   
   (nil
      (let (result-form)
	 (emt:doc "PARAM: ELS=0 is given
PARAM: LIST is empty")
	 (emt:doc "Response: The ELS=0 form is printed.")

	 (emt:assert
	    (equal
	       (hiformat:map
		  #'(lambda
		       (&rest dummy)
		       '("1"))
		  'nil :separator
		  #'(lambda
		       (&rest dummy)
		       '("\n"))
		  :els=0
		  '("12" "144"))
	       '("12" "144")))
	 ))
   (nil
      (let (result-form)
	 (emt:doc "PARAM: ELS=1 is given
PARAM: LIST has 1 element")
	 (emt:doc "Response: The ELS=1 function is called instead of the usual function.")

	 (emt:assert
	    (equal
	       (hiformat:map
		  #'(lambda
		       (&rest dummy)
		       '("1"))
		  '(0)
		  :separator
		  #'(lambda
		       (&rest dummy)
		       '("\n"))
		  :els=1
		  #'(lambda
		       (dummy)
		       '("12" "144")))
	       '("12" "144")))
	 ))
   (nil
      (let (result-form)
	 (emt:doc "Situation: The given form has a governor.")
	 (setq result-form
	    (hiformat:map
	       #'(lambda
		    (&rest dummy)
		    '(example-gov "1"))
	       '(a b c)
	       :separator
	       '("\n")))
	 ;;$$PUNTED We'd want to actually run a trivial governor for
	 ;;this.
	 (emt:doc "Response: The governor still gets the right number
	    of arguments.")
	 (emt:assert
	    (emt:eq-persist-p #'equal 
	       result-form
	       "dbid:210a4dd3-d4a3-41f9-8cd0-a081d9a1c4d2"))))
   

   )


;;;_. Footers
;;;_ , Provides

(provide 'formatter/hiformat/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; formatter/hiformat/tests.el ends here
