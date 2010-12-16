;;;_ emtest/viewer/receive/testhelp.el --- Testhelp for receive

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

(require 'emtest/viewer/receive)
(require 'emtest/viewer/view-types)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/match)

;;;_. Body

;;;_ , Testhelp

(defun emtvr:th:assert-the-1-right-node (nodes-freshened)
   ""
   
   (emt:assert
      (emtm nodes-freshened
	 (list
	    (list
	       (eval
		  '(emtg:value 
		      :narrow 
		      ((type presentation-path))
		      :ignore-tags (role)))
	       (emt:view:make-suite-newstyle
		  :presentation-path
		  (eval
		     '(emtg:value 
			 :narrow 
			 ((type presentation-path))
			 :ignore-tags (role)))
		  :result
		  (eval
		     '(emtg (type suite)))))))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/receive/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/receive/testhelp.el ends here
