;;;_ emtest/viewer/sumgrades.el --- Summarizing Emtest grades

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint,lisp

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

(require 'emtest/types/grade-types)

;;;_. Body
;;;_ , Functions
;;;_  . emtvr:grade->summary
(defun emtvr:grade->summary (obj)
   "Change OBJ object into a grade summary.
OBJ must be a emt:testral:grade-aux and may already be a summary."
   (check-type obj emt:testral:grade-aux)
   (typecase obj 
      (emt:testral:grade:summary obj)
      (t
	 (let
	    ((obj-aux (emt:testral:make-grade:summary)))
	    (emtvr:add-grades obj-aux obj)
	    obj-aux))))

;;;_  . emtvr:add-grades
(defun emtvr:add-grades (sums a)
   ""
   (typecase a
      (emt:testral:grade:summary
	 (incf 
	    (emt:testral:grade:summary->test-cases sums)
	    (emt:testral:grade:summary->test-cases a))
	 (incf 
	    (emt:testral:grade:summary->fails sums)
	    (emt:testral:grade:summary->fails a))	 
	 (incf 
	    (emt:testral:grade:summary->ungradeds sums)
	    (emt:testral:grade:summary->ungradeds a))
	 (incf 
	    (emt:testral:grade:summary->dormants sums)
	    (emt:testral:grade:summary->dormants a))
	 (incf 
	    (emt:testral:grade:summary->blowouts sums)
	    (emt:testral:grade:summary->blowouts a)))
      (emt:testral:grade:test-case
	 (incf 
	    (emt:testral:grade:summary->test-cases sums)))
      (emt:testral:grade:fail
	 (incf 
	    (emt:testral:grade:summary->fails      sums)))
      (emt:testral:grade:ungraded
	 (incf 
	    (emt:testral:grade:summary->ungradeds  sums)))
      (emt:testral:grade:dormant
	 (incf 
	    (emt:testral:grade:summary->dormants   sums)))
      (emt:testral:grade:blowout
	 (incf 
	    (emt:testral:grade:summary->blowouts   sums)))      
      (t nil)))

;;;_  . emtvr:combine-grade
(defun emtvr:combine-grade (bads)
   "Combine the list BADS into one entry"
   (let
      ((all
	  (reduce
	     #'(lambda (a b)
		  (check-type a emt:testral:grade-aux)
		  (check-type b emt:testral:grade-aux)
		  (cond
		     ((null a) b)
		     ((null b) a)
		     ((and
			 (emt:testral:grade-p a)
			 (emt:testral:grade-p b))
			(let
			   ((sums (emt:testral:make-grade:summary)))
			   (emtvr:add-grades sums a)
			   (emtvr:add-grades sums b)
			   sums))
		     (t
			(error "Shouldn't get here"))))
	     bads)))
      (check-type all emt:testral:grade-aux)
      all))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/sumgrades)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/sumgrades.el ends here
