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
;;;_  . emtvr:->grade-summary
(defun emtvr:->grade-summary (obj)
   "Change OBJ object into a grade summary.
OBJ may be a grade symbol or already be a summary."
   (etypecase obj 
      (emt:grade:summary obj)
      (emt:testral:grade-type
	 (let
	    ((obj-aux (emt:make-grade:summary)))
	    (emtvr:add-grades obj-aux obj)
	    obj-aux))))
;;;_  . emtvr:sym->severity
(defun emtvr:sym->severity (sym)
   "Return the severity of SYM, which should be a grade symbol."
   (emtvf:grade-fmt->severity
      (emtvf:get-grade-info sym)))
;;;_  . emtvr:add-one-grade
(defun emtvr:add-one-grade (sums sym count)
   "Add the grade represented by SYM to SUMS"
   
   (let*
      ((grade-list
	  (emt:grade:summary->grades sums))
	 (apair
	    (assq sym grade-list)))
      (if apair
	 ;;If it's already recorded, add to the count
	 (incf (second apair) count)
	 ;;Otherwise record it
	 (push
	    (list sym count)
	    (emt:grade:summary->grades sums)))
      (when
	 (>
	    (emtvr:sym->severity sym)
	    (emtvr:sym->severity (emt:grade:summary->worst sums)))
	 (setf 
	    (emt:grade:summary->worst sums)
	    sym))))


;;;_  . emtvr:add-grades
(defun emtvr:add-grades (sums a)
   "Add A to grade summary SUMS and return SUMS
SUMS must be a `emt:grade:summary'.
A may be a grade symbol, a grade summary, or nil."
   (check-type sums emt:grade:summary)
   (etypecase a
      (null sums)
      (symbol
	 (emtvr:add-one-grade sums a 1))
      (emt:grade:summary
	 (dolist (grade (emt:grade:summary->grades a))
	    (emtvr:add-one-grade sums 
	       (first grade) 
	       (second grade)))))
   sums)

;;;_  . emtvr:combine-grade
(defun emtvr:combine-grade (grades)
   "Combine the list GRADES into one entry"
   (let
      ((all
	  (reduce
	     #'emtvr:add-grades
	     grades
	     :initial-value (emt:make-grade:summary))))
      
      (check-type all emt:grade:summary)
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
