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
;;;_  . emtvr:summary->summary
;;$$TRANSITIONAL
(defun emtvr:summary->summary (obj)
   "Return a new-style grade summary, converted from OBJ"
   (typecase obj
      (emt:grade:summary obj)
      (emt:testral:grade:summary
	 (let*
	    (
	       (obj (emtvr:grade->summary obj))
	       (test-cases (emt:testral:grade:summary->test-cases obj))
	       (fails      (emt:testral:grade:summary->fails      obj))
	       (ungradeds  (emt:testral:grade:summary->ungradeds  obj))
	       (dormants   (emt:testral:grade:summary->dormants   obj))
	       (blowouts   (emt:testral:grade:summary->blowouts   obj)))
	    (emt:make-grade:summary
	       :grades
	       (delq nil
		  (list
		     (if (> blowouts   0) `(blowout   ,blowouts  ) nil)
		     (if (> ungradeds  0) `(ungraded  ,ungradeds ) nil)
		     (if (> fails      0) `(failed    ,fails     ) nil)
		     (if (> dormants   0) `(dormant   ,dormants  ) nil)
		     (if (> test-cases 0) `(test-case ,test-cases) nil)))
	 
	       :worst
	       (cond
		  ((> blowouts   0) 'blowout)
		  ((> ungradeds  0) 'ungraded)
		  ((> fails      0) 'failed)
		  ((> dormants   0) 'dormant)
		  ((> test-cases 0) 'test-case)
		  (t                nil)))))))
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

(defun emtvr:grade->summary-NEW (obj)
   "Change OBJ object into a grade summary.
OBJ must be a emt:testral:grade-aux and may already be a summary."
   (check-type obj emt:testral:grade-aux)
   (etypecase obj 
      (emt:grade:summary obj)
      (symbol
	 (let
	    ((obj-aux (emt:make-grade:summary)))
	    (emtvr:add-grades obj-aux obj)
	    obj-aux))))
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
	    (emtvf:grade-fmt->priority
	       (emtvf:get-grade-info sym))
	    (emtvf:grade-fmt->priority
	       (emtvf:get-grade-info (emt:grade:summary->worst sums))))
	 (setf 
	    (emt:grade:summary->worst sums)
	    sym))))


;;;_  . emtvr:add-grades
(defun emtvr:add-grades (sums a)
   "Add A to grade summary SUMS.
A must be a grade or a grade summary."
   (typecase sums
      (emt:grade:summary
	 (etypecase a
	    (symbol
	       (emtvr:add-one-grade sums a 1))
	    (emt:grade:summary
	       (dolist (grade (emt:grade:summary->grades a))
		  (emtvr:add-one-grade sums 
		     (first grade) 
		     (second grade))))))
      ;;$$OBSOLESCENT
      (emt:testral:grade:summary
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
	    (symbol
	       (case a
		  (failed
		     (incf 
			(emt:testral:grade:summary->fails      sums)))
		  (ungraded
		     (incf 
			(emt:testral:grade:summary->ungradeds  sums)))
		  (dormant
		     (incf 
			(emt:testral:grade:summary->dormants   sums)))
		  (blowout
		     (incf 
			(emt:testral:grade:summary->blowouts   sums)))
		  (test-case
		     (incf 
			(emt:testral:grade:summary->test-cases sums)))
		  (t
		     ;;$$PUNT  Handle `suite', `assertion', etc.  But we'll
		     ;;move to another method before that becomes reasonable.
		     )
		  ))
      
	    (t nil)))))

;;;_  . emtvr:combine-grade
(defun emtvr:combine-grade (grades)
   "Combine the list GRADES into one entry"
   (let
      ((all
	  (reduce
	     #'(lambda (a b)
		  (check-type a emt:testral:grade-aux)
		  (check-type b emt:testral:grade-aux)
		  (cond
		     ((null a) b)
		     ((null b) a)
		     ;;$$IMPROVE ME  If one is already a summary, just
		     ;;use it.  Have to pass one as initial-value.
		     ((and
			 (or
			    (emt:testral:grade-p a)
			    (symbolp a))
			 (or
			    (emt:testral:grade-p b)
			    (symbolp b)))
			(let
			   ((sums (emt:make-grade:summary)))
			   (emtvr:add-grades sums a)
			   (emtvr:add-grades sums b)
			   sums))
		     (t
			(error "Shouldn't get here"))))
	     grades)))
      
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
