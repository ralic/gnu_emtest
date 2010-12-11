;;;_ emtest/common/grade-types.el --- Grade types

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

(eval-when-compile (require 'cl))

;;;_. Body
;;;_ , emt:testral:grade
;;;_  . Base type
(defstruct (emt:testral:grade
	      (:constructor emt:testral:make-grade)
	      (:copier nil)
	      (:conc-name emt:testral:grade->))
   "Base class for grades"
   contents)
;;;_  . Dormant
(defstruct (emt:testral:grade:dormant
	      (:include emt:testral:grade)
	      (:constructor emt:testral:make-grade:dormant)
	      (:copier nil)
	      (:conc-name emt:testral:grade:dormant->))
   "A dormant grade"
   reason)

;;;_  . Fail
(defstruct (emt:testral:grade:fail
	      (:include emt:testral:grade)
	      (:constructor emt:testral:make-grade:fail)
	      (:copier nil)
	      (:conc-name emt:testral:grade:fail->))
   "A failure grade")

;;;_  . Ungraded
(defstruct (emt:testral:grade:ungraded
	      (:include emt:testral:grade)
	      (:constructor emt:testral:make-grade:ungraded)
	      (:copier nil)
	      (:conc-name emt:testral:grade:ungraded->))
   "A lack of grade because of errors in the test")

;;;_  . Blowout
(defstruct (emt:testral:grade:blowout
	      (:include emt:testral:grade)
	      (:constructor emt:testral:make-grade:blowout)
	      (:copier nil)
	      (:conc-name emt:testral:grade:blowout->))
   "A lack of grade because of errors outside the test")
;;;_  . Test-case
(defstruct (emt:testral:grade:test-case
	      (:include emt:testral:grade)
	      (:constructor emt:testral:make-grade:test-case)
	      (:copier nil)
	      (:conc-name emt:testral:grade:test-case->))
   "A pseudo-grade indicating that a test-case completed")

;;;_  . Summary
(defstruct (emt:testral:grade:summary
	      (:include emt:testral:grade)
	      (:constructor emt:testral:make-grade:summary)
	      (:copier nil)
	      (:conc-name emt:testral:grade:summary->))
   "Class for summarized grades"
   (test-cases 0 :type integer)
   (fails      0 :type integer)
   (ungradeds  0 :type integer)
   (dormants   0 :type integer)
   (blowouts   0 :type integer))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/grade-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/grade-types.el ends here
