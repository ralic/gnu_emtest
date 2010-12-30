;;;_ emtest/types/prestn-path.el --- Presentation-paths for TESTRAL

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp,maint,internal

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

(eval-when-compile
   (require 'cl))


;;;_. Body
;;;_ , Types
;;;_  . emt:testral:id-element
(deftype emt:testral:id-element () 
   "Id elements are strings."
   '(or string symbol integer))

;;;_  . emt:testral:prestn-path
(deftype emt:testral:prestn-path () '(repeat emt:testral:id-element))

;;;_  . emt:testral:testrun-id
(defalias 'emt:testral:testrun-id-p 'stringp)
;;;_ , Comparison function
;;;_  . emt:testral:id=
(defun emt:testral:id= (a b)
   ""
   (equal a b))
;;;_  . Conversion functions
;;;_   , emt:testral:id->emtvp:name
(defalias 'emt:testral:id->emtvp:name 'identity)
;;;_   , emt:testral:map-id->emtvp:name
(defalias 'emt:testral:map-id->emtvp:name 'identity)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/types/prestn-path)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/types/prestn-path.el ends here
