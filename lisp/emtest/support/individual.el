;;;_ emtest/support/individual.el --- Properties for specific clauses or suites

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint

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

;; For now, there's only one thing in this file and it's not
;; individuated yet, though it will be.

;;;_ , Requires

;;;_. Body
;;;_ , Types
;;It's an alist of (how-to-run . property-alist)

;;;_ , List of individual tests
(defvar emt:ind:alist 
   '()
   "Alist of properties of individual tests" )
;;;_ , emt:ind:get-prop Get properties of a test
;;;_ , emt:ind:set-prop Set properties of a test

;;;_. Footers
;;;_ , Provides

(provide 'emtest/support/individual)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/support/individual.el ends here
