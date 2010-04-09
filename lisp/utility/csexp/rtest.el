;;;_ utility/csexp/rtest.el --- Tests of Csexp

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

(require 'utility/csexp)

;;;_. Body
;;;_ , Test data


   ;;This should be rewritten as eg examples
(defstruct csexp:th:pair
   ""
   encoded
   decoded)
   
;;Encoded/decoded pairs
(defconst csexp:thd:pairs
   (list
      (make-csexp:th:pair
	 :encoded "()"
	 :decoded ())
      (make-csexp:th:pair
	 :encoded "(2:ab)"
	 :decoded '(ab))
      (make-csexp:th:pair
	 :encoded "(2:ab2:cd)"
	 :decoded '(ab cd))
      ;;Other examples here
      )
   "" )



;;;_. Footers
;;;_ , Provides

(provide 'utility/csexp/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/csexp/rtest.el ends here
