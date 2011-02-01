;;;_ emtest/runners/quote.el --- Pseudo-runner for quoted-dormant test-cases

;;;_. Headers
;;;_ , License
;; Copyright (C) 2011  Tom Breton (Tehom)

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

(require 'emtest/types/testral-types)

;;;_. Body

;;;_  . nil runner emt:runner:quote
;;;###autoload
(defun emt:runner:quote (props form report-f)
   "Report a dormant result.  For quoted test-cases."

   (funcall report-f
      (emt:testral:make-suite
	 :contents '()
	 :grade 'dormant)))

;;;_ , Register it
;;;###autoload (eval-after-load 'emtest/main/all-runners
;;;###autoload '(emt:runner:add 'quote #'emt:runner:quote
;;;###autoload   "Dormant test-case pseudo-runner"))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runners/quote)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runners/quote.el ends here
