;;;_ utility/loal.el --- Lists of Alists functionality

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

;; 


;;;_ , Requires
(eval-when-compile
   (require 'cl))
;;;_. Body
;;;_ , Types
;;;_  . loal:single-alist
(deftype loal:single-alist ()
   (require 'emtest/testhelp/deep-type-checker)
   '(repeat (list symbol t)))
;;;_  . loal
(deftype loal ()
   '(repeat loal:single-alist))

;;;_ , Functions
;;;_  . loal:assq Get data
(defun loal:assq (key data-lol)
   ""
   (check-type data-lol loal)
   (catch 'loal:assq
      (dolist (data data-lol)
	 (let*
	    ((found (assq key data)))
	    (if found (throw 'loal:assq found))))))

;;;_  . loal:val
(defun loal:val (key data-list &optional default)
   ""
   
   (let
      ((cell (loal:assq key data-list)))
      (if cell
	 (second cell)
	 default)))

;;;_  . loal:acons
(defun loal:acons (key value data-lol)
   ""
   
   (cons (list (list key value)) data-lol))
;;;_  . loal:update
;;$$TEST ME
(defun loal:update (key func data-lol &optional default)
   "Returns a LOAL that contains the updated value.
The updated value hides the original value."
   
   (let*
      ((old-val
	  (loal:val key data-lol default)))
      
      (loal:acons key (funcall func old-val) data-lol)))


;;;_. Footers
;;;_ , Provides

(provide 'utility/loal)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/loal.el ends here
