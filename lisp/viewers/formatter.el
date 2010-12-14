;;;_ viewers/formatter.el --- Formatter from structured data to text

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: 

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

(require 'viewers/loformat)

;;;_. Body

;;;_ , formatter:top
;;The entry point for static formatting. 
(defun formatter:top (buf-name obj &optional alist)
   "Format OBJ into buffer BUF-NAME.
Optional arg ALIST is a list of inserters, as for `loformat:insert'.
Returns the buffer."
   
   (let
      ((buf
	  (get-buffer-create buf-name)))
      (with-current-buffer buf
	 (erase-buffer)
	 (loformat:insert obj alist))
      (pop-to-buffer buf)))

;;;_. Footers
;;;_ , Provides

(provide 'viewers/formatter)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/formatter.el ends here
