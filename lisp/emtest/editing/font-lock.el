;;;_ emtest/editing/font-lock.el --- Font-lock support for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint,lisp,internal

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
;;Nothing

;;;_. Body
;;;_ , emt:ed:font-lock:make-keywordgroup
(defun emt:ed:font-lock:make-keywordgroup (object)
   ""
   `(,(regexp-opt (car object) 'words) 
       1
       ,(second object)
       prepend))
;;;_ , emt:ed:font-lock:data
(defconst emt:ed:font-lock:data 
   '(((
	 "emtp:eval"
	 "emtp"
	 "emt:deftest-3"
	 "emtb:with-buf"
	 "emtb:with-file-f"
	 "emtmv:require-x"
	 "emtmv:with-version"
	 "emt:eq-persist-p"
	 )
	font-lock-keyword-face)
       (("emt:doc")
	  font-lock-doc-face)
       (("emt:assert")
	  font-lock-warning-face))
   
   "Keywords for Emtest, by group" )

;;;_ , emt:ed:font-lock:add-keywords
;;;###autoload
(defun emt:ed:font-lock:add-keywords ()
   ""
   (when (featurep 'font-lock)
      (font-lock-add-keywords 'emacs-lisp-mode
	 (mapcar
	    #'emt:ed:font-lock:make-keywordgroup
	    emt:ed:font-lock:data))))

;;;_ , Insinuate
;;;###autoload (add-hook 'emacs-lisp-mode-hook 'emt:ed:font-lock:add-keywords)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/font-lock)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/font-lock.el ends here
