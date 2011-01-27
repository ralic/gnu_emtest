;;;_ emtest/editing/expect.el --- Help to capture a script for external.el

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint,lisp,convenience

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

(require 'pp)

;;;_. Body

;;;_ , emtr:expect:buffer->form
(defun emtr:expect:buffer->form (prompt)
   "Return a script form generated from the current buffer.
Buffer should contain a transcript of a session with the program that
is to be scripted.  

This fills the same need as autoexpect."
   (let 
      ((rv-pieces '()))

      (while (re-search-forward 
		(concat
		   prompt
		   "\\([^\n]*\n\\)"
		   "\\([^\n]*\n\\)"
		   prompt)
		nil t)
	 (push
	    (list 
	       (match-string-no-properties 1) 
	       (match-string-no-properties 2))
	    rv-pieces)
	 (goto-char (match-end 2)))

      (mapcar
	 #'(lambda (piece)
	      `(t ,(first piece)
		  (emt:doc "WRITE ME")
		  (emt:assert 
		     (equal answer 
			,(concat (second piece) prompt))
		     t)))
	 (nreverse rv-pieces))))

;;;_ , emtr:expect:buffer-capture-form
;;;###autoload
(defun emtr:expect:buffer-capture-form (prompt)
   "Push entries for an `emtr:expect' script onto the kill ring.
Current buffer should contain a transcript of a session."
   
   (interactive "sPrompt: ")
   (kill-new 
      (pp-to-string
	 (emtr:expect:buffer->form prompt))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/expect)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/expect.el ends here
