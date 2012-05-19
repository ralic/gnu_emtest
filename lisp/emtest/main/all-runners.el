;;;_ emtest/main/all-runners.el --- Collect runners

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



;;;_. Body

;;;_ , Collecting runners
;;;_  . List of runners
(defvar emt:runner:alist 
   '()
   "List of explorer methods.

Files that define runners should arrange to call
`emt:runner:add' to add their methods.  Recommended: autoload
a form like:
`(eval-after-load 'emtest/main/all-runners 
  '(emt:runner:add SYM FUNCTION NAME))'.

Format: Each entry is (GOV-SYMBOL FUNCTION), where 
 * GOV-SYMBOL is a governor symbol
 * FUNCTION runs a test-case.  FUNCTION must take 3 args:
   * props, an alist of inherited properties.
   * form, a form to be interpreted as a test-case.  Specifically, the
     cdr that follows GOV-SYMBOL.
   * report-f, a callback function to report the test-case results." )

;;;_  . emt:runner:add

(defun emt:runner:add (gov-symbol func &optional name)
   "Add FUNC as explorer governed by GOV-SYMBOL"
   (utim:new-apair 
      gov-symbol func emt:runner:alist))
;;;_  . emt:runner:get-info
(defun emt:runner:get-info (gov-symbol)
   "Get the relevant info for GOV-SYMBOL.
GOV-SYMBOL should be a symbol."
   (utim:assq-value 
      gov-symbol
      emt:runner:alist
      #'emt:runner:fallback))

;;;_  . emt:runner:get-func 
(defalias 'emt:runner:get-func 'emt:runner:get-info)

;;;_ , Special pseudo-runner
(defun emt:runner:fallback (props form-cdr report-f)
   "Report can't find a runner."
   
   (funcall report-f
      (emt:testral:make-suite
	 ;;$$IMPROVE ME Add a note here for contents

	 ;;$$SUPPORT ME In notes.el, a means for concisely
	 ;;making a notelist with just given notes.
	 :contents '()
	 :grade 'ungraded)))

;;Note would be like
'(emtt:testral:add-note
   "problem"
    'ungraded
   'error-raised
    'unrecognized-governor
    (emt:def:clause->governor clause))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/main/all-runners)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/main/all-runners.el ends here
