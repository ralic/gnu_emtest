;;;_ emtest/main/all-explorers.el --- Explorer-collection functionality

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint, lisp

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

(require 'emtest/main/config)
(require 'emtest/types/run-types)
(require 'emtest/types/testral-types)

;;;_. Body
;;;_ , Collecting explorers
;;;_  . List of explorers
(defvar emt:exps:alist 
   '()
   "List of explorers and their data.

Files that define explorers should call `emt:exps:add' to
add their methods.  Recommended: autoload a form like:
`(eval-after-load 'emtest/main/all-runners '(emt:exps:add SYM
FUNCTION NAME))'.

Format: Each entry is (GOV-SYMBOL (FUNCTION NAME SHOW-AVAIL)), where 
 * GOV-SYMBOL is a governor symbol
 * FUNCTION explores the test or suite.
 * NAME is the name of the method.
 * SHOW-AVAIL is non-nil if may be called with a test-id that
   provides no further arguments to make it list what's available" )

;;;_  . emt:exps:add

(defun emt:exps:add (gov-symbol func &optional name list-all &rest dummy)
   "Add FUNC as explorer governed by GOV-SYMBOL.

If LIST-ALL is non-nil, func may be called with a test-id that
provides no further arguments and should list what's available."
   (utim:new-apair 
      gov-symbol 
      (list func (or name "<UNNAMED>") list-all)
      emt:exps:alist))
;;;_  . emt:exps:get-info
(defun emt:exps:get-info (gov-symbol)
   "Get the relevant info for GOV-SYMBOL.
GOV-SYMBOL should be a symbol."
   (utim:assq-value 
      gov-symbol
      emt:exps:alist
      (list #'emt:exp:fallback "Fallback")))

;;;_  . emt:exps:get-func 
(defun emt:exps:get-func (how)
   "Get a relevant function for HOW.
Should not fail.
HOW must be a list."
   (car
      (emt:exps:get-info (car how))))

;;;_ , Special explorers
;;;_  . emt:exp:available
;;This doesn't require an autoload but all other explorers do.
(defun emt:exp:available (test-id props path report-f)
   "Report about Emtest, listing the explore methods."

   (funcall report-f
      (emt:testral:make-suite
	 :contents 
	 (emt:testral:make-runform-list
	    :els
	    ;; $$IMPROVE ME: Skip if show-avail is nil.
	    (mapcar 
	       #'(lambda (x)
		    (emtt:make-explorable
		       :how-to-run (list (car x))
		       ;; Could use name here.
		       :prestn-path (list (car x))))
	       emt:exps:alist))
	 
	 :grade nil)))


;;;_   , Register it
;; We don't show whats-available itself in whats-available, so
;; SHOW-AVAIL is `nil'
(emt:exps:add 'whats-available #'emt:exp:available "What's available" nil)

;;;_   , emtest
;;;###autoload
(defun emtest ()
   "Run the top level of Emtest, which will show what's available."
   
   (interactive)
   (emt:lch:run '(whats-available) '() '(whats-available)))


;;;_  . emt:exp:fallback
;;Not part of the list of methods.
(defun emt:exp:fallback (test-id props path report-f)
   "Report that no matching explore method could be found."

   (funcall report-f
      (emt:testral:make-suite
	 :contents nil
	 :grade 
	 'ungraded)))

;;;_. Footers
;;;_ , Provides
(provide 'emtest/main/all-explorers)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/main/all-explorers.el ends here
