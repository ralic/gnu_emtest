;;;_ emtest/explorers/all.el --- Explorer-collection functionality

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
(defvar emtt:test-finder:method-list 
   '()
   "List of explorer methods.

Files that define explorers should call `emtt:add-explorer' to
add their methods.  Recommended: autoload a form like:
`(eval-after-load 'emtest/main/all-runners '(emtt:add-explorer SYM
FUNCTION NAME))'.

Format: Each entry is (GOV-SYMBOL FUNCTION NAME BASE-SCORE), where 
 * GOV-SYMBOL is a governor symbol
 * FUNCTION explores the test or suite.
 * NAME is the name of the method.
 * BASE-SCORE is the default explorability score for this type." )

;;;_  . emtt:add-explorer

(defun emtt:add-explorer (gov-symbol func &optional name base-score &rest dummy)
   "Add FUNC as explorer governed by GOV-SYMBOL"
   (utim:new-apair 
      gov-symbol 
      (list func (or name "<UNNAMED>") (or base-score 0))
      emtt:test-finder:method-list))
;;;_  . emtt:get-explore-info
(defun emtt:get-explore-info (gov-symbol)
   "Get the relevant info for GOV-SYMBOL.
GOV-SYMBOL should be a symbol."
   (utim:assq-value 
      gov-symbol
      emtt:test-finder:method-list
      (list #'emtt:explore-fallback "Fallback")))

;;;_  . emtt:get-explore-func 
(defun emtt:get-explore-func (how)
   "Get a relevant function for HOW.
Should not fail.
HOW must be a list."
   (car
      (emtt:get-explore-info (car how))))

;;;_ , emtt:get-explore-base-score
(defun emtt:get-explore-base-score (gov-sym)
   "Get the base score of a given test governor."
   
   ;;$$IMPROVE ME Let gov register a base-score and use that.
   (if (memq gov-sym '(form indexed-clause))
      0
      10))

;;;_  . Special explorers
;;;_   , emtt:explore-hello
;;This doesn't require an autoload but all others do.
(defun emtt:explore-hello (test-id props path report-f)
   "Report about Emtest, listing the explore methods."
   
   (funcall report-f
      (emt:testral:make-test-runner-info
	 :name "Emtest"
	 :version emtt:version
	 :explore-methods-supported
	 ;;$$RETHINK ME May make more sense to pass symbol or symbol
	 ;;name now.
	 (mapcar #'third emtt:test-finder:method-list))))

;;;_    . Register it

(emtt:add-explorer 'hello #'emtt:explore-hello
   "Tester signature") 

;;;_   , emtt:explore-fallback
;;Not part of the list of methods.
(defun emtt:explore-fallback (test-id props path report-f)
   "Report that no matching explore method could be found."

   (funcall report-f
      (emt:testral:make-suite
	 :contents nil
	 :grade 
	 'ungraded)))

;;;_. Footers
;;;_ , Provides
(provide 'emtest/explorers/all)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/all.el ends here
