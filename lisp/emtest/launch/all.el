;;;_ emtest/launch/all.el --- Launchers for Emtest

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

(require 'emtest/main/find-tests)
(require 'emtest/viewer/emviewer2)
(require 'emtest/explorers/suite)
(require 'emtest/explorers/library)
(require 'emtest/explorers/clause)
(require 'emtest/types/run-types)
(require 'emtest/main/config)  ;;Just for receiver configuration
(require 'emtest/editing/lisp)
(require 'emtest/support/individual)

;;;_. Body

;;;_ , Borrowed variables
;;;_  . Counter

;;Should probably live elsewhere.  Possibly in emtest/viewer/receive.
;;But then emtl:dispatch-normal must move there as well.
(defvar emtl:testrun-counter 0 
   "A counter used to make testrun-id.
With `cl' loaded, use it as (incf emtl:testrun-counter)." )

;;;_ , emtl:dispatch-normal
;;$$MOVE ME into test-finder
(defun emtl:dispatch-normal (what-to-run &optional prefix receiver)
   ""
   (emtt:test-finder:top 
      what-to-run 
      prefix  ;;Default is the empty list.
      (prin1-to-string (incf emtl:testrun-counter))
      (or receiver emtl:receiver-f)))


;;;_ , emt:debug-on-entry
;;$$IMPROVE ME  Make the interface much more specific.
;;;###autoload
(defun emt:debug-on-entry (&rest r)
   "Debug all test clauses on entry."
   
   (interactive)
   (setq emti:debug-p t))

;;;_ , emt:cancel-debug-on-entry
;;$$IMPROVE ME  Make the interface volunteer only currently debugged
;;runnables.
;;;###autoload
(defun emt:cancel-debug-on-entry (&rest r)
   ""
   
   (interactive)
   (setq emti:debug-p nil))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/launch/all)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/launch/all.el ends here
