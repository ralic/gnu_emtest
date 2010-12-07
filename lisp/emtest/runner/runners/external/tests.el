;;;_ emtest/runner/runners/external/tests.el --- Tests for emtest/runner/runners/external

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

(require 'emtest/runner/runners/external)

;;;_. Body
;;;_ , Insulation
(defconst emtest/runner/runners/external:th:surrounders 
   '()
   "The normal surrounders for emtest/runner/runners/external tests" )
;;;_ , Script
;;Just a script that should work for expect.  Borrowed from a Greg
;;example.
'(
    ;;This is *not* freeform, `child' is a governor and we might allow
    ;;others.
    (child '("/bin/sh" "-i"))  ;;how to launch the child process.
)

;
; Run an interactive shell as a child process
;
'(greg-child "/bin/sh" "-i")

;
; Set the shell prompt
;
'(greg-send "PS1='% '\n")   

;
; Now test that the shell echoes what we expect.
; If we have a timeout or an eof, we will get a failure result.
;
'
(greg-testcase "echo 'hello'" #t
  (lambda ()
    (greg-send "echo hello\n")  ; Get it to send us something
    (expect-strings
      ("hello\r\n% " #t)
    )
  )
)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/runners/external/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/runners/external/tests.el ends here
