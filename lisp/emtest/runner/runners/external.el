;;;_ emtest/runner/runners/external.el --- Expect-like testing external programs

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint,processes

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
;;;_ , Entry points (for clause explorer)
;;;_  . emtr:external
(defun emtr:external (props form report-f)
   "Run a test-case on external program and report the result."

   ;;$$PUNT for now
   (let*
      ()
      
      ))
;;;_  . Scratch area
;;Could also use start-process-shell-command but wildcards etc seem
;;unneeded. 
;;Make a fresh buffer
'
(setq my-buf
   (generate-new-buffer "external"))

'
(setq my-prog+args
   '("/bin/sh" "-i"))


'
(setq my-proc
   (apply #'start-process "external" my-buf my-prog+args))

'
(setq my-tq
   (tq-create my-proc))

;;Some aren't tests, just setup.
(defvar my-56-a)
'
(tq-enqueue my-tq 
    "PS1='% '\n"
    "% "
    56
    #'(lambda (data answer)
	 (setq my-56-a (list data answer)))
   
    t)

'
(tq-enqueue my-tq 
   "echo hello\n"
   "% "
   57
   #'(lambda (data answer)
	(setq my-56-a (list data answer)))
   
          t)

;;;_. Footers
;;;_ , Provides

(provide 'emtest/runner/runners/external)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/runner/runners/external.el ends here
