;;;_ do-elinstall.el --- Script to do elinstall installation

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint,convenience

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

(require 'elinstall)

;;;_. Body

(elinstall
   "emtest"
   (elinstall-directory-true-name)
   '(load-path 
       (all t 
	  ;;Explorer plugins use this to inform top about their
	  ;;existence via this file.
	  (def-file "emtest/runner/loadexplorers.el" nil
	     (dir "emtest/runner/explorers"))))
   t)

;;$$IMPROVE ME And then handle the various plugins that should 
;;, by defining a different deffile for them.

;;;_. Footers
;;;_ , Provides

;;Nothing, this is a script.

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + no-byte-compile: t
;;;_  + no-update-autoloads: t
;;;_  + End:

;;;_ , End
;;; elinstall/do-elinstall.el ends here
