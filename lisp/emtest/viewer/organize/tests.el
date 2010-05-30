;;;_ emtest/viewer/organize/tests.el --- Tests for organizing results pre-display

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

(require 'utility/pathtree)
(require 'emtest/viewer/receive)
(require 'emtest/viewer/emviewer2)
(require 'emtest/viewer/organize)
(require 'emtest/viewer/emviewer2/testhelp)

(require 'emtest/runner/define)
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/match)
(require 'emtest/runner/explorers/library/testhelp)
(require 'emtest/runner/launch)


;;;_. Body
;;;_ , Testhelp
;;;_  . emtvo:tester-cb
(defun emtvo:tester-cb (report)
   "A callback for tester.  It only goes as far as the pathtree, no further."
   (check-type report emt:testral:report)
   (emtv2:setup-if-needed)
   (emtvr:newstyle emtv2:receiver report)
   ;;For our purposes, there's nothing to freshen (yet.  Summarization
   ;;will change that)
   '(emtvp:freshen emtv2:pathtree))
;;;_  . 
;;May need to define with `emtm:define-struct-governor'
;;;_ , emtest/viewer/organize
(emt:deftest-3 emtest/viewer/organize
   (nil
      ;;$$SHARE ME - factor `emtv2:ts:with-mock-viewer' in
      ;;emviewer2/testhelp.el to share this insulation.
      (let
	 (
	    emtv2:receiver
	    emtv2:result-root)
	 (emt:library:th ((count 1))
	    (emt:doc "Situation: A known load-history and defined suites.")
	    (emt:doc "Situation: The receive callback only goes as far as
   the pathtree, no further.")
	    (emt:doc "Operation: Launch a library, which will launch its suites")
	    (emt:library
	       (emtg (type lib-path))
	       #'emtvo:tester-cb)
	    (emt:doc "Response: The results only occur where expected.")
	    (assert
	       (emtm emtv2:result-root
		  ;;$$WRITE ME
		  ;;Which tester ()
		  ;;Library, directly beneath it.
		  ;;1 suite, beneath that library.
		  ()
		  )
	       t)

	    ))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/organize/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/organize/tests.el ends here
