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

(require 'emtest/viewer/organize)
(require 'emtest/main/define)

(require 'emtest/viewer/view-types/testhelp)
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/match)
(require 'emtest/explorers/library/testhelp)
(require 'emtest/launch/all/testhelp)
(require 'utility/pathtree)
(require 'utility/pathtree/testhelp)

;;;_. Body
;;;_ , Testhelp
;;;_  . emtvo:tester-cb
(defun emtvo:tester-cb (report)
   "A callback for tester.  It only goes as far as the pathtree, no further."
   (check-type report emt:testral:report)
   (emtvo:setup-if-needed #'ignore #'ignore)
   (emtvo:receive report))

;;;_  . Struct governors for pattern-match
;;Are in pathtree testhelp

;;;_ , emtest/viewer/organize
(emt:deftest-3 
   ((of 'emtest/viewer/organize)
      (:surrounders
	 ;;$$SHARE ME - factor `emtv2:ts:with-mock-viewer' in
	 ;;emviewer2/testhelp.el to share this insulation.
	 '((let
	      (emtvo:receiver emtvo:pathtree)))))
   (nil
      (progn
	 (emt:doc "Operation: Send just a hello.")
	 (emtl:th:hello
	    #'emtvo:tester-cb)
	 (emt:doc "Validate:  We have set up.")
	 (emt:doc "Validate:  We have received some result objects.")
	 (emt:assert (identity emtvo:receiver))
	 (emt:assert (identity emtvo:pathtree))
	 (emt:doc "Validate: Tree is the right type all thru it")
	 (emt:assert
	    (emtvp:th:type-correct-p emtvo:pathtree))
	 (emt:doc "Response: The results occur exactly in expected
   positions in the tree.")
	 (emt:assert
	    (emtm 
	       emtvo:pathtree
	       (emtvp:make 
		  :root
		  ;;Pathtree's root
		  (emt:view:make-presentable
		     :children
		     (list
			;;Tester info
			(emt:view:make-suite
			   :children '()))))))
	 (emt:doc "Response: WRITEME.")))


   (nil
      (emt:library:th ((count 1))
	 (emt:doc "Situation: A known load-history and defined suites.")
	 (emt:doc "Situation: The receive callback only goes as far as
   the pathtree, no further.")
	 (emt:doc "Operation: Launch a library, which will launch its suites")
	 (emt:library
	    (emtg (type lib-path))
	    #'emtvo:tester-cb)

	 (emt:doc "Validate:  We have set up.")
	 (emt:doc "Validate:  We have received some result objects.")
	 (emt:assert (identity emtvo:receiver))
	 (emt:assert (identity emtvo:pathtree))
	 (emt:doc "Validate: Tree is the right type all thru it")
	 (emt:assert
	    (emtvp:th:type-correct-p emtvo:pathtree))
	 (emt:doc "Response: The results occur exactly in expected
   positions in the tree.")
	 (emt:assert
	    (emtm 
	       emtvo:pathtree
	       (emtvp:make 
		  :root
		  ;;Pathtree's root
		  (emt:view:make-presentable
		     :children
		     (list
			;;Tester info
			(emt:view:make-presentable
			   :children
			   (list
			      ;;Library report
			      (emt:view:make-suite
				 :children
				 (list
				    ;;Suite report
				    (emt:view:make-suite
				       :children
				       ()))))))))))))
   
   (nil
      (emt:library:th ((count 0))
	 (emt:doc "Situation: A known load-history and defined suites.")
	 (emt:doc "Situation: The receive callback only goes as far as
   the pathtree, no further.")
	 (emt:doc "Operation: Launch a library, which will launch its suites")
	 (emt:library
	    (emtg (type lib-path))
	    #'emtvo:tester-cb)

	 (emt:doc "Validate:  We have set up.")
	 (emt:doc "Validate:  We have received some result objects.")
	 (emt:assert (identity emtvo:receiver))
	 (emt:assert (identity emtvo:pathtree))
	 (emt:doc "Validate: Tree is the right type all thru it")
	 (emt:assert
	    (emtvp:th:type-correct-p emtvo:pathtree))
	 (emt:doc "Response: The results occur exactly in expected
   positions in the tree.")
	 (emt:assert
	    (emtm 
	       emtvo:pathtree
	       (emtvp:make 
		  :root
		  ;;Pathtree's root
		  (emt:view:make-presentable
		     :children
		     (list
			;;Tester info
			(emt:view:make-presentable
			   :children
			   (list
			      ;;Library report
			      (emt:view:make-suite
				 :children
				 (list)))))))))))
   
   )


;;;_. Footers
;;;_ , Provides

(provide 'emtest/viewer/organize/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/viewer/organize/tests.el ends here
