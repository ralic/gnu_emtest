;;;_ emtest/testhelp/mocks/filebuf/testhelp.el --- Testhelp for filebuf

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
(require 'emtest/testhelp/mocks/filebuf)


;;;_. Body
;;;_ , Test data

;;;_  . Test config
(defconst emtb:th-examples-dir
   (emtb:expand-filename-by-load-file "filebuf/examples/") 
   "" )


;;;_  . Master/slave files
(defconst emtb:thd:examples
   (emt:eg:define+
      ((project emtest)(library mockbuf))
      (item
	 ((role master)(type filename))
	 (expand-file-name "file3" emtb:th-examples-dir))
      ;;This item may go away as we automatically make tmp.
      (item
	 ((role slave)(type filename))
	 (expand-file-name "slave-dir/file3" emtb:th-examples-dir))))

;;;_  . emtb:check-file:thd:virtual-dir (OBSOLESCENT)
'
(defconst emtb:check-file:thd:virtual-dir
   (list
      (make-emtb:virtual-dir 
	 emtb:th-examples-dir
	 :dir "." 
	 :file-basenames
	 '("file1" "file2")))
   
   "File context for mockbuf check-file functionality" )

;;;_   , emtb:buf-contents-matches:thd:dir

(defconst emtb:buf-contents-matches:thd:dir
   (expand-file-name "matches-file/" emtb:th-examples-dir)
   "Directory of example files for emtb:buf-contents-matches." )
;;;_   , emtb:buf-contents-matches:thd:yes.txt
(defconst emtb:buf-contents-matches:thd:yes.txt 
   "line A
line B
"
   "The contents of the buffers used in the tests." )


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/filebuf/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/filebuf/testhelp.el ends here
