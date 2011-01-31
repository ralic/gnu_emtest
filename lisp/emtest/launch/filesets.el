;;;_ emtest/launch/filesets.el --- Launch tests via fileset

;;;_. Headers
;;;_ , License
;; Copyright (C) 2011  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: maint,convenience,lisp

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

(require 'fileset-whole)
(require 'emtest/launch/all) ;;Transitional, stuff will move.

;;;_. Body
;;;_ , emt:fileset
;;;###autoload
(defun emt:fileset (fileset-name)
   "Run the tests defined in FILESET."
   (interactive
      (list (fileset-whole-read-fileset)))
   (let*
      (
	 (fileset
	    (filesets-get-fileset-from-name fileset-name))
	 (test-files
	    (filesets-get-filelist fileset nil nil))
	 (test-files
	    ;;Remove those that aren't elisp source
	    (remove-if-not
	       #'(lambda (filename)
		    (string-match emacs-lisp-file-regexp filename))
	       test-files))
	 (suite-syms '()))

      (dolist (filename test-files)
	 (when (not (assoc filename load-history))
	    (load filename t nil t)))
      
      (do-symbols (sym)
	 (let*
	    ((props (get sym 'emt:properties))
	       (load-file-name (second (assoc 'load-file-name props))))
	    (when
	       (and load-file-name
		  (member load-file-name test-files))
	       
	       ;;Collect that suite by symbol
	       (push sym suite-syms))))

      ;;emtl:run-suite on each symbol.  Wanna group them by filename
      ;;but that's not so easy.
      (dolist (suite suite-syms)
	 (emtl:run-suite suite))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/launch/filesets)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/launch/filesets.el ends here
