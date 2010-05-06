;;;_ emtest/testhelp/mocks/dirtree.el --- Directory tree mock

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

(require 'emtest/testhelp/misc)
(require 'emtest/testhelp/mocks/filebuf)

;;;_. Body
;;;_ , Types
;;;_  . emtmd:repr-file

(defstruct (emtmd:repr-file
	      (:constructor 
		 emtmd:make-repr-file
		 (name &optional contents-spec
		    &aux 
		    (contents
		       (if contents-spec
			  (emtb:string-containing-object-f contents-spec)
			  nil))))
	      (:conc-name emtmd:repr-file->)
	      (:copier nil))
   
   "Dirtree representation of a file"
   name
   contents)
;;;_  . emtmd:repr-dir
(deftype emtmd:repr-dir ()
   "Dirtree representation of a directory"
   '(cons string emtmd:repr-contents))
;;;_   , emtmd:make-repr-dir
(defun emtmd:make-repr-dir (name &rest contents)
   ""
   (list* name contents))
;;;_  . emtmd:repr-contents
(deftype emtmd:repr-contents ()
   "Dirtree representation of directory contents"
   ;;File has to precede dir, because dir gets explored even for files.
   '(repeat (or emtmd:repr-file emtmd:repr-dir)))
;;;_   , emtmd:make-repr-contents
(defun emtmd:make-repr-contents (&rest contents)
   ""
   contents)

;;;_ , emtmd:root

(defun emtmd:root ()
   "Return the name of a new temp directory."
   (let
      ((dir
	  (make-temp-name emtb:slave-root)))
      (make-directory dir t)
      dir))



;;Config which dir to point at.
;;Since we don't know when to erase, we'll leave things there.

;;;_ , emtmd:with-dirtree
;;     * copy master dir recursively to there
;;     * Or for tree, set each buffer filename to there.

(defmacro emtmd:with-dirtree (spec &rest body)
   "Run body in directory whose contents are given by SPEC
SPEC can be any of the following:
 * A directory whose contents are copied.
 * An `emtmd:repr-contents' object
 * A list of contents.
"
   
   `(let
       ((default-directory (emtmd:root)))
       (unwind-protect
	  (progn
	     ,@body)
	  ;;When done, remove that dir tree unless it should be saved.

	  )))

;;;_ , emtmd:get-repr-contents   
(defun emtmd:get-repr-contents (dir)
   "Get a representation of the recursive contents of DIR"
   (assert dir)
   (assert (or
	      (file-name-absolute-p dir)
	      default-directory))
   (let
      ((els-w/nils
	  (mapcar
	     #'(lambda (x)
		  (cond
		     ((member (car x) '("." ".."))  nil)
		     ((eq (second x) t)
			(apply
			   #'emtmd:make-repr-dir
			   (car x)
			   (emtmd:get-repr-contents
			      (expand-file-name (car x) dir))))
		     (t 
			(emtmd:make-repr-file
			   (car x)
			   `(:file ,(expand-file-name (car x) dir))))))
	     (directory-files-and-attributes dir))))

      (apply #'emtmd:make-repr-contents
	 (delq nil els-w/nils))))



;;;_ , emtmd:repr-contents-equal

;;Sort-and-compare is tempting for this, since comparison is
;;recursive.
(defun emtmd:repr-contents-equal (a b)
   ""
   ;;Can this ever legitimately receive a file repr for either?
   (check-type a emtmd:repr-contents)
   (check-type b emtmd:repr-contents)
   (emt:sets= a b
      :test
      #'(lambda (a b)
	   (etypecase a
	      (emtmd:repr-file
		 (and 
		    (emtmd:repr-file-p b)
		    (string= 
		       (emtmd:repr-file->name a) 
		       (emtmd:repr-file->name b))
		    ;;Only compare if both are non-nil.
		    (if
		       (and
			  (emtmd:repr-file->contents a)
			  (emtmd:repr-file->contents b))
		       (string= 
			  (emtmd:repr-file->contents a) 
			  (emtmd:repr-file->contents b))
		       t)))
	      (cons 
		 (and
		    (consp b)
		    (stringp (car b))
		    (string= (car a) (car b))
		    ;;Each will be an emtmd:repr-contents
		    (emtmd:repr-contents-equal (cdr a)(cdr b))))))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/dirtree)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/dirtree.el ends here
