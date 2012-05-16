;;;_ emtest/editing/versioning.el --- Versioning assistance for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint, tools

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
;;;_ , Conventional docstring converters
(defun emt:ed:vers:substring->docform (str start &optional next)
   ""
   (let*
      ((raw-sub
	  (substring str start next))
	 (sub-1
	    (url-strip-leading-spaces raw-sub))
	 (sub-2
	    ;;Escape any left-parenthesis at the beginning of line
	    (replace-regexp-in-string 
	       "\n(" "\n\\(" sub-1 nil t)))
      
      (unless (string= sub-2 "")
	 (list 
	    'emt:doc sub-2))))

(defun emt:ed:vers:string->docstring-list (str)
   "Convert a string to docstring forms"
   
   (let*
      ((rv-str-list '())
	 (start 0)
	 (next)
	 (regexp
	    (concat
	       "[ \f\t\n\r\v]*"
	       (regexp-opt 
		  '("Situation:"
		      "Operation:"
		      "Response:"
		      "Afterwards:"
		      "Behavior:"
		      "Result:"
		      "Shows:"
		      "Proves:")))))
      
      (while (setq next 
		(string-match 
		   regexp
		   str (1+ start)))
	 (push
	    (emt:ed:vers:substring->docform str start next)
	    rv-str-list)
	 (setq start next))
      (push
	 (emt:ed:vers:substring->docform str start next)
	 rv-str-list)
      (nreverse (delq nil rv-str-list))))


;;;_ , Test converters

(defun emt:ed:vers:rtest->emtest (sexp)
   "Convert an rtest form to an emtest form"

   (when (listp sexp)
      (case (car sexp)
	 (put
	    (destructuring-bind
	       (put sym test-thru target)
	       sexp
	       `(put ,sym 'emt:test-thru ,target)))
	 (rtest:deftest
	    (destructuring-bind (rtest-sym sym &rest clauses)
	       sexp
	       `(emt:deftest-3 ,sym
		   ,@(mapcar #'emt:ed:vers:rtest-clause->emtest clauses)))))))

(defun emt:ed:vers:rtest-clause->emtest-x (sexp)
   ""
   (destructuring-bind (docstring form) 
      sexp
      `(nil
	  (progn
	     ,@(emt:ed:vers:string->docstring-list docstring)
	     ,form))))

(defun emt:ed:vers:rtest-clause->emtest (sexp)
   "Convert an rtest clause to an emtest clause"
   (when (listp sexp)
      (if
	 (eq (car sexp) 'quote)
	 (list 
	    'quote
	    (emt:ed:vers:rtest-clause->emtest-x (second sexp)))
	 (emt:ed:vers:rtest-clause->emtest-x sexp))))
;;;_  . emt:ed:vers:goto-next-sexp 
;;Move point directly in front of next sexp
;;Return non-nil on success, nil or error on failure.
(defun emt:ed:vers:goto-next-sexp (&optional noerror)
   ""
   
   (interactive)
   (let
      ((old-point (point)))
      (forward-sexp)
      (backward-sexp)
      (if (< (point) old-point)
	 (if noerror
	    (progn
	       (goto-char old-point)
	       nil)
	    (error "There is no sexp after point"))
	 t)))


;;;_  . Entry point
;;;###autoload
(defun emtest:versioning:convert-rtest-at-point (&optional noerror)
   ""
   
   (interactive)

   ;;Move point to directly in front of next sexp so we don't eat
   ;;comments.
   (let
      ((ok (emt:ed:vers:goto-next-sexp noerror)))
      (if ok
	 (let*
	    (  (pp-escape-newlines nil)
	       (start (point))
	       (rtest-form
		  (read (current-buffer)))
	       (emtest-form (emt:ed:vers:rtest->emtest rtest-form)))
	    (when emtest-form
	       (delete-region start (point))
	       (pp emtest-form (current-buffer)))
	    t))))


;;;_  . emt:ed:vers:cvt-rtest-buffer
(defun emt:ed:vers:cvt-rtest-buffer ()
   ""
   ;;Find the library name wrt the known dir root
   (let* 
      ((lib-path 
	  (file-name-directory
	     (file-relative-name 
		buffer-file-name
		emt:ed:vers:root-project-dir)))
	 
	 (emt-lib-name (concat lib-path "tests"))
	 (rtest-lib-name (concat lib-path "rtest")))
      
      ;;Replace it where it's used.
      (while (search-forward rtest-lib-name nil t)
	 (replace-match emt-lib-name nil t))

      ;;Convert all the forms.
      (goto-char (point-min))
      (while (emtest:versioning:convert-rtest-at-point t))))


;;;_  . emt:ed:vers:root-project-dir
;;$$IMPROVE ME Should be customizable or calculated, not handwritten.
(defconst emt:ed:vers:root-project-dir 
   "~/projects/elisp/emtest/lisp/"
   "" )
;;;_  . emtest:versioning:convert-rtest-file
;;;###autoload
(defun emtest:versioning:convert-rtest-file (filename)
   "Copy an rtest file, setting it up for emtest
FILENAME is the filename of an existing rtest file"
   (interactive
      (list 
	 ;;Could provide predicate to ensure "rtest.el" ending.  But
	 ;;that blocks directories.
	 (read-file-name "Copy which rtest file? "
	    nil
	    buffer-file-name
	    t
	    nil
	    #'(lambda (name)
		 (or
		    (file-directory-p name)
		    (string= (file-name-nondirectory name)
		       "rtest.el"))))))

   (let*
      ((path
	  (file-name-directory filename))
	 ;;Figure out the equivalent tests file
	 (emt-filename
	    (expand-file-name "tests.el" path)))
      
      ;;If the tests file already exists, complain and abort
      (when (file-exists-p emt-filename)
	 (error "Target file already exists: %s" emt-filename))
      ;;Copy to it
      (copy-file filename emt-filename)

      ;;Visit that file
      (with-current-buffer
	 (find-file-noselect emt-filename)
	 ;;Convert it
	 (emt:ed:vers:cvt-rtest-buffer)
	 ;;Save it
	 (save-buffer)))
   
   ;;Add dependencies?  Ie, find requires section and run elisp-depend.
   ;;But with maybe nothing loaded, that won't tell us what it should.

   ;;Pop that buffer up - maybe
   )

;;;_ , emt:ed:vers:make-defstruct-explicit
;;Usage:
;;Place point on already-paranthesized struct name (but not on the first character).  It can have
;;properties already such as :include.
;;Command `emt:ed:vers:make-defstruct-explicit'
;;In dired with all potentially affected files marked, query-replace
;;the symbols with their better-behaved counterparts.

(fset 'emt:ed:vers:canonize-defstruct
   [C-M-kp-left ?\C-  C-M-kp-right ?\M-w ?\C-x ?r ?s ?a ?\C-m ?\C-i ?\( ?: ?c ?o ?n ?s ?t ?r ?u ?c ?t ?o ?r ?  ?m ?a ?k ?e ?- ?\C-x ?r ?i ?a ?\C-\M-u C-M-kp-right ?\C-m ?\C-i ?\( ?: ?c ?o ?n ?c ?- ?n ?a ?m ?e ?  ?\C-x ?r ?i ?a C-M-kp-right ?- ?\C-\M-u C-M-kp-right ?\C-m ?\C-i ?\( ?: ?c ?o ?p ?i ?e ?r ?  ?n ?i ?l ?\C-\M-u C-M-kp-right])

;;How: First, put point in register b for that buffer
;;Go to where the thing is and grab name
;;Go to query buffer
;;Begin query-replace with paste
;;User will edit the name and start the replacing
;;Replace it everywhere.
(fset 'emt:ed:vers:replace-everywhere
   [?\C-  C-kp-left ?\C-  C-M-kp-right ?\C-x ?r ?s ?a ?\C-x ?r ?j ?b ?Q ?\C-x ?r ?i ?a return up])

;;;_  . Examples
;;With point-replaces "!"
'
(defstruct (fo!o)
   ""
   )
;;Becomes:
'
(defstruct (foo
	    (:constructor make-foo)
	    (:conc-name foo-))
   ""
   )

;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/versioning)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/versioning.el ends here
