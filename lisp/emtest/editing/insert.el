;;;_ emtest/editing/insert.el --- emt-insert for inserting skeletons

;;;_. Headers
;;;_ , License
;; Copyright (C) 2008  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: 

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

(unless (fboundp 'emt:deftest)
   (defmacro emt:deftest (&rest dummy))
   (defmacro emt:if-avail (&rest dummy)))


;;;_. Body

;;;_ , emt:skeletons list of available skeletons

(defconst emt:skeletons 
   (mapcar
      #'(lambda (x)
	   (cons (symbol-name x) x))
      
      '(
	  emt:insert-requires
	  emt:insert-examples-dir
	  emt:insert-with-buf
	  emt:insert-test
	  emt:insert-clause
	  emt:insert-prop-test-thru
	  emt:insert-persister
	  emt:insert-example-def
	  emt:insert-testpoint
	  ))

   "Skeletons related to emtest" )

;;;_ , emt:insert (Dispatcher for insert commands)

;;;###autoload
(defun emt:insert (skeleton)
   "Insert a skeleton related to emtest"
  
   (interactive 
      (list
	 (let* 
	    (
	       (table emt:skeletons)
	       (element-name
		(completing-read "Insert which skeleton: "
		   table)))
	    (if
	       (string= element-name "")
	       nil
	       (cdr (assoc element-name table))))))
   
   (if skeleton
      (call-interactively skeleton)))

;;;_ , The insertion commands.
;;;_  . emt:insert-requires
(defun emt:insert-requires ()
   ""
   (interactive)
   (pp
      '(unless (fboundp 'emt:deftest)
	  (defmacro emt:deftest (&rest dummy))
	  (defmacro emt:if-avail (&rest dummy)))
      (current-buffer)))


;;;_  . emt:insert-examples-dir
(define-skeleton emt:insert-examples-dir
   "Insert skeleton to define an examples directory"
   (read-string "Package name:" )
   "(defconst "str":th:examples-dir
      (emt:expand-filename-by-load-file \"examples\") 
      \"Directory where examples are\" )"
   )

;;;_  . emt:insert-with-buf
(define-skeleton emt:insert-with-buf
   "Insert skeleton to call a form in a mock-buffer"
   ()
   "(with-buffer-containing-object
      (
	 ;;ARGS
	 )
      ;;BODY
      )
   "
   )

;;;_  . emt:insert-test

(defun emt:insert-test (suite-name)
   "Insert a test definition.

Prompts for a suite name.  The default suite-name is the previous
function."
   (interactive
      (list 
	 (let
	    ((default-suite-name
		(progn
		   (require 'rtest-edit)
		   ;;$$REPLACE ME and this should handle `of' lists.
		   ;;There is code somewhere in emt that does this.
		   (symbol-name (rtest:suite-sym-at-point)))))
	    
	 (read-string "Name of suite: " default-suite-name nil
	    default-suite-name))))
   (let
      ((pp-escape-newlines nil))
      (pp
	 `(emt:deftest-3 ,(intern suite-name)
	     ,emt:insert:clause-form)
	 
      
	 (current-buffer))))


;;;_  . emt:insert-clause
'  ;;$$OBSOLETE
(define-skeleton emt:insert-clause
   "Insert an emtest clause"
   ()
   "
   (  \"Situation: WRITEME.
Response: WRITEME.\"
      (progn) ;;Test-form
      )
"
   )
(defconst emt:insert:clause-form 
   '(()
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")))
   "" )
(defun emt:insert-clause ()
   ""
   
   (interactive)
   (pp
      emt:insert:clause-form
      (current-buffer)))

;;;_  . emt:insert-prop-test-thru

(defun emt:insert-prop-test-thru (suite-name)
   "Insert an annotation: test this suite thru another suite.

Prompts for a suite name.  The default suite-name is the previous
function."
   (interactive
      (list 
	 (let
	    ((default-suite-name
		(progn
		   ;;$$REPLACE ME, same as above
		   (require 'rtest-edit)
		   (symbol-name (rtest:suite-sym-at-point)))))
	    
	 (read-string "Name of suite: " default-suite-name nil
	    default-suite-name))))
   
   (skeleton-insert 
      '(
	  ()
	  
	  "(put '"str" 'emt:test-thru
   'THE-FUNCTION-TO-TEST-IT-THRU)")
      nil suite-name))

;;;_  . emt:insert-persister
(defun emt:insert-persister ()
   "Insert a persister"
   (interactive)
   (require 'org-id nil)
   (let
      ((id (org-id-new "dbid")))
      (pp
	 `(emt:persist ,id)
	 (current-buffer))))

;;;_  . emt:insert-example-def
(defun emt:insert-example-def ()
   ""
   
   (interactive)
   (let
      ((id (org-id-new "xmp")))
      (pp
	 `(defconst ,(intern id)
	     (emt:eg:define+ 
		((tag value) list)
		items-start-here))
	 
	 (current-buffer))))
;;;_  . emt:insert-testpoint
(defun emt:insert-testpoint ()
   ""
   
   (interactive)
   (let
      ((id (org-id-new "tp")))
      ;;Would like to surround a region as skeletons can do.  But then
      ;;can't easily get `id'.
      (pp
	 `(emtp ,(intern id)
	     (arg1))
	 (current-buffer))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/insert)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/insert.el ends here
