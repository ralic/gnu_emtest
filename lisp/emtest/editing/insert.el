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

(require 'emtest/editing/lisp)
(require 'pp)
(require 'utility/uuid)

;;;_. Body

;;;_ , emt:skeletons list of available skeletons

(defconst emt:skeletons 
   (mapcar
      #'(lambda (x)
	   (cons (symbol-name x) x))
      
      '(
	  emt:insert-requires
	  emt:insert-require-tp
	  emt:insert-examples-dir
	  emt:insert-with-buf
	  emt:insert-test
	  emt:insert-clause
	  emt:insert-prop-test-thru
	  emt:insert-persister
	  emt:insert-db-id
	  emt:insert-example-def
	  emt:insert-testpoint
	  emt:insert-testpoint-control
	  ))

   "Skeletons related to emtest" )

;;;_ , emtest:insert (Dispatcher for insert commands)

;;;###autoload
(defun emtest:insert (skeleton)
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
      '(unless (fboundp 'emt:if-avail)
	  (defmacro emt:deftest-3 (&rest dummy))
	  (defmacro emt:if-avail (&rest dummy)))
      (current-buffer)))
;;;_  . defconst emt:insert:clause-form 
(defconst emt:insert:require-tp
   '(progn
       (eval-when-compile
	  (require 'emtest/testhelp/testpoint/requirer))
       (emtp:require))
   
   "Requires form for testpoint")

(defun emt:insert-require-tp ()
   "Insert a requires form for testpoint"
   (interactive)
   (pp emt:insert:require-tp (current-buffer)))

;;;_  . emt:insert-examples-dir
(define-skeleton emt:insert-examples-dir
   "Insert skeleton to define an examples directory"
   (read-string "Package name:" )
   "(defconst "str":th:examples-dir
      (emt:expand-filename-here \"examples\") 
      \"Directory where examples are\" )"
   )

;;;_  . emt:insert-with-buf
(define-skeleton emt:insert-with-buf
   "Insert skeleton to call a form in a mock-buffer"
   ()
   "(emtb:with-buf
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
	 ;;$$REFACTOR ME - code is shared with emt:insert-prop-test-thru
	 (let
	    ((default-suite-name
		(symbol-name (emtel:suite-sym-at-point))))
	 (read-string "Name of suite: " default-suite-name nil
	    default-suite-name))))
   (let
      ((pp-escape-newlines nil))
      (pp
	 `(emt:deftest-3 ((of ',(intern suite-name)))
	     ,emt:insert:clause-form)
	 (current-buffer))))


;;;_  . emt:insert-clause

(defconst emt:insert:clause-form 
   '(()
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")))
   "Sexp og a skeleton clause" )
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
		(symbol-name (emtel:suite-sym-at-point))))
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
   (let
      ((id (utiuid:generate "dbid")))
      (pp
	 `(emt:assert (emt:eq-persist-p #'equal _ ,id))
	 (current-buffer))))
;;;_  . emt:insert-db-id
(defun emt:insert-db-id ()
   "Insert a database id"
   
   (interactive)
   (insert
      "(db-id `(persist ,_$$WRITEME))"))
;;;_  . emt:insert-example-def
(defun emt:insert-example-def ()
   ""
   
   (interactive)
   (let
      ((id (utiuid:generate "xmp")))
      (pp
	 `(defconst ,(intern id)
	     (emtg:define+ 
		((tag value) list)
		items-start-here))
	 
	 (current-buffer))))
;;;_  . emt:insert-testpoint
(defun emt:insert-testpoint ()
   ""
   
   (interactive)
   (let
      ((id (utiuid:generate "tp")))
      ;;Would like to surround a region as skeletons can do.  But then
      ;;can't easily get `id'.
      (pp
	 `(emtp ,(intern id)
	     (arg1))
	 (current-buffer))))

(defun emt:insert-testpoint-control ()
   ""
   
   (interactive)
   (let
      ()
      (insert "\n;;Clauses are tp, tp*, tp-reached, mock*, and finally\n")
      (pp '(emtp:eval t )
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
