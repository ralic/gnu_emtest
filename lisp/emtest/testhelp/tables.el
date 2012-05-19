;;;_ emtest/testhelp/tables.el --- Tables, replacing tagnames

;;;_. Headers
;;;_ , License
;; Copyright (C) 2012  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp,maint

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

(require 'cl)

;;;_. Body
;;;_ , Types

(defstruct (emt:tab:table
	      (:constructor emt:tab:make-table)
	      (:conc-name emt:tab:table->))
   docstring
   ;; Alist from symbol to index.
   sym-alist
   ;; Each row is a vector headed by the symbol emt:tab:--row, with
   ;; element 2 being the whole table (so we can use emt:tab just
   ;; knowing the row)
   rows)

(defconst emt:tab:N/A (list) 
   "Symbol provided to mean `no entry'." )
(defun emt:tab:--set-rows-to-self (table)
   ""
   (mapcar
      #'(lambda (row)
	   (setf
	      (aref row 1)
	      table))
      (emt:tab:table->rows table)))


(defmacro emt:tab:define (docstring definition &rest rows)
   "Define a table of type `emt:tab:table'"

   ;; For now, slot is exactly a symbol.  Later it can allow
   ;; defaults and initialization forms.

   ;; Also allow the keys:
   ;; :no-define: it's just a constructed aux& argument
   ;; :persist-id: like :no-define but define the slot as persisting.
   ;; :primary-key which will be used by persist-id and should be
   ;; unique (unenforced)
   ;; Process slot definitions, creating various list objects.
   (destructuring-bind (sym-alist proc-args proc-body)
      (loop
	 for slot in definition
	 for i upfrom 3
	 collect (cons slot i) into sym-alist
	 collect slot into proc-args
	 collect slot into proc-body
	 finally return (list sym-alist proc-args proc-body))

      ;; Make a corresponding row-define function
      (let
	 ((define-row
	     (eval
		`(function*
		    (lambda
		       (row-docstring &key ,@proc-args)
		       (vector 
			  ;; 0 - a symbol indentifying it as some type
			  ;; of row.
			  'emt:tab:--row
			  ;; 1 - the table itself.  Set later because
			  ;; we don't know that table when this is run.
			  nil  
			  ;; 2 - the row's docstring.
			  row-docstring
			  ;; 3+ - row data
			  ,@proc-body))))))
	    
	 ;; Return a form that defines this table.
	 `',(let
	       ((table
		   (emt:tab:make-table
		      :docstring docstring
		      :sym-alist 
		      (cons
			 '(docstring . 2)
			 sym-alist)  
		      :rows
		      (mapcar define-row rows))))
	       (emt:tab:--set-rows-to-self table)
	       table))))

'
(emt:tab:define
   "My table of data for my test"
   ;; Structure is similar to defstruct
   (input
      result)
   ("Trivial example" :input 0 :result 0)
   ("More complex example" :input 10 :result 20))

(defun emt:tab (x &rest args)
   ""
   
   (interactive)
   ;; If we got a table, args are row and column-tag, but if we got a
   ;; row, arg is column-tag and we get table from 
   (let*
      ()
      
      ))

;; Print it out?

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tables)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tables.el ends here
