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

;;;_ , Constant
(defconst emt:tab:N/A (list) 
   "Symbol provided to mean `no entry'." )

;;;_ , emt:tab:table helpers
(defun emt:tab:sym->index (table sym)
   ""
   (let
      ((cell (assq sym (emt:tab:table->sym-alist table))))
      (when cell (cdr cell))))

(defun emt:tab:--set-rows-to-self (table)
   ""
   (mapcar
      #'(lambda (row)
	   (setf
	      (aref row 1)
	      table))
      (emt:tab:table->rows table)))
;;;_ , Row helpers
(defsubst emt:tab:row->table (row)
   ""
   (aref row 1))

;;;_ , Building a table

(defmacro emt:tab:make (docstring definition &rest rows)
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
		       ;; The first object passed becomes the actual
		       ;; argument list.
		       ((row-docstring &key ,@proc-args))
		       ;; Rows are vectors in the following format:
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

;;;_ , Accessing a table

(defun emt:tab (row sym)
   "Return the column of ROW corresponding to SYM.

ROW must be a row of a table made by `emttab:make'"
   (let*
      ((table (emt:tab:row->table row))
	(index (emt:tab:sym->index table sym)))
      (if index
	 (aref row index)
	 (error "No such column label: %s" sym))))


;;;_ , emt:tab:for-each-row

(defmacro emt:tab:for-each-row (table var-sym &rest body)
   "Evaluate BODY once for each row of TABLE with VAR-SYM bound to the row."
   
   (let
      ((lam 
	  `#'(lambda (,var-sym)
	       ,@body)))
      
      `(mapcar ,lam (emt:tab:table->rows table))))


(emt:deftest-3
   ((of 'emt:tab:make))
   (nil
      (let
	 ((table
	     (emt:tab:make
		"My table of data for my test"
		(input result)
		("Trivial example" :input 0 :result 0)
		("More complex example" :input 10 :result 20))))
	 (emt:doc "Situation: We have a table.  We look at it in
   various ways.")

	 (emt:doc "Operation: Loop thru it.")
	 (emt:tab:for-each-row table i
	    (emt:doc "Check: Value is one of ours.")
	    (emt:assert
	       (member
		  (emt:tab i 'input)
		  '(0 10)))
	    (emt:assert
	       (member
		  (emt:tab i 'result)
		  '(0 20)))
	    (emt:doc "Operation: Use the values in a little test.")
	    (emt:doc "Result: the values correspond within rows")
	    (emt:assert
	       (equal
		  (* 2 (emt:tab i 'input))
		  (emt:tab i 'result))))
	 

	 )))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tables)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tables.el ends here
