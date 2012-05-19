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

(defun emt:tab:sym->index (table sym)
   ""
   (let
      ((cell (assq sym (emt:tab:table->sym-alist table))))
      (when cell (cdr cell))))


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

(defun emt:tab (x &rest args)
   ""
   (destructuring-bind
      (table row name)
      (etypecase x
	 (emt:tab:table
	    (list
	       x
	       (car args)
	       (cadr args)))
	 
	 
	 
	 )

      )
   ;; If we got a table, args are primary key value and column-tag,
   ;; but if we got a row, arg is column-tag and we get table from
   ;; row.
   (let*
      ()
      
      ))

(defmacro emt:tab:for-each-row (table var-sym &rest body)
   ""

   (let*
      ()
      
      ))

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
