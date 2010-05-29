;;;_ emtest/common/persist/testhelp.el --- Testhelp for emt-persist

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
(require 'emtest/common/persist)
(require 'emtest/testhelp/tagnames)

;;;_. Body

;;;_ , Examples

;;;_  . Interface for testing with persists
;;$$OBSOLESCENT
(defconst emt:persist:thd:funcall-examples
   (emtg:define+
      ((project emtest)(library tester)(section emt:funcall))
      (transparent-tags () (type subtype))
      (item
	 ((type archive-placeholder))
	 (emt:db:make-id-index
	    :id "a"
	    :backend '(persist "Dummy")))
      (item
	 ((type version-placeholder))
	 (emt:db:make-version-index
	    :id-index (emtg (type archive-placeholder))
	    :version-id "v.1"))
   

      ;;A bit wobbly - careful of the symbol vs function-quoted symbol
      ;;distinction.
      (item
	 ((type call-sexp))
	 (list #'equal 
	    1 
	    (emtg (type archive-placeholder))))))

;;;_  . Test data

(defconst emt:persist:thd:examples
   (emtg:define+ xmp:077db118-e844-49f1-aa63-3224e0e2b6f7
      ((project emtest)(library persist))
      (item ((type emt:db:single))
	 (emt:db:make-single
	    :version-id 0
	    :value 12
	    :find-value nil
	    :use-category 'correct-answer
	    :notes ()
	    :creation-time 0))
      (transparent-tags () (type foundp))
      (group ((count 0))
	 (item ((type emt:db:persist-archive))
	    (emt:db:make-persist-archive
	       :list (emtg (type versions))))
	 (item ((type id)) 0)
	 (item ((type values))
	    ())
	 (item ((type versions))
	    ()))

      (type-must-be ((type whole-db)) emt:db:record-alist)
      (type-must-be ((type emt:db:persist-archive)) emt:db:persist-archive)
      (type-must-be ((type versions)) (repeat emt:db:single))
      (type-must-be ((type archive-placeholder))
	 emt:db:id-index)
      ;;Can't type-check this for bootstrap reasons - emt:funcall
      ;;"recognizes" this type of object as a persist placeholder and
      ;;tries to look up its value.
      ;;    (type-must-be ((type version-placeholder))
      ;;       emt:db:version-index)

      (group ((count 1))
	 (item ((type whole-db))
	    (list
	       (cons 
		  (emtg (type id))
		  (emtg (type emt:db:persist-archive)))))
	 (item ((type emt:db:persist-archive))
	    (emt:db:make-persist-archive
	       :list (emtg (type versions))))
	 (item ((type id)) 'x0)
	 (item ((type values))
	    '(12))
	 (item ((type versions))
	    (list
	       (emt:db:make-single
		  :version-id 0
		  :value 12
		  :find-value nil
		  :use-category 'correct-answer
		  :notes ()
		  :creation-time 0))))

      (group ((section persist-viewer))
	 (item ((type whole-db))
	    (list
	       (cons 
		  (emtg (type id))
		  (emtg (type emt:db:persist-archive)))))
      
	 (item ((type emt:db:persist-archive))
	    (emt:db:make-persist-archive
	       :list (emtg (type versions))))
	 (item ((type id)) 'x10)
	 (item ((type version-id)) 'fg95mg)
	 (item ((type 1-value))
	    "This is the text of the stored item.")
	 (item ((type archive-placeholder))
	    (emt:db:make-id-index
	       :id (emtg (type id))
	       :backend '(persist "Dummy")))
	 (item ((type version-placeholder))
	    (emt:db:make-version-index
	       :id-index (emtg (type archive-placeholder))
	       :version-id (emtg (type version-id))))
	 (item ((type values))
	    (list (emtg (type 1-value))))
	 (item ((type 1-version))
	    (emt:db:make-single
	       :version-id (emtg (type version-id))
	       :value (emtg (type 1-value))
	       :find-value nil
	       :use-category 'correct-answer
	       :notes ()
	       :creation-time 0))
      
	 (item ((type versions))
	    (list (emtg (type 1-version))))
	 (item ((type data)(name got))
	    "Mismatched string")
	 (item ((type tried)(foundp t))
	    (make-emt:result:diag:tried-persist-version.
	       :arg-ix 2
	       :placeholder
	       (emtg (type version-placeholder))))
	 (item ((type tried)(foundp nil))
	    (make-emt:result:diag:tried-persist-archive.
	       :arg-ix 2
	       :placeholder
	       (emtg (type archive-placeholder))
	       :use-category 'correct-answer
	       :reason 'none-found))
      
;; 	 (item ((type result-diag)(foundp t))
;; 	    (make-emt:result:diag:call
;; 	       :status nil
;; 	       :tried 
;; 	       (list (emtg (type tried)(foundp t)))
;; 	       :info-about (list)
;; 	       :call-sexp
;; 	       `(equal
;; 		   ,(emtg (type data)(name got))
;; 		   ,(emtg (type archive-placeholder)))))

;; 	 (item ((type result-diag)(foundp nil))
;; 	    (make-emt:result:diag:call
;; 	       :status nil
;; 	       :tried 
;; 	       (list (emtg (type tried)(foundp nil)))
;; 	       :info-about ()
;; 	       :call-sexp
;; 	       `(equal
;; 		   ,(emtg (type data)(name got))
;; 		   ,(emtg (type archive-placeholder)))))

	 (item ((type formatted-persist-data))
	    `(object
		,(emtg (type data)(name got))
		,(emtg (type archive-placeholder)))))
   
   
   
      ))

;;;_ , Helper functions

;;;_  . emt:db:internal:ts:mock

(defmacro* emt:db:internal:ts:mock (initial-db &rest body)
   ""

   `(let
       ((db-353 
	   (emt:db:make-whole
	      :list ,initial-db)))
       (flet
	  ((emt:db:internal:get-all (&rest dunno)
	   db-353)
	  (emt:db:internal:set-all (index arg) 
	     (setq db-353 arg)))
       
       ,@body)))

;;;_  . emt:db:set:th

;;With a mock database and with id bound neatly.
(defmacro* emt:db:set:th ((&keys initial-db) ((id-sym id)) &rest body)
   "
ID-SYM will be bound to a `emt:db:id-index' as if by `let'"
   
   `(let
       ((,id-sym 
	   (emt:db:make-id-index
	      :id ,id
	      :backend 'dummy)))

       (emt:db:internal:ts:mock ,initial-db
	  ,@body)))

;;;_  . Testhelpers to get raw version list from db
;;;_   , emt:db:get-versions
(defun emt:db:get-versions (id &optional filter)
   ""
   ;;$$REPLACE ME
   ;;Use `tinydb-alist-update' with this filter as functionality.
   (let*
      ((x (emt:db:by-ix:get-record id t)))
      (remove*
	 filter
	 (emt:db:persist-archive->list x)
	 :test-not
	 #'(lambda (filter obj)
	      (emt:db:use:subtype-of
		 (emt:db:single->use-category obj)
		 filter)))))

;;;_   , emt:db:get-all-values
(defun emt:db:get-all-values (id &optional filter)
   ""
   (mapcar
      #'emt:db:single:get-value
      (emt:db:get-versions id filter)))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/persist/testhelp)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/persist/testhelp.el ends here
