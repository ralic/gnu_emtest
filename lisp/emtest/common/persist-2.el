;;;_ emtest/common/persist-2.el --- Persistence for testing

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp,maint,internal,data

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

;; Redesigned persistence for Emtest


;;;_ , Requires

(require 'tinydb/persist)

;;;_. Body
;;;_ , Types
;;;_  . emt:db:record
(defstruct (emt:db:record
	      (:type list)
	      (:copier nil)
	      (:constructor emt:db:make-record)
	      (:conc-name emt:db:record->))
   "A record in the database"
   key ;;First so that we can use assoc
   use-category
   timestamp
   value)

;;;_  . use-category type
(deftype emt:persist:use-category () 
   '(member correct-answer correct-type wrong-answer nil))
;;;_ , Accessing items

;;;_  . emt:db:get-value
(defun emt:db:get-value (backend id &optional category)
   ""
   
   (let*
      ((all
	  (emt:db:internal:get-all backend))
	 (cell
	    (assoc id all)))
      (if cell
	 cell
	 ;;$$IMPROVE ME  Would like a dedicated error value
	 (error "Key not found: %s in backend %s" id backend))))


;;;_  . emt:db:set-value
(defun emt:db:set-value (backend id value &optional dummy-category)
   ""
  
   (let*
      ((all
	  (remove*
	     id
	     (emt:db:internal:get-all backend)
	     :key #'emt:db:record->key)))
      (push
	 (emt:db:make-record
	    :key          id
	    :use-category 'correct-answer
	    :timestamp    (current-time)
	    :value        value)
	 all)
      (emt:db:internal:set-all backend all)))


;;;_ , The database itself
;;;_  . emt:db:internal:tq-alist
(defvar emt:db:internal:tq-alist 
   '()
   "Alist from absolute filenames to file tqs" )

;;;_  . Make the queues - one for each distinct filename
(defun emt:db:internal:name->tq (filename)
   ""
   (or
      (let
	 ((cell (assoc filename emt:db:internal:tq-alist)))
	 (second cell))
      (let 
	 ((filetq
	     (tinydb-persist-make-q filename '() nil #'listp)))
	 (push (list filename filetq) emt:db:internal:tq-alist)
	 filetq)))



;;;_  . emt:db:internal:get-all
(defun emt:db:internal:get-all (backend)
   ""
   ;;For now, always use tinydb.el as the backend
   (let
      ((filename
	  (second backend)))
      (tinydb-get-obj (emt:db:internal:name->tq filename))))

;;;_  . emt:db:internal:set-all
(defun emt:db:internal:set-all (backend value)
   ""
   
   ;;For now, always use tinydb.el as the backend
   (let
      ((filename
	  (second backend)))
      (tinydb-set-obj (emt:db:internal:name->tq filename) value)))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/persist-2)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/persist-2.el ends here
