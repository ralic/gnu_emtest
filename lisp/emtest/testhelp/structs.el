;;;_ emtest/testhelp/structs.el --- Testhelp for structs: examples.

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp,maint,internal

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

(eval-when-compile (require 'cl))

;;;_. Body
;;;_ , AxB
(defstruct (emths:struct-AxB
	      (:constructor emths:make-struct-AxB)
	      (:conc-name emths:struct-AxB->))
   "An example structure with two untyped fields"
   field-A
   field-B)

;;;_   , Examples

(defconst emths:struct-AxB:1 
   (emths:make-struct-AxB
      :my-field 'val1 
      :my-second-field 'val2)
   "" )

(defconst emths:struct-AxB:2
   (emths:make-struct-AxB
      :my-field 'val1
      :my-second-field 'val2)
   "" )

;;;_ , emths:struct-container

(defstruct (emths:struct-container
	      (:constructor emths:make-struct-container)
	      (:conc-name emths:struct-container->))
   "A structure that contains one rtest-struct"
   obj)

;;;_  . Examples

(defconst emths:struct-container:1 
   (emths:make-struct-container
      :obj emths:struct-AxB:1)
   "" )

(defconst emths:struct-container:2
   (emths:make-struct-container
      :obj emths:struct-AxB:2)
   "" )

;;;_ , emths:struct-as-list
(defstruct (emths:struct-as-list 
	      (:type list) 
	      :named
	      (:constructor emths:make-struct-as-list)
	      (:conc-name emths:struct-as-list->))

   "A structure in list form, tagged"
   field-A
   field-B)
;;;_  . Examples

(defconst emths:struct-as-list:ex1 
   (emths:make-struct-as-list
      :field-A 'val1 
      :field-B 'val2)
   "" )

;;;_ , emths:struct-as-vector
(defstruct (emths:struct-as-vector 
	      (:type vector) 
	      :named
	      (:constructor emths:make-struct-as-vector)
	      (:conc-name emths:struct-as-vector->))
   
   "A structure in vector form, tagged"
   field-A
   field-B)

;;;_  . Examples

(defconst emths:struct-as-list:ex1 
   (emths:make-struct-as-list
      :field-A 'val1 
      :field-B 'val2)
   "" )

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/structs)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/structs.el ends here
