;;;_ emtest/types/run-types.el --- Result types for emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2008  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp

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
(require 'emtest/types/prestn-path)

;;;_. Body

;;;_ , emthow
(deftype emthow () '(repeat t))

;;;_ , emtt:explorable (Full runnable)
(defstruct (emtt:explorable
	      (:copier nil)
	      (:conc-name emtt:explorable->)
	      (:constructor emtt:make-explorable))
   "All the information needed to specify how to run a test or suite."
   (how-to-run () 
      :type emthow
      :doc "What explorer to use for this.")
   
   (prestn-path () 
      :type emt:testral:prestn-path
      :doc "The presentation path so far")

   (properties () 
      :type (repeat (list symbol *))
      :doc "The properties that this explorable has when it's run")
   (aliases () 
      :type (repeat emthow) 
      :doc "A possibly empty list of other IDs that would launch the
      same thing")) 


;;;_. Footers
;;;_ , Provides
(provide 'emtest/types/run-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/types/run-types.el ends here
