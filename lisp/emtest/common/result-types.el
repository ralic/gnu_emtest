;;;_ emtest/common/result-types.el --- Result types for emtest

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
(require 'emtest/common/testral-types)

;;;_. Body

;;;_ , emthow
;;Most of this could be moved into runner/explorers/ directory.  But
;;it must be available to launchers too.
;;;_  . Base
(defstruct (emthow
	      (:copier nil)
	      (:constructor emthow:make)
	      (:conc-name emthow->))
   ""
   ;;Abstract.
   )
;;;_  . emthow:hello
(defstruct (emthow:hello
	      (:copier nil)
	      (:constructor emthow:make-hello)
	      (:conc-name emthow:hello->)
	      (:include emthow))
   "")

;;;_  . emthow:invalid
(defstruct (emthow:invalid
	      (:copier nil)
	      (:constructor emthow:make-invalid)
	      (:conc-name emthow:invalid->)
	      (:include emthow))
   "")

;;;_  . emthow:suite
(defstruct (emthow:suite
	      (:copier nil)
	      (:constructor emthow:make-suite)
	      (:conc-name emthow:suite->)
	      (:include emthow))
   ""
   ;;Type not expressed.  Co-varies with tester.
   ;;$$IMPROVE ME Type me (as a symbol)
   ;;$$This is NOT emt:testral:suite-id, which is just a name.
   suite-ID)

;;;_  . emthow:form
(defstruct (emthow:form
	      (:copier nil)
	      (:constructor emthow:make-form)
	      (:conc-name emthow:form->)
	      (:include emthow))
   ""
   ;;A test form.
   test-form
   )
;;;_  . emthow:indexed-clause
(defstruct (emthow:indexed-clause
	      (:copier nil)
	      (:constructor emthow:make-indexed-clause)
	      (:conc-name emthow:indexed-clause->)
	      (:include emthow))
   ""
   (suite-sym () :type symbol)
   ;;Formerly index was considered part of context.
   (clause-index 0 :type integer))


;;;_  . emthow:library
(defstruct (emthow:library
	      (:copier nil)
	      (:constructor nil)
	      (:conc-name emthow:library->)
	      (:include emthow))
   ""
   ;;Abstract.
   )
;;Not settled yet


(defstruct (emthow:library:elisp-load
	      (:copier nil)
	      (:constructor emthow:make-library:elisp-load)
	      (:conc-name emthow:library:elisp-load->)
	      (:include emthow:library))
   ""
   load-name
   lib-sym
   )
(defstruct (emthow:library:elisp-file
	      (:constructor emthow:make-library:elisp-file)
	      (:conc-name emthow:library:elisp-file->)
	      (:include emthow:library))
   ""
   file-name
   lib-sym
   )

;;;_  . emthow:project
;;Not settled yet
(defstruct (emthow:project
	    (:constructor emthow:make-project)
	    (:conc-name emthow:project->)
	    (:include emthow))
  ""
  name
  )

;;;_  . emthow:all-projects
(defstruct (emthow:all-projects
	    (:constructor emthow:make-all-projects)
	    (:conc-name emthow:all-projects->)
	    (:include emthow))
   ""
   ;;No fields
   )

;;;_  . emthow:all-libraries
(defstruct (emthow:all-libraries
	    (:constructor emthow:make-all-libraries)
	    (:conc-name emthow:all-libraries->)
	    (:include emthow))
   ""
   ;;No fields
   )

;;;_  . emthow:all-testers
(defstruct (emthow:all-testers
	    (:constructor emthow:make-all-testers)
	    (:conc-name emthow:all-testers->)
	    (:include emthow))
   ""
   )

;;;_  . emthow:from-t-dir
(defstruct (emthow:from-t-dir
	      (:copier nil)
	      (:constructor emthow:make-from-t-dir)
	      (:conc-name emthow:from-t-dir->)
	      (:include emthow))
   ""
   (dir-name () :type string))


;;;_  . emthow:from-dir
(defstruct (emthow:from-dir
	      (:copier nil)
	      (:constructor emthow:make-from-dir)
	      (:conc-name emthow:from-dir->)
	      (:include emthow))
   ""
   (dir-name () :type string))
;;;_  . emthow:dynamic
(defstruct (emthow:dynamic
	      (:copier nil)
	      (:constructor emthow:make-dynamic)
	      (:conc-name emthow:dynamic->)
	      (:include emthow))
   "Special method to make explorables at runtime."
   name
   params)


;;;_  . emtt:explorable (Full runnable)
(defstruct (emtt:explorable
	      (:copier nil)
	      (:conc-name emtt:explorable->)
	      (:constructor emtt:make-explorable))
   "All the information needed to specify how to run a test or suite/"
   (how-to-run () 
      :type emthow
      :doc "What to launch for this exploration.")
   
   (prestn-path () 
      :type emt:testral:partial-suite-id
      :doc "The presentation path so far")

   (properties () 
      :type (repeat (list symbol *))
      :doc "The properties that this explorable has when it's run")
   ;;Aliases might also allow a string as UUID
   (aliases () 
      :type (repeat emthow) 
      :doc "A possibly empty list of other IDs that would launch the
      same thing")) 

;;;_  . emtt:dynamic-method
;;$$USE ME
(defstruct (emtt:dynamic-method
	      (:constructor emtt:make-dynamic-method)
	      (:copier nil)
	      (:conc-name emtt:dynamic-method->))
   "A dynamic exploration method."
   name
   keys)

;;;_  . emtt:method (Union of those types)
;;$$USE ME
(deftype emtt:method ()
   "A static or dynamic exploration method, for test-runner-info"
   '(or emtt:dynamic-method emtt:explorable))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/common/result-types)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/common/result-types.el ends here
