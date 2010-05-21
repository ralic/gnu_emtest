;;;_ emtest/editing/pcmpl-emtest.el --- pcomplete Elisp functions for Emtest

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: tools, convenience

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

(require 'emtest/testhelp/eg)
(require 'pcmpl-elisp)

;;;_. Body

;;;_ , helpers for eg library

;;;_  . pcomplete/emacs-lisp-mode/emt:eg
;;;###autoload
(defun pcomplete/emacs-lisp-mode/emt:eg ()
   ""

   (let
      ((avail-tags (emt:eg:all-tags)))
      (while t
	 (pcomplete-here
	    '(
		("()" nested nil
		   (lambda (avail-tags)
		      (pcomplete-here-sexps avail-tags)
		      (let*
			 (  (tag
			       (pcomplete-arg-as-sexp 1))
			    (avail-args
			       (emt:eg:all-tag-args tag)))
			 (pcomplete-here-sexps avail-args)))))
	    nil nil nil (list avail-tags))

	 
	 ;;When we pass a tag, remember that we don't need to try it
	 ;;again.  Must do this here because the nested form doesn't
	 ;;get called in unentered sections.
	 (let* 
	    ((kv (pcomplete-arg-as-sexp 1))
	       (tag 
		  (if
		     (consp kv)
		     (car kv)
		     kv)))

	    (setq avail-tags (remove tag avail-tags))))))

;;;_  . pcomplete/emacs-lisp-mode/emt:eg:narrow
;;;###autoload
(defun pcomplete/emacs-lisp-mode/emt:eg:narrow ()
   ""
   ;;May make a common worker instead.
   (pcomplete-nested
      (pcomplete/emacs-lisp-mode/emt:eg)))
;;;_ , Helpers for emt-define
;;;_  . pcomplete/emacs-lisp-mode/emt:define-2
;;Adapted for emt:define-3 but untested.
(defun pcomplete/emacs-lisp-mode/emt:define-3 ()
   ""
   ;;Suite name.  $$ADAPT ME.  This will become the suggested value of
   ;;the "of" property.  Encap me for that.
   '
   (pcomplete-here
      (list
	 (progn
	    ;;Find a symbol-name outside here.
	    (save-excursion
	       (goto-char (pcomplete-begin 'first))
	       (up-list -1)
	       (symbol-name (emt:suite-sym-at-point))))))

   ;;How to indicate that this could also be a bare symbol?
   (pcomplete-nested
      (progn
	 (pcomplete-here '("props"))
	 (while t
	    (pcomplete-nested
	       (progn
		  ;;Suggest the common properties
		  (pcomplete-here '("db-id" "of" ":surrounders"))
		  ;;Punt their suggested values for now.  Suggestions
		  ;;will be respective of the property name
		  )))))
   (while t
      ;;$$ADD ME Could also choose to copy a clause, using
      ;;(pcomplete-build-furthers-obj "*Copy a clause*" LAMBDA-FORM nil)
      ;;That's an alternative to descending.
      ;;Could alternately off the template:
      ;;emt:insert:clause-form
      (pcomplete-nested
	 (progn
	    ;;Clause.
	    (pcomplete-here-sexps
	       (list
		  "Situation: WRITEME.
Response: WRITEME."))

	    ;;$$ADD ME This could volunteer common useful functors in
	    ;;all their glory: `emt:eg:narrow',
	    ;;`with-buffer-containing-object', `emtp:eval', etc
	    (pcomplete-here-sexps '((progn)))))))



;;;_   , Tests
;;It's direct

;;;_  . mockbuf with-buffer-containing-object
;;;###autoload
(defun pcomplete/emacs-lisp-mode/with-buffer-containing-object ()
   ""
   (pcomplete-nested
      (while t
	 (pcomplete-here-sexps 
	    '(:sexp :string :file :dir :visited-name :point-replaces
		:sequence))

	 ;;Case on the previous one.  Regardless the case, there must
	 ;;be exactly one step forward here.
	 (case (pcomplete-arg-as-sexp 1)
	    ;;Any object at all.  No way to indicate that in pcomplete,
	    ;;but maybe bind help.  
	    ;;((:sexp :printed-object))
	    ;;Make a blank string - user can fill it in.
	    (:string (pcomplete-here-sexps '("" (emt:eg))))
	    ;;Pick a file
	    ;;(:file)
	    ;;Pick a dir.  If a value is given, we'd like to use it to
	    ;;help expand ":file"
	    ;;(:dir)
	    ;;Suggest the usual value - strings don't work here
	    (:point-replaces (pcomplete-here-sexps '("!")))
	    ;;Recurse.
	    ;;(:sequence)
	    (t
	       (pcomplete-here '()))))))


;;;_ , Insinuation

;;;###autoload (add-hook 'emacs-lisp-mode-hook #'pcomplete-emtest-setup)
;;;###autoload
(defun pcomplete-emtest-setup ()
   ""
   (mapcar
      #'(lambda (x)
	   (add-to-list 'pcomplete-elisp-command-names-list x))
      '(  "emt:eg"
	  "emt:eg:narrow"
	  "emt:define-2"
	  "with-buffer-containing-object"
	  )))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/pcmpl-emtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/pcmpl-emtest.el ends here
