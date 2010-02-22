;;;_ editor/pcmpl-emtest.el --- pcomplete Elisp functions for Emtest

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

(require 'tester/testhelp/eg)
(require 'pcmpl-elisp)
(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(when (not (fboundp 'emt:deftest-2))
    (defmacro emt:deftest-2 (&rest dummy))
    (defmacro emt:if-avail (&rest dummy)))

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

;;;_   , Tests

(rtest:deftest pcomplete/emacs-lisp-mode/emt:eg
   ;;Uses test-helpers `emt:eg:th:with-example-examples' and
   ;;`pcomplete:th:test'.
   ("Validation. The functor `emt:eg' is recognized as a command
completion" 
      (and
	 (member "emt:eg" (pcomplete-elisp-get-command-names))
	 t))
   

   ( "Behavior: The functor `emt:eg' is parsed correctly by
`pcomplete-parse-elisp-arguments'."
      (with-buffer-containing-object
	 (:sexp '(emt:eg !)
	    :point-replaces "!")
	 (emacs-lisp-mode) ;;Must be in emacs-lisp-mode
	 (destructuring-bind
	    (args &rest positions)
	    (pcomplete-parse-elisp-arguments)
	    (and
	       (equal
		  args 
		  '("emt:eg" ""))))))

   (  "Position: complete outer form."
      (emt:eg:th:with-example-examples
	 (pcomplete:th:test
	    :sexp '(emt:eg !)
	    :completions '("dummy-tag" "a" "b")
	    :known-heads ("emt:eg"))))


   (  "Situation: The existing examples are exactly those in
`emt:eg:th:with-example-examples'. 
Point is on first arg, on tag.
Behavior: offer the known tags as completions."
      (emt:eg:th:with-example-examples
	 (pcomplete:th:test
	    :sexp '(emt:eg (!))
	    :completions '("dummy-tag" "a" "b")
	    :known-heads ("emt:eg"))))

   (  "Situation: The existing examples are exactly those in
`emt:eg:th:with-example-examples'.
One tag-value arg has already been given.
Point is on second arg, on tag.
`pcomplete-use-paring' is true.
Behavior: offer only the other tags as completions."
      (emt:eg:th:with-example-examples
	 (let
	    ((pcomplete-use-paring t))
	    (pcomplete:th:test
	       :sexp '(emt:eg (a t)(!))
	       :completions '("dummy-tag" "b")
	       :known-heads ("emt:eg")))))

   (  "Situation: The existing examples are exactly those in
`emt:eg:th:with-example-examples'.
Point is on first arg, on value.
Tag is already given as dummy-tag.
Behavior: offer the known values of `dummy-tag' as completions."
      (emt:eg:th:with-example-examples
	 (pcomplete:th:test
	    :sexp '(emt:eg (dummy-tag !))
	    :completions '("1" "2")
	    :known-heads ("emt:eg"))))

   )

;;;_  . pcomplete/emacs-lisp-mode/emt:eg:narrow
;;;###autoload
(defun pcomplete/emacs-lisp-mode/emt:eg:narrow ()
   ""
   ;;May make a common worker instead.
   (pcomplete-nested
      (pcomplete/emacs-lisp-mode/emt:eg)))
;;;_ , Helpers for emt-define
;;;_  . pcomplete/emacs-lisp-mode/emt:define-2
(defun pcomplete/emacs-lisp-mode/emt:define-2 ()
   ""
   ;;Suite name
   (pcomplete-here
      (list
	 (progn
	    (require 'rtest-edit)
	    ;;Have to exit from here to find it.
	    (save-excursion
	       (goto-char (pcomplete-begin 'first))
	       (up-list -1)
	       (symbol-name (rtest:suite-sym-at-point))))))

   ;;Props
   ;;How to indicate that this is optional and could go right thru to
   ;;the other?  That's mostly a user interface issue.
   (pcomplete-nested
      (progn
	 (pcomplete-here '("props"))
	 (while t
	    (pcomplete-nested
	       (progn
		  ;;Suggest the common properties
		  (pcomplete-here '("db-id"))
		  ;;Punt their suggested values for now.  Suggestions
		  ;;will be respective of the property name
		  )))))
   (while t
      ;;$$Could also choose to copy a clause, using
      ;;(pcomplete-build-furthers-obj "*Copy a clause*" LAMBDA-FORM nil)
      ;;That's an alternative to descending.
      (pcomplete-nested
	 (progn
	    ;;Clause.
	    (pcomplete-here-sexps
	       (list
		  "Situation: WRITEME.
Response: WRITEME."))
	    ;;This could volunteer common governors in all their
	    ;;glory: `emt:eg:narrow', `with-buffer-containing-object',
	    ;;`emtp:eval', etc
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

(provide 'editor/pcmpl-emtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; editor/pcmpl-emtest.el ends here
