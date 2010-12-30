;;;_ emtest/editing/pcmpl-emtest/tests.el --- Tests for pcmpl-elisp

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

(require 'pcmpl-elisp)
(require 'emtest/editing/pcmpl-emtest)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/tagnames/testhelp)
(require 'emtest/testhelp/mocks/filebuf)
(require 'emtest/testhelp/standard)
(require 'emtest/main/define)


;;;_. Body

;;;_   , Tests

(emt:deftest-3 pcomplete/emacs-lisp-mode/emtg
   (nil
      (progn
	 (emt:doc "Validation. The functor `emtg' is recognized as a command
completion")
	 (and
	    (member "emtg"
	       (pcomplete-elisp-get-command-names))
	    t)))
   (nil
      (progn
	 (emt:doc "Behavior: The functor `emtg' is parsed correctly by
`pcomplete-parse-elisp-arguments'.")
	 (emtb:with-buf
	    (:sexp
	       '(emtg !)
	       :point-replaces "!")
	    (emacs-lisp-mode)
	    (destructuring-bind
	       (args &rest positions)
	       (pcomplete-parse-elisp-arguments)
	       (and
		  (equal args
		     '("emtg" "")))))))
   (nil
      (progn
	 (emt:doc "Position: complete outer form.")
	 (emtg:th:with-example-examples
	    (pcomplete:th:test :sexp
	       '(emtg !)
	       :completions
	       '("dummy-tag" "a" "b")
	       :known-heads
	       ("emtg")))))
   (nil
      (progn
	 (emt:doc "Situation: The existing examples are exactly those in
`emtg:th:with-example-examples'. 
Point is on first arg, on tag.")
	 (emt:doc "Behavior: offer the known tags as completions.")
	 (emtg:th:with-example-examples
	    (pcomplete:th:test :sexp
	       '(emtg
		   (!))
	       :completions
	       '("dummy-tag" "a" "b")
	       :known-heads
	       ("emtg")))))
   (nil
      (progn
	 (emt:doc "Situation: The existing examples are exactly those in
`emtg:th:with-example-examples'.
One tag-value arg has already been given.
Point is on second arg, on tag.
`pcomplete-use-paring' is true.")
	 (emt:doc "Behavior: offer only the other tags as completions.")
	 (emtg:th:with-example-examples
	    (let
	       ((pcomplete-use-paring t))
	       (pcomplete:th:test :sexp
		  '(emtg
		      (a t)
		      (!))
		  :completions
		  '("dummy-tag" "b")
		  :known-heads
		  ("emtg"))))))
   (nil
      (progn
	 (emt:doc "Situation: The existing examples are exactly those in
`emtg:th:with-example-examples'.
Point is on first arg, on value.
Tag is already given as dummy-tag.")
	 (emt:doc "Behavior: offer the known values of `dummy-tag' as completions.")
	 (emtg:th:with-example-examples
	    (pcomplete:th:test :sexp
	       '(emtg
		   (dummy-tag !))
	       :completions
	       '("1" "2")
	       :known-heads
	       ("emtg"))))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/editing/pcmpl-emtest/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/editing/pcmpl-emtest/tests.el ends here
