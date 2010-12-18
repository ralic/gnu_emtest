;;;_ viewers/loformat/rtest.el --- Tests for loformat

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, internal

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
(require 'viewers/loformat)
(require 'emtest/runner/define)
(require 'emtest/testhelp/mocks/filebuf)
(require 'emtest/testhelp/standard)

;;;_. Body
;;;_ , loformat:insert

(emt:deftest-3 loformat:insert
   (nil
      (progn
	 (emt:doc "Param: A bare string")
	 (emt:doc "Response: Inserts that string.")
	 (with-temp-buffer
	    (loformat:insert "a")
	    (equal "a"
	       (buffer-string)))))
   (nil
      (progn
	 (emt:doc "Param: A list of strings")
	 (emt:doc "Response: Inserts those strings in order.")
	 (with-temp-buffer
	    (loformat:insert
	       '("a" "b" "c"))
	    (equal "abc"
	       (buffer-string)))))
   (nil
      (progn
	 (emt:doc "Param: A tree of strings")
	 (emt:doc "Response: Inserts those strings in order.")
	 (with-temp-buffer
	    (loformat:insert
	       '("a"
		   ("b")
		   "c"))
	    (equal "abc"
	       (buffer-string)))))
   (nil
      (progn
	 (emt:doc "Param: A string with properties")
	 (emt:doc "Response: The inserted text has the same properties.")
	 (with-temp-buffer
	    (let
	       ((str "abc")
		  (props
		     '(face bold)))
	       (set-text-properties 0 3 props str)
	       (loformat:insert str)
	       (equal
		  (text-properties-at 1)
		  props)))))
   (nil
      (progn
	 (emt:doc "Param: An overlay spec.")
	 (emt:doc "Afterwards: There's an overlay encompassing just the data that was
given inside the overlay spec.")
	 (with-temp-buffer
	    (loformat:insert
	       '("123"
		   (overlay nil "45" "67")
		   "890"))
	    (overlay-recenter 1)
	    (let*
	       ((overlays
		   (cdr
		      (overlay-lists)))
		  (overlay-data
		     (mapcar
			#'(lambda
			     (ov)
			     (list
				(overlay-start ov)
				(overlay-end ov)))
			overlays)))
	       (emt:assert
		  (equal overlay-data
		     '((4 8)))))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Point is not at beginning of line.")
	 (emt:doc "Operation: (nl-if-none)")
	 (emt:doc "Behavior: Insert a new line.")
	 (emtb:with-buf
	    (:string "abc!" :point-replaces "!")
	    (loformat:insert
	       '(nl-if-none))
	    (emt:assert
	       (equal "abc\n"
		  (buffer-string)))
	    t)))
   (nil
      (progn
	 (emt:doc "Situation: Point is at beginning of line.")
	 (emt:doc "Operation: (nl-if-none)")
	 (emt:doc "Behavior: Insert nothing.")
	 (emtb:with-buf
	    (:string "abc\n!" :point-replaces "!")
	    (loformat:insert
	       '(nl-if-none))
	    (emt:assert
	       (equal "abc\n"
		  (buffer-string)))
	    t))))

;;;_ , loformat:insert:indent
(emt:deftest-3
   ((of 'loformat:insert:indent))
   (nil
      (emtb:with-buf
	 ()
	 (loformat:insert
	    '(indent 4 "a\nb\nc\n"))
	 (emt:assert
	    (equal "    a\n    b\n    c\n"
	       (buffer-string))))))

;;;_. Footers
;;;_ , Provides

(provide 'viewers/loformat/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/loformat/rtest.el ends here
