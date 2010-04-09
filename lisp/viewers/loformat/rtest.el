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

;;;_. Body
;;;_ , emt:plain-viewer:insert

(rtest:deftest emt:plain-viewer:insert

   (  "Param: A bare string
Response: Inserts that string."
      (with-temp-buffer
	 (emt:plain-viewer:insert "a")
	 (equal "a" (buffer-string))))
   
   (  "Param: A list of strings
Response: Inserts those strings in order."
      (with-temp-buffer
	 (emt:plain-viewer:insert '("a" "b" "c"))
	 (equal "abc" (buffer-string))))

   (  "Param: A tree of strings
Response: Inserts those strings in order."
      (with-temp-buffer
	 (emt:plain-viewer:insert '("a" ("b") "c"))
	 (equal "abc" (buffer-string))))
      
   (  "Param: A string with properties
Response: The inserted text has the same properties."
      (with-temp-buffer
	 (let
	    ((str "abc")
	       (props '(face bold)))
	    (set-text-properties 0 3 props str)
	    (emt:plain-viewer:insert str)
	    (equal 
	       (text-properties-at 1)
	       props))))
   ;;Overlays

   (  "Param: An overlay spec.
Afterwards: There's an overlay encompassing just the data that was
given inside the overlay spec."
      (with-temp-buffer
	 (emt:plain-viewer:insert
	    '("123"
		;;An overlay, no properties
		(overlay () "45" "67")
		"890"))
	 (overlay-recenter 1)
	 (let*
	    ;;Get all the overlays after center, which is all of them.
	    ((overlays (cdr (overlay-lists)))
	       ;;Convert them into comparable data
	       (overlay-data
		  (mapcar
		     #'(lambda (ov)
			  (list
			     (overlay-start ov)
			     (overlay-end ov)))
		     overlays)))
	    
	    ;;Compare to expected value
	    (assert
	       (equal
		  overlay-data
		  '((4 8)))
	       t))
	 t))
   
   ;;nl-if-none

   (  "Situation: Point is not at beginning of line.
Operation: (nl-if-none)
Behavior: Insert a new line."
      (with-buffer-containing-object 
	 (:string "abc!" :point-replaces "!")
	 (emt:plain-viewer:insert '(nl-if-none))
	 (assert
	    (equal "abc\n" (buffer-string))
	    t)
	 
	 t))
   
   (  "Situation: Point is at beginning of line.
Operation: (nl-if-none)
Behavior: Insert nothing."
      (with-buffer-containing-object 
	 (:string "abc\n!" :point-replaces "!")
	 (emt:plain-viewer:insert '(nl-if-none))
	 (assert
	    (equal "abc\n" (buffer-string))
	    t)
	 
	 t))


   ;;Indent region - not yet

   )

;;;_. Footers
;;;_ , Provides

(provide 'viewers/loformat/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/loformat/rtest.el ends here
