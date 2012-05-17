;;;_ emtest/explorers/foreign/tests.el --- Tests for emtest/explorers/foreign

;;;_. Headers
;;;_ , License
;; Copyright (C) 2012  Tom Breton (Tehom)

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

(require 'emtest/explorers/foreign)
(require 'emtest/testhelp/mocks/filebuf)

;;;_. Body
;;;_ , Insulation
(defconst emtest/explorers/foreign:th:surrounders 
   '()
   "The normal surrounders for emtest/explorers/foreign tests" )
;;;_ , Examples
(defconst emt:xp:foreign:thd:examples
   (emtg:define+
      ((project emtest)(library foreign))
      (transparent-tags () (type))
      (group ((name 0))
	 (item ((type string)) "1:a")
	 (item ((type response)) "a"))))

;;;_ , Tests
(emt:deftest-3 emt:xp:foreign-read-buffer-csexp
   (nil
      (emt:assert
	 (equal (emt:xp:foreign-read-buffer-csexp "1:a") "a")))
   (nil
      (emt:assert
	 (equal (emt:xp:foreign-read-buffer-csexp "1:abc") "a")))
   (nil
      (emt:assert
	 (equal (emt:xp:foreign-read-buffer-csexp "2:abc") "ab")))
   (nil
      (emt:assert
	 (equal (emt:xp:foreign-read-buffer-csexp "(2:ab1:c)") '("ab" "c"))))
   (nil
      (emt:assert
	 (equal (emt:xp:foreign-read-buffer-csexp "(  )") '())))

   (nil
      (emt:assert
	 (equal (emt:xp:foreign-read-buffer-csexp "( k )") '()))))


;; Assumes emt:xp:foreign-ctor-alist is constant.
(emt:deftest-3
   ((of 'emt:xp:foreign-stringtree->object))
   (nil
      (emt:assert
	 (equal (emt:xp:foreign-stringtree->object '("list" "a" "b")) 
	    '("a" "b"))))
   (nil
      (emt:assert
	 (equal (emt:xp:foreign-stringtree->object '("integer" "57"))
	    57)))

   ;; Would test keywise '("suite" ("contents" "a") ("grade" "b"))
   ;; See 'emtest/testhelp/structs
   ;; And recursively, '("suite" ("contents" ("list" "a")) ("grade" "b"))
   )

(emt:deftest-3
   ((of 'emt:xp:foreign:object->stringtree))
   (nil
      (emt:assert
	 (equal (emt:xp:foreign:object->stringtree '("a" "b"))
	     '("list" "a" "b")))))

(emt:deftest-3
   ((of 'emt:xp:foreign:struct-stringtreer))
   (nil
      ;; Would like a test on the constant classes too.  

      (emt:assert
	 (equal
	    (emt:xp:foreign:struct-stringtreer
	       'emt:testral:suite
	       (emt:testral:make-suite
		  :contents '()
		  :grade 'ok))
	    '(("contents" ("list")) ("grade" ("symbol" "ok"))))))
   (nil
      (let
	 ((*how-to-prefix* '(prefix-el-1 prefix-el-2)))
	 (emt:assert
	    (equal
	       (emt:xp:foreign:stringtree->object
		  '("how-to-run" ("symbol" "a") ("symbol" "b")))
	       (emt:run:->how '(prefix-el-1 prefix-el-2 a b))))))
   )

(emt:deftest-3
   ((of 'emt:xp:foreign:object->stringtree))
   (nil
      (emt:assert
	 (equal
	    (emt:xp:foreign:object->stringtree 
	       (emt:testral:make-suite
		  :contents '()
		  :grade 'ok))
	    ;; NB, this was preliminary and not right
	    '("suite" 
		("contents" ("list")) 
		("grade" ("symbol" "ok"))))))
   (nil
      (emt:assert
	 (equal
	    (emt:xp:foreign:object->stringtree (emt:run:->how '(a b)))
	    '("how-to-run" ("symbol" "a") ("symbol" "b")))))

   )

(emt:deftest-3
   ((of 'emt:xp:foreign:stringtree-to-stringlist))
   (nil
      (emt:assert
	 (equal
	    (emt:xp:foreign:stringtree-to-stringlist 
	       "a")
	    '("1"":""a"))))
   (nil
      (emt:assert
	 (equal
	    (emt:xp:foreign:stringtree-to-stringlist 
	       '("a"))
	    '("(""1"":""a"")"))))
   )

(emt:deftest-3
   ((of 'emt:xp:foreign:stringtree-to-csexp))
   (nil
      (emt:assert
	 (equal
	    (emt:xp:foreign:stringtree-to-csexp
	       '(("a""b")"c"("d")))
	    "((1:a1:b)1:c(1:d))"))))

(emt:deftest-3
   ((of 'emt:xp:foreign:encode-TESTRAL))
   (nil
      (emt:assert
	 (equal
	    (emt:xp:foreign:encode-TESTRAL (emt:run:->how '(a b)))
	    "(10:how-to-run(6:symbol1:a)(6:symbol1:b))"))))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/explorers/foreign/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + no-byte-compile: t
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/explorers/foreign/tests.el ends here
