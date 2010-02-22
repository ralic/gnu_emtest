;;;_ tester/emt-funcall/tests.el --- Tests for emt-funcall

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

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))

;;;_. Body

;;;_  . Tests
(put 'emt:trace:protect 'rtest:test-thru
   'emt:should-f)
;;;_  . Tests
(rtest:deftest emt:trace:add-to-stored-diag

   ;;But this test won't work because tester will bind
   ;;`emt:trace:add-to-stored-diag' to a local value.  Acquiring the
   ;;global value would require more code.
   '
   (  "Situation: There are more than 6 reports on `emt:trace:stored-diag' 
Response: `emt:trace:stored-diag' is trimmed to 6 reports."
      (let ((emt:trace:stored-diag '(a b c d e f g)))
	 (emt:trace:add-to-stored-diag 'h)
	 (= (length emt:trace:stored-diag) (+ 5 1)))))
;;;_  . Tests
(put 'emt:funcall-handle-persistence 'rtest:test-thru
   'emt:funcall)
;;;_  . Tests
(put 'emt:funcall-x 'rtest:test-thru
   'emt:funcall)
;;;_  . Tests
(rtest:deftest emt:funcall
   ;;"should" etc won't work inside `emt:funcall:th' but will
   ;;work outside it.

   (  "Situation: Comparison fails.
Report-control is to report everything.
Response: Store a report and return nil"


      (destructuring-bind (retval diag &key status &allow-other-keys)
	 (emt:funcall:th:3  #'equal 1 2)
	 (assert
	    (eq retval nil) t)
	 (assert
	    (eq status nil) t)
	 (assert
	    (equal 
	       (emt:result:diag:call-call-sexp diag)
	       '(equal 1 2))
	    t)
	 t))
   


   (  "Situation: Comparison succeeds.
Report-control is to report everything.
Response: Store a report and return t"


	 (destructuring-bind (retval diag &key status &allow-other-keys)
	    (emt:funcall:th:3 #'member 1 '(1))
	    (assert
	       (equal retval '(1)) t)
	    (assert
	       (eq status t) t)
	    t))

   (  "Situation: Comparison errors.
Report-control is to report everything.
Response: Store a report (and throw an error)"


      (destructuring-bind (retval diag &key status &allow-other-keys)
	 (emt:funcall:th:3 #'error "A dummy error")
	 (assert
	    (eq status 'error) t)
	 t))


   (  "Situation: Comparison errors.
Report-control is to report everything.
Response: (Store a report and) throw an error"

      (rtest:gives-error
	 (emt:trace:protect
	    (emt:funcall #'error "A dummy error")))))





;;;_. Footers
;;;_ , Provides

(provide 'tester/emt-funcall/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; tester/emt-funcall/tests.el ends here
