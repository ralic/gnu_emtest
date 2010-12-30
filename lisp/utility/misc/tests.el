;;;_ utility/misc/tests.el --- Tests for utility/misc

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
(require 'utility/misc)
(require 'emtest/main/define)
(require 'emtest/testhelp/misc)
(require 'emtest/testhelp/standard)

;;;_. Body
;;;_ , utim:constantp
(emt:deftest-3 utim:constantp
   
   (nil
      (progn
	 (emt:doc "Shows: Normal literals give non-nil")
	 (assert
	    (utim:constantp 12))))
       
   (nil
      (progn
	 (emt:doc "Shows: Normal literals give non-nil")
	 (assert
	    (utim:constantp "abc"))))

   (nil
      (progn
	 (emt:doc "Shows: Self-evaluating symbols are considered literals here")
	 (assert
	    (utim:constantp t))))
	
   (nil
      (progn
	 (emt:doc "Shows: Non self-evaluating symbols give nil")
	 (assert
	    (not
	       (let (foo)
		  (utim:constantp 'foo))))))

   (nil
      (progn
	 (emt:doc "Shows: Unbound symbols give nil")
	 (assert
	    (not
	       (let (foo)
		  (makunbound 'foo)
		  (utim:constantp 'foo))))))
       
   (nil
      (progn
	 (emt:doc "Shows: `form', defined within the call itself, doesn't falsely
appear constant.") 
	 (assert
	    (not
	       (utim:constantp 'form)))))

   (nil
      (progn
	 (emt:doc "Shows: Unquoted lists give nil.  NB, the single quote is stripped
before utim:constantp sees it")
	 (assert
	    (not
	       (utim:constantp '(2))))))
       
   (nil
      (progn
	 (emt:doc "Shows: Vectors with all constants give t")
	 (assert
	    (utim:constantp [1 2 3])
	    )))
       
   (nil
      (progn
	 (emt:doc "Shows: Vectors with some variables give nil")
	 (assert
	    (not
	       (let (foo)
		  (utim:constantp [1 foo foo 4])))))))

;;;_  . utim:form-by-option

(emt:deftest-3 utim:form-by-option
   (nil
      (progn
	 (emt:doc "Expands form according to data in options")
	 (rtest:retvalue->status
	    (equal
	       (utim:form-by-option
		  '((x bar baz))
		  'x
		  #'(lambda
		       (data)
		       `(foo ,(second data)
			   ,(third data))))
	       '(foo bar baz)))))
   (nil
      (progn
	 (emt:doc "With a FORM-ELSE given, expands it when option isn't given.")
	 (rtest:retvalue->status
	    (equal
	       (utim:form-by-option nil 'x nil
		  #'(lambda
		       (x)
		       '(foo)))
	       '(foo))))))

;;;_. Footers
;;;_ , Provides

(provide 'utility/misc/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; utility/misc/tests.el ends here
