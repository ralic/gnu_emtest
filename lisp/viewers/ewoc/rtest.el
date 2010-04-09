;;;_ viewers/ewoc/rtest.el --- Testing for ewoc.el

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: maint, lisp

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

;; These tests require my package rtest, and in particular mockbuf.
;; However, rtest is in flux right now (and the new version will
;; depend on ewoc), so I'm afraid it's not easy to actually run these
;; tests right now.

;;The last test relies on TTN's colorcomp application.

;;;_ , Requires

(when (not (fboundp 'rtest:deftest))
    (defmacro rtest:deftest (&rest dummy))
    (defmacro rtest:if-avail (&rest dummy)))
(require 'viewers/ewoc/testhelp)
(require 'tester/testhelp/mocks/filebuf)

;;;_. Body

;;;_ , Tests
(rtest:deftest ewoc
   (  "Situation: newline separator, empty header & footer.
Demonstrates: Correct behavior of various parts of ewoc interface."
      (with-temp-buffer
	 (let
	    ((ewoc 
		(ewoc-create
		   #'insert nil nil))
	       (expected-contents
		  "
@
a
b
c
d

"
		  ))
	    
	    
	    (ewoc-enter-last ewoc "@")
	    (ewoc-enter-last ewoc "a")
	    (ewoc-enter-last ewoc "b")
	    (ewoc-enter-last ewoc "c")
	    (ewoc-enter-last ewoc "d")

	    ;;Validate
	    (assert
	       (emtb:buf-contents-matches
		  :string expected-contents))
      
	    ;;Check that nodes' data is as expected
	    (let* 
	       ((node (ewoc-nth ewoc 0)))
	       (assert
		  (equal (ewoc-data node) "@")
		  t))

	    (let* 
	       ((node (ewoc-nth ewoc 4)))
	       (assert
		  (equal (ewoc-data node) "d")
		  t))


	    ;;Test ewoc-location.  Since there's one element to a
	    ;;line, we need only test what line it's on and that it's
	    ;;at the beginning of the line.  `count-lines' needs +1 to
	    ;;account for the initial separator.  `goto-start-of-el'
	    ;;accounts for the separator and the fact that the top
	    ;;line = 1.
	    (labels
	       ((bolp/1 (pos)
		   (save-excursion
		      (goto-char pos)
		      (bolp)))
		  (assert-pos-matches-num (pos el-num)
		     (assert
			(bolp/1 pos)
			t)
		     (assert
			(= (1- (count-lines 1 pos)) el-num)
			t))
		  (assert-node-matches-num (node el-num)
		     (assert-pos-matches-num (ewoc-location node)
			el-num))
		  (goto-start-of-el (el-num) 
		     (goto-line (+ 2 el-num))))
	       
	       ;;ewoc-location
	       ;;First element
	       (let* 
		  ((node (ewoc-nth ewoc 0)))
		  (assert-node-matches-num node 0))
	    
	       ;;Last element
	       (let* 
		  ((node (ewoc-nth ewoc 4)))
		  (assert-node-matches-num node 4))
	    


	       ;;Test ewoc-locate

	       ;;Point at the beginning of an element, returns that
	       ;;element.  Because of the initial separator, `goto-line'
	       ;;needs to go to 1 + element number.
	       (goto-start-of-el 2)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "b")
		     t))
	    

	       ;;First node
	       (goto-start-of-el 0)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "@")
		     t))

	       ;;Last node
	       (goto-start-of-el 4)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "d")
		     t))

	       ;;After end
	       (goto-char (point-max))
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "d")
		     t))

	       ;;Before beginning
	       (goto-char 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "@")
		     t))


	       ;;Test ewoc-goto-prev
	       ;;4 - 1 to 3 
	       (goto-start-of-el 4)
	       (ewoc-goto-prev ewoc 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "c")
		     t))

	       ;;4 - 2 to 2
	       (goto-start-of-el 4)
	       (ewoc-goto-prev ewoc 2)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "b")
		     t))

	       ;;0 - 1 still at 0
	       (goto-start-of-el 0)
	       (ewoc-goto-prev ewoc 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "@")
		     t))
	    
	       ;;Test ewoc-goto-next
	       ;;2 + 1 to 3
	       (goto-start-of-el 2)
	       (ewoc-goto-next ewoc 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "c")
		     t))

	       ;;2 + 2 to 4
	       (goto-start-of-el 2)
	       (ewoc-goto-next ewoc 2)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "d")
		     t))

	       ;;4 + 1 still at 4
	       (goto-start-of-el 4)
	       (ewoc-goto-next ewoc 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "d")
		     t))
	       
	       ;;Test ewoc--refresh-node.
	       (emt:gives-error
		  (ewoc--refresh-node #'insert nil ewoc))
	       
	       ;;23.1 fails these tests.
	       (let* 
		  ((node (ewoc-nth ewoc 0)))
		  (ewoc--refresh-node #'insert node ewoc)
		  (assert
		     (emtb:buf-contents-matches
			:string expected-contents)))
	       (let* 
		  ((node (ewoc-nth ewoc 1)))
		  (ewoc--refresh-node #'insert node ewoc)
		  (assert
		     (emtb:buf-contents-matches
			:string expected-contents)))

	       (let* 
		  ((node (ewoc-nth ewoc 4)))
		  (ewoc--refresh-node #'insert node ewoc)
		  (assert
		     (emtb:buf-contents-matches
			:string expected-contents)))

	       ;;Test ewoc-refresh
	       (ewoc-refresh ewoc)
	       (assert
		  (emtb:buf-contents-matches
		     :string expected-contents
		     ))))
	 t))


   (  "Situation: no separator, empty header & footer.
Demonstrates: Correct behavior of various parts of ewoc interface."
      (with-temp-buffer
	 (let
	    ((ewoc 
		(ewoc-create #'insert nil nil ""))
	       (expected-contents
		  "@abcd"))
	    
	    (ewoc-enter-last ewoc "@")
	    (ewoc-enter-last ewoc "a")
	    (ewoc-enter-last ewoc "b")
	    (ewoc-enter-last ewoc "c")
	    (ewoc-enter-last ewoc "d")
	    
	    ;;Validate
	    (assert
	       (emtb:buf-contents-matches
		  :string expected-contents))

	    ;;Check that nodes' data is as expected
	    (let* 
	       ((node (ewoc-nth ewoc 0)))
	       (assert
		  (equal (ewoc-data node) "@")
		  t))

	    (let* 
	       ((node (ewoc-nth ewoc 4)))
	       (assert
		  (equal (ewoc-data node) "d")
		  t))


	    (labels
	       (
		  (assert-pos-matches-num (pos el-num)
		     (assert
			(= (1- pos) el-num)
			t))
		  (assert-node-matches-num (node el-num)
		     (assert-pos-matches-num (ewoc-location node)
			el-num))
		  (goto-start-of-el (el-num) 
		     (goto-char (1+ el-num))))
	       
	       ;;ewoc-location
	       ;;First element
	       (let* 
		  ((node (ewoc-nth ewoc 0)))
		  (assert-node-matches-num node 0))
	    
	       ;;Last element
	       (let* 
		  ((node (ewoc-nth ewoc 4)))
		  (assert-node-matches-num node 4))
	    


	       ;;Test ewoc-locate

	       ;;Point at the beginning of an element, returns that
	       ;;element.  Because of the initial separator, `goto-line'
	       ;;needs to go to 1 + element number.
	       (goto-start-of-el 2)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "b")
		     t))
	    

	       ;;First node
	       (goto-start-of-el 0)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "@")
		     t))

	       ;;Last node
	       (goto-start-of-el 4)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "d")
		     t))

	       ;;After end
	       (goto-char (point-max))
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "d")
		     t))

	       ;;Before beginning
	       (goto-char 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "@")
		     t))


	       ;;Test ewoc-goto-prev
	       ;;4 - 1 to 3 
	       (goto-start-of-el 4)
	       (ewoc-goto-prev ewoc 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "c")
		     t))

	       ;;4 - 2 to 2
	       (goto-start-of-el 4)
	       (ewoc-goto-prev ewoc 2)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "b")
		     t))

	       ;;0 - 1 still at 0
	       (goto-start-of-el 0)
	       (ewoc-goto-prev ewoc 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "@")
		     t))
	    
	       ;;Test ewoc-goto-next
	       ;;2 + 1 to 3
	       (goto-start-of-el 2)
	       (ewoc-goto-next ewoc 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "c")
		     t))

	       ;;2 + 2 to 4
	       (goto-start-of-el 2)
	       (ewoc-goto-next ewoc 2)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "d")
		     t))

	       ;;4 + 1 still at 4
	       (goto-start-of-el 4)
	       (ewoc-goto-next ewoc 1)
	       (let* 
		  ((node (ewoc-locate ewoc)))
		  (assert
		     (equal (ewoc-data node) "d")
		     t))
	       
	       ;;Test ewoc--refresh-node.
	       (emt:gives-error
		  (ewoc--refresh-node #'insert nil ewoc))
	       
	       (let* 
		  ((node (ewoc-nth ewoc 0)))
		  (ewoc--refresh-node #'insert node ewoc)
		  (assert
		     (emtb:buf-contents-matches
			:string expected-contents)))
	       (let* 
		  ((node (ewoc-nth ewoc 1)))
		  (ewoc--refresh-node #'insert node ewoc)
		  (assert
		     (emtb:buf-contents-matches
			:string expected-contents)))

	       (let* 
		  ((node (ewoc-nth ewoc 4)))
		  (ewoc--refresh-node #'insert node ewoc)
		  (assert
		     (emtb:buf-contents-matches
			:string expected-contents)))

	       ;;Test ewoc-refresh
	       (ewoc-refresh ewoc)
	       (assert
		  (emtb:buf-contents-matches
		     :string expected-contents
		     ))))
	 t))

   (  "Param: non-nil, non-string separator argument.
Response:  Behaves as the empty separator."
      (with-temp-buffer
	 (let
	    ((ewoc 
		(ewoc-create #'insert nil nil t))
	       (expected-contents
		  "@abcd"))
	    
	    (ewoc-enter-last ewoc "@")
	    (ewoc-enter-last ewoc "a")
	    (ewoc-enter-last ewoc "b")
	    (ewoc-enter-last ewoc "c")
	    (ewoc-enter-last ewoc "d")
	    
	    (assert
	       (emtb:buf-contents-matches
		  :string expected-contents)))

	 t))

      (  "Situation: no separator, empty header & footer, some empty elements.
Demonstrates: Correct behavior of refresh and map."
      (with-temp-buffer
	 (let
	    ((ewoc 
		(ewoc-create
		   #'insert nil nil ""))
	       (expected-contents
		  "ace"))
	    
	    (ewoc-enter-last ewoc "a")
	    (ewoc-enter-last ewoc "")
	    (ewoc-enter-last ewoc "c")
	    (ewoc-enter-last ewoc "")
	    (ewoc-enter-last ewoc "e")
	    
	    ;;Validate
	    (assert
	       (emtb:buf-contents-matches
		  :string expected-contents))

	    ;;Refresh

	    (ewoc-refresh ewoc)

	    (assert
	       (emtb:buf-contents-matches
		  :string expected-contents))

	    ;;Map

	    ;;All elements non-nil, ie reprinted.
	    (let
	       ((l
		   (ewoc-map #'identity ewoc)))
	       ;;`ewoc-map' doesn't actually collect a list of return
	       ;;values, so we can't test that.
	       )
	    
	    (assert
	       (emtb:buf-contents-matches
		  :string expected-contents))
	   
	    ;;One element non-nil, ie reprinted.
	    (ewoc-map #'string-equal
	       ewoc
	       "c")
	    (assert
	       (emtb:buf-contents-matches
		  :string expected-contents)))
	 
	 t))

   (  "Situation: no separator, empty header & footer, some empty elements.
Demonstrates: Correct behavior of filter."
      (with-temp-buffer
	 (let
	    ((ewoc 
		(ewoc-create
		   #'insert nil nil "")))
	    
	    (ewoc-enter-last ewoc "a")
	    (ewoc-enter-last ewoc "b")
	    (ewoc-enter-last ewoc "")
	    (ewoc-enter-last ewoc "a")
	    (ewoc-enter-last ewoc "c")
	    (ewoc-enter-last ewoc "a")
	    (ewoc-enter-last ewoc "")
	    (ewoc-enter-last ewoc "d")
	    
	    ;;Validate
	    (assert
	       (emtb:buf-contents-matches
		  :string "abacad"))

	    (ewoc-filter ewoc
	       #'(lambda (data str)
		    (not (string-equal data str))
		    )
	       "a")
	    
	    (assert
	       (emtb:buf-contents-matches
		  :string "bcd"))

	    ;;Behavior of `ewoc-do'
	    (let
	       ((count 0))
	       (ewoc-do (i ewoc)
		  (incf count))
	       (assert (= count 5) t))
	    
	    (let
	       ((count 0))
	       (ewoc-do (i ewoc)
		  (when (not (string-equal i ""))
		     (incf count)))
	       (assert (= count 3) t)))
	 
	 t))
   

   ;;This is slightly nasty because of variation in the way emacsen
   ;;represent and print keymaps.
   (  "Demonstration: The colorcomp application."
      (let*
	 ((color "green")
	    (buf-name
	       ;;Hack: Since colorcomp doesn't tell us what buffer it
	       ;;made, first generate the buffer name the same way it
	       ;;does.  Later we'll find the buffer with this name.
	       (generate-new-buffer-name 
		  (format "originally: %s" color))))
	 ;;If `colorcomp' is not available, this test is dormant (not
	 ;;supported yet)
	 (require 'colorcomp)
	 
	 (colorcomp color)
	 (with-current-buffer (get-buffer buf-name)
	    (assert
	       (emtb:buf-contents-matches
		  :dir "examples/"
		  :file "colorcomp.1.txt"))

	    ;;Put it thru its paces and check that buffer text is
	    ;;correct.
	    (dotimes (i 32) (colorcomp-R-more))
	    (assert
	       (emtb:buf-contents-matches
		  :dir "examples/"
		  :file "colorcomp.2.txt"))

	    (dotimes (i 64) (colorcomp-B-more))
	    (assert
	       (emtb:buf-contents-matches
		  :dir "examples/"
		  :file "colorcomp.3.txt"))

	    (dotimes (i 15) (colorcomp-G-less))
	    (assert
	       (emtb:buf-contents-matches
		  :dir "examples/"
		  :file "colorcomp.4.txt"))

	    (colorcomp-copy-as-kill-and-exit))
	 t))
   
   )


;;;_. Footers
;;;_ , Provides

(provide 'viewers/ewoc/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/ewoc/rtest.el ends here
