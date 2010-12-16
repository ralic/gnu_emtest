;;;_ emtest/testhelp/mocks/filebuf/tests.el --- Tests for filebuf

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

;;Only a few examples use emtg.  Most or all should.

;;;_ , Requires

(require 'emtest/runner/define)
(require 'emtest/testhelp/mocks/filebuf)
(require 'emtest/testhelp/mocks/filebuf/testhelp)
(require 'emtest/testhelp/tagnames)
(require 'emtest/testhelp/standard)
(require 'emtest/editing/insert)

;;;_. Body
;;;_   , emtb:with-buf-f
(put 'emtb:with-buf-f 'emt:test-thru 'emtb:with-buf)


;;;_    . emtb:with-buf

(emt:deftest-3 emtb:with-buf
   (nil
      (progn
	 (emt:doc "Param: :string STRING is given
Reaction: Fill the buffer with exactly STRING")
	 (emtb:with-buf
	    (:string "abc")
	    (equal
	       (buffer-string)
	       "abc"))))
   (nil
      (progn
	 (emt:doc "Param: :sexp OBJECT is given.
Reaction: Fill the buffer with exactly a printed representation of
OBJECT")
	 (equal
	    (emtb:with-buf
	       (:sexp
		  '(1 5))
	       (emtb:buffer-object))
	    '(1 5))))
   (nil
      (progn
	 (emt:doc "Param: :point-replaces STR is given.
Reaction: The first occurence of STR is deleted ")
	 (emtb:with-buf
	    (:string "abcdef" :point-replaces "cd")
	    (equal
	       (buffer-string)
	       "abef"))))
   (nil
      (progn
	 (emt:doc "Param: :point-replaces STR is given.
Reaction: Point is now where STR was ")
	 (emtb:with-buf
	    (:string "abcdef" :point-replaces "cd")
	    (looking-at "ef$"))))
   (nil
      (progn
	 (emt:doc "Shows: :point-replaces can cause parts of an object to be unprinted.")
	 (equal
	    (emtb:with-buf
	       (:sexp
		  '(ab cd ef)
		  :point-replaces "cd")
	       (emtb:buffer-object))
	    '(ab ef))))
   (nil
      (progn
	 (emt:doc "Restores the original buffer even in case of error.")
	 (let
	    ((win
		(current-window-configuration)))
	    (ignore-errors
	       (emtb:with-buf
		  (:string "abc")
		  (error "A deliberate error")))
	    (equal win
	       (current-window-configuration)))))

   (nil
      (progn
	 (emt:doc "Exercise the `visited-name' feature.")
	 (emtg:with emtb:thd:examples
	    ((project emtest)
	       (library mockbuf))
	    (let
	       ((master-file
		   (emtg
		      (role master)
		      (type filename)))
		  (slave-file
		     (emtg
			(role slave)
			(type filename))))
	       (when
		  (file-exists-p slave-file)
		  (delete-file slave-file))
	       (emtb:with-buf
		  (:file master-file :visited-name slave-file)
		  (save-buffer 0))
	       (emtb:files-match master-file slave-file)))))

   (nil
      (progn
	 (emt:doc "Situation: The slave file already exists.")
	 (emt:doc "Response: Works the same.")
	 (emtg:with emtb:thd:examples
	    ((project emtest)
	       (library mockbuf))
	    (let
	       ((master-file
		   (emtg
		      (role master)
		      (type filename)))
		  (slave-file
		     (emtg
			(role slave)
			(type filename))))
	       (unless
		  (file-exists-p slave-file)
		  (with-temp-buffer
		     (insert "Some unmatched text")
		     (write-file slave-file nil)))
	       (emtb:with-buf
		  (:file master-file :visited-name slave-file)
		  (save-buffer 0))
	       (emtb:files-match master-file slave-file)))))
   (nil
      (progn
	 (emt:doc "Param: the symbol `tmp' is given as `:visited-name'.")
	 (emt:doc "Behavior: Creates a temporary file.")
	 (emtg:with emtb:thd:examples
	    ((project emtest)
	       (library mockbuf))
	    (let
	       (slave-file
		  (master-file
		     (emtg
			(role master)
			(type filename))))
	       (emtb:with-buf
		  (:file master-file :visited-name 'tmp)
		  (setq slave-file
		     (buffer-file-name))
		  (save-buffer 0))
	       (emtb:files-match master-file slave-file)))))
   (nil
      (progn
	 (emt:doc "Param: the symbol `tmp' is given as `:visited-name'.
Neither `dir' nor `file' is given.")
	 (emt:doc "Behavior: Creates a temporary file with those contents.")
	 (emtg:with emtb:thd:examples
	    ((project emtest)
	       (library mockbuf))
	    (let
	       (slave-file
		  (master-file
		     (emtg
			(role master)
			(type filename))))
	       (emtb:with-buf
		  (:string "abc" :visited-name 'tmp)
		  (setq slave-file
		     (buffer-file-name))
		  (save-buffer 0))
	       (emt:assert
		  (string= "abc"
		     (emtb:file-contents-absname slave-file)))))))
   (nil
      (progn
	 (emt:doc "Shows: `sequence' works.
Param: sequence with two strings")
	 (emt:doc "Response: Contents of buffer are as expected.")
	 (emtb:with-buf
	    (:sequence
	       ((string "abc")
		  (string "def")))
	    (equal
	       (buffer-string)
	       "abcdef")))))


;;;_   , emtb:with-file-f

(emt:deftest-3 emtb:with-file-f
   (nil
      (progn
	 (emt:doc "Proves: Body form is run - it changes the value of `x'.")
	 (let
	    ((str "abc def ghi")
	       x)
	    (emtb:with-file-f
	       (:string str)
	       filename
	       (setq x 12))
	    (emt:assert
	       (equal x 12))
	    t)))
   (nil
      (progn
	 (emt:doc "Proves: The filename is absolute.")
	 (let
	    ((str "abc def ghi"))
	    (emtb:with-file-f
	       (:string str)
	       filename
	       (emt:assert
		  (file-name-absolute-p filename)))
	    t)))
   (nil
      (progn
	 (emt:doc "Proves: Th file contents match the expected contents.")
	 (let
	    ((str "abc def ghi")
	       x)
	    (emtb:with-file-f
	       (:string str)
	       filename
	       (emt:assert
		  (string=
		     (emtb:file-contents-absname filename)
		     str)))
	    t)))
   (nil
      (progn
	 (emt:doc "Proves: `emtb:with-file-f' returns the result of running
BODY.")
	 (let
	    ((str "abc def ghi"))
	    (emt:assert
	       (equal
		  (emtb:with-file-f
		     (:string str)
		     filename 12)
		  12))
	    t)))
   (nil
      (progn
	 (emt:doc "Proves: After the body has finished running, the file is gone.")
	 (let
	    (filename-kept)
	    (emtb:with-file-f
	       (:string "abc def ghi")
	       filename
	       (setq filename-kept filename))
	    (emt:assert
	       (not
		  (file-exists-p filename-kept)))
	    t)))
   (nil
      (progn
	 (emt:doc "Param: The flag `:absent' is given and is non-nil.
No file is made.")
	 (emtb:with-file-f
	    (:absent t)
	    filename
	    (emt:assert
	       (not
		  (file-exists-p filename)))
	    t)))
   (nil
      (progn
	 (emt:doc "Param: The flag `:absent' is given and is non-nil.")
	 (emt:doc "Situation: That file is created by body.")
	 (emt:doc "Response: After body has run, file no longer exists.")
	 (let
	    (filename-kept)
	    (emtb:with-file-f
	       (:absent t)
	       filename
	       (setq filename-kept filename)
	       (with-temp-buffer
		  (insert "Some stuff")
		  (write-file filename)))
	    (emt:assert
	       (not
		  (file-exists-p filename-kept)))
	    t))))


;;;_    . emtb:cautious-insert-file
(put 'emtb:cautious-insert-file 'emt:test-thru 'emtb:with-buf)



;;;_    . emtb:cautious-load-file (OBSOLESCENT)
'
(put 'emtb:cautious-load-file 'emt:test-thru 'emtb:with-buf)



;;;_    . emtb:find-file-goto-text (OBSOLESCENT)

(emt:deftest-3 emtb:find-file-goto-text
   (nil
      (progn
	 (emt:doc "Situation: Filename is not absolute.")
	 (emt:doc "Response: Error.")
	 (emth:gives-error
	    (emtb:find-file-goto-text ".")))))


;;;_    . emtb:find-file-2 (OBSOLESCENT)

;;Tested thru emtb:make-form-find-file, but that's increasingly
;;less adequate.

;;Tested thru ade:deactivate-siblings, but that's obsolete.

;;;_   , emtb:buf-is-file
(emt:deftest-3 emtb:buf-is-file
   '(nil
       (progn
	  (emt:doc "Situation: WRITEME.")
	  (emt:doc "Response: WRITEME.")
	  (progn))))

;;;_   ; emtb:buf-is-file-at

(emt:deftest-3 emtb:buf-is-file-at
   (nil
      (progn
	 (emt:doc "Situation: Positions simply don't match.")
	 (emt:doc "Response: return nil.")
	 (not
	    (let
	       ((filename
		   (emtb:virtual-dir:base->filename "file1" emtb:check-file:thd:virtual-dir))
		  (str "position=mockbuf-1"))
	       (with-current-buffer
		  (find-file-noselect filename)
		  (goto-char 4)
		  (emtb:buf-is-file-at filename str))))))
   (nil
      (progn
	 (emt:doc "Situation: emtb:find-file-goto-text has just been called with
the same arguments. No motion argument is given.")
	 (emt:doc "Response: Returns non-nil.")
	 (let
	    ((filename
		(emtb:virtual-dir:base->filename "file1" emtb:check-file:thd:virtual-dir))
	       (str "position=mockbuf-1"))
	    (with-current-buffer
	       (emtb:find-file-goto-text filename str)
	       (emtb:buf-is-file-at filename str)))))
   (nil
      (progn
	 (emt:doc "Situation: MOTION moves to same place (BOB).")
	 (emt:doc "Response: return non-nil.")
	 (let
	    ((filename
		(emtb:virtual-dir:base->filename "file1" emtb:check-file:thd:virtual-dir))
	       (str "position=mockbuf-1"))
	    (with-current-buffer
	       (find-file-noselect filename)
	       (goto-char 4)
	       (emtb:buf-is-file-at filename str
		  #'(lambda nil
		       (goto-char
			  (point-min))))))))
   (nil
      (progn
	 (emt:doc "Situation: Original positions are in different places, but
MOTION finds a % that precedes both.")
	 (emt:doc "Response: return non-nil.")
	 (let
	    ((filename
		(emtb:virtual-dir:base->filename "file2" emtb:check-file:thd:virtual-dir)))
	    (with-current-buffer
	       (emtb:find-file-goto-text filename "position=mockbuf-1")
	       (emtb:buf-is-file-at filename "position=mockbuf-meta-1"
		  #'(lambda nil
		       (search-backward "%")))))))
   (nil
      (progn
	 (emt:doc "Situation: MOTION can't be executed.")
	 (emt:doc "Response: error.")
	 (let
	    ((filename
		(emtb:virtual-dir:base->filename "file1" emtb:check-file:thd:virtual-dir)))
	    (with-current-buffer
	       (find-file-noselect filename)
	       (not
		  (ignore-errors
		     (progn
			(emtb:buf-is-file-at filename str
			   '(1 2))
			t))))))))


;;;_   , emtb:file-contents-absname

(put 'emtb:file-contents-absname 'emt:test-thru 'emtb:buf-contents-matches)



;;;_   , emtb:buf-contents-matches

(emt:deftest-3 emtb:buf-contents-matches
   (nil
      (progn
	 (emt:doc "Param: `string' is given.
Action: Treat string as the expected value.")
	 (emtb:with-buf
	    (:string "abc def")
	    (emtb:buf-contents-matches :string "abc def"))))
   (nil
      (progn
	 (emt:doc "Param: `string' is given.
Action: Treat string as the expected value.")
	 (not
	    (emtb:with-buf
	       (:string "abc def")
	       (emtb:buf-contents-matches :string "Do not match
")))))
   (nil
      (progn
	 (emt:doc "Param: `string' and `regex-marks' are given.
Action: Treat string as containing bounded regexps, as for
emtb:string-matches.")
	 (emtb:with-buf
	    (:string "abc def")
	    (emtb:buf-contents-matches :string "abc [.*]" :regex-marks
	       '("[" "]")))))
   (nil
      (progn
	 (emt:doc "Behavior: Point is not moved.")
	 (emtb:with-buf
	    (:string "abc def")
	    (goto-char 3)
	    (emtb:buf-contents-matches :string "abc def")
	    (=
	       (point)
	       3))))
   (nil
      (progn
	 (emt:doc "Situation: Filename is not absolute")
	 (emt:doc "Response: Error.")
	 (let
	    ((default-directory emtb:buf-contents-matches:thd:dir))
	    (emth:gives-error
	       (emtb:buf-contents-matches :file "yes.txt")))))
   (nil
      (progn
	 (emt:doc "Situation: `dir' is given but expanded filename is not absolute,")
	 (emt:doc "Response: Error.")
	 (emth:gives-error
	    (emtb:buf-contents-matches :file "yes.txt" :dir "."))))
   (nil
      (progn
	 (emt:doc "Situation: File does not exist")
	 (emt:doc "Response: Error.")
	 (emth:gives-error
	    (emtb:buf-contents-matches :file "invalid.txt" :dir emtb:buf-contents-matches:thd:dir))))
   (nil
      (progn
	 (emt:doc "Param: `buf' is given.
Action: Compare file to contents of `buf'.")
	 (emtb:with-buf
	    (:string emtb:buf-contents-matches:thd:yes\.txt)
	    (let
	       ((buf
		   (current-buffer)))
	       (with-temp-buffer
		  (insert "This buffer would not match the file")
		  (emtb:buf-contents-matches :file "yes.txt" :dir emtb:buf-contents-matches:thd:dir :buf buf))))))
   (nil
      (progn
	 (emt:doc "Param: `buf' is not given.
Action: Compare file to contents of current buffer.")
	 (emtb:with-buf
	    (:string emtb:buf-contents-matches:thd:yes\.txt)
	    (emtb:buf-contents-matches :file "yes.txt" :dir emtb:buf-contents-matches:thd:dir))))
   (nil
      (progn
	 (emt:doc "Situation: Buffer contents and file contents do not match.")
	 (emt:doc "Response: Comparison fails.")
	 (not
	    (emtb:with-buf
	       (:string emtb:buf-contents-matches:thd:yes\.txt)
	       (emtb:buf-contents-matches :file "no.txt" :dir emtb:buf-contents-matches:thd:dir)))))
   (nil
      (progn
	 (emt:doc "Param: `regex-marks' is given.
Action: Treat file as containing bounded regexps, as for
emtb:string-matches.")
	 (emtb:with-buf
	    (:string emtb:buf-contents-matches:thd:yes\.txt)
	    (emtb:buf-contents-matches :file "regexp-yes.txt" :dir emtb:buf-contents-matches:thd:dir :regex-marks
	       '("[" "]")))))
   (nil
      (progn
	 (emt:doc "Param: `sexp' is given.
Action: Succeed just if the buffer contains the sexp representation
of the object.")
	 (emtb:with-buf
	    (:string "(a b c)")
	    (emtb:buf-contents-matches :sexp
	       '(a b c)))))
   (nil
      (progn
	 (emt:doc "Param: `sexp' is given.")
	 (emt:doc "Behavior: Point is not moved")
	 (emtb:with-buf
	    (:string "(a b c)")
	    (goto-char 3)
	    (emtb:buf-contents-matches :sexp
	       '(a b c))
	    (=
	       (point)
	       3))))
   (nil
      (progn
	 (emt:doc "Situation: Validator is given. It's a file that matches the
regexp pattern.")
	 (emt:doc "Response: Proceed normally.")
	 (emtb:with-buf
	    (:string emtb:buf-contents-matches:thd:yes\.txt)
	    (emtb:buf-contents-matches :file "regexp-yes.txt" :dir emtb:buf-contents-matches:thd:dir :regex-marks
	       '("[" "]")
	       :validate-re
	       '((:file "yes.txt"))))))
   (nil
      (progn
	 (emt:doc "Situation: Validator is given. It's a file that does not match
the regexp pattern.")
	 (emt:doc "Response: Error.")
	 (emtb:with-buf
	    (:string emtb:buf-contents-matches:thd:yes\.txt)
	    (emth:gives-error
	       (emtb:buf-contents-matches :file "regexp-yes.txt" :dir emtb:buf-contents-matches:thd:dir :regex-marks
		  '("[" "]")
		  :validate-re
		  '((:file "no.txt")))))))
   (nil
      (progn
	 (emt:doc "Situation: Validator given. It's a string that matches the regexp
pattern.")
	 (emt:doc "Response: Proceed normally.")
	 (emtb:with-buf
	    (:string emtb:buf-contents-matches:thd:yes\.txt)
	    (emtb:buf-contents-matches :file "regexp-yes.txt" :dir emtb:buf-contents-matches:thd:dir :regex-marks
	       '("[" "]")
	       :validate-re
	       `((:string ,emtb:buf-contents-matches:thd:yes\.txt)))))))


;;;_   , emtb:extract-regexp

(emt:deftest-3 emtb:extract-regexp
   (nil
      (progn
	 (emt:doc "Situation: An empty string.")
	 (emt:doc "Response: regexp is just ^$ to only accept the empty string")
	 (equal
	    (emtb:extract-regexp "" "[" "]")
	    "^$")))
   (nil
      (progn
	 (emt:doc "Situation: String is just one regexp
Return: that regexp surrounded by ^$ to match a whole string.")
	 (equal
	    (emtb:extract-regexp "[b+]" "[" "]")
	    "^b+$")))
   (nil
      (progn
	 (emt:doc "Situation: String is several parts, regexps are trivial.
Return: those parts in order, surrounded by ^$ to match a whole string.")
	 (equal
	    (emtb:extract-regexp "a[b]c" "[" "]")
	    "^abc$")))
   (nil
      (progn
	 (emt:doc "Situation: A character class is given in bracket form.
Recognizers are brackets.")
	 (emt:doc "Response: Gives error")
	 (emth:gives-error
	    (emtb:extract-regexp "[[:digit:]*]" "[" "]"))))
   (nil
      (progn
	 (emt:doc "Situation: A character class is given in bracket form.
Recognizers are brackets.")
	 (emt:doc "Response: Extracts the character class indicator correctly")
	 (equal
	    (emtb:extract-regexp "%[[:digit:]*%]" "%[" "%]")
	    "^[:digit:]*$"))))


;;;_   , emtb:string-matches-re
(put 'emtb:string-matches-re 'emt:test-thru 'emtb:string-matches)



;;;_   , emtb:string-matches

(emt:deftest-3 emtb:string-matches
   (nil
      (progn
	 (emt:doc "Param: `regex-marks' is not given.  
Action: Compare to expected as a string.")
	 (emtb:string-matches "abc def" "abc def")))
   (nil
      (progn
	 (emt:doc "Situation: `string-got' and `expected' do not match.")
	 (emt:doc "Response: Comparison fails.")
	 (not
	    (emtb:string-matches "abc" "def"))))
   (nil
      (progn
	 (emt:doc "Param: `regex-marks' is a pair of strings.
Action: Compare to a regex formed from file.  Any part between those
strings as fore/after markers.")
	 (emtb:string-matches "abbbc" "a[b+]c"
	    '("[" "]"))))
   (nil
      (progn
	 (emt:doc "Situation: regex-style expected string which doesn't match")
	 (emt:doc "Response: Comparison fails.")
	 (not
	    (emtb:string-matches "abbbd" "a[b+]c"
	       '("[" "]")))))
   (nil
      (progn
	 (emt:doc "Situation: regex matches any space")
	 (emt:doc "Response: Comparison succeeds.")
	 (emtb:string-matches "a b	c" "a[\\s-*]b[\\s-*]c"
	    '("[" "]"))))
   (nil
      (progn
	 (emt:doc "Situation: regex matches any space.  It's given in
character-class form.")
	 (emt:doc "Response: Comparison succeeds.")
	 (emtb:string-matches "a b	c" "a%[[[:space:]]*%]b%[[[:space:]]*%]c"
	    '("%[" "%]"))))
   (nil
      (progn
	 (emt:doc "Situation: regex matches any number")
	 (emt:doc "Response: Comparison succeeds.")
	 (emtb:string-matches "a0123b456" "a%[[0-9]*%]b%[[0-9]+%]"
	    '("%[" "%]"))))
   (nil
      (progn
	 (emt:doc "Situation: regex (in char class form) matches any number")
	 (emt:doc "Response: Comparison succeeds.")
	 (emtb:string-matches "a0123b456" "a%[[[:digit:]]*%]b%[[[:digit:]]+%]"
	    '("%[" "%]"))))
   (nil
      (progn
	 (emt:doc "Situation: regex-style file has, as a non-regexp section, some
magic regexp characters.")
	 (emt:doc "Response: Comparison still treats that section as literal.")
	 (emtb:string-matches "a+b+|c" "a+b+|c"
	    '("[" "]"))))
   (nil
      (progn
	 (emt:doc "Param: `regex-marks' is something else.  
Action: Error.")
	 (emth:gives-error
	    (emtb:string-matches "a" "a" "Just one string")))))


;;;_    . emtb:buffer-object

(emt:deftest-3 emtb:buffer-object
   (nil
      (progn
	 (emt:doc "Shows: `emtb:buffer-object' reads the lisp object whose
printed representation the current buffer contains.")
	 (equal
	    (emtb:with-buf
	       (:string "")
	       (pp
		  '(1 5)
		  (current-buffer))
	       (emtb:buffer-object))
	    '(1 5))))
   (nil
      (progn
	 (emt:doc "Shows: `emtb:buffer-object' and
`emtb:with-buf' are complementary.")
	 (equal
	    (emtb:with-buf
	       (:sexp
		  '(ab ef))
	       (emtb:buffer-object))
	    '(ab ef)))))





;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/filebuf/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/filebuf/tests.el ends here
