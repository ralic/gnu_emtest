;;;_ emtest/testhelp/mocks/filebuf/rtest.el --- Rtests for filebuf

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
(require 'emtest/testhelp/mocks/filebuf)
(require 'emtest/testhelp/mocks/filebuf/testhelp)


;;;_. Body
;;;_   , emtb:with-buf-f
(put 'emtb:with-buf-f 'rtest:test-thru
   'emtb:with-buf)

;;;_    . emtb:with-buf

(rtest:deftest emtb:with-buf
   ("Param: :string STRING is given
Reaction: Fill the buffer with exactly STRING"
      (emtb:with-buf (:string "abc")
	 (equal
	    (buffer-string)
	    "abc")))

   ("Param: :sexp OBJECT is given.
Reaction: Fill the buffer with exactly a printed representation of
OBJECT"
      (equal
	 (emtb:with-buf 
	    (:sexp '(1 5))
	    (emtb:buffer-object))
	 '(1 5)))
   
   ("Param: :point-replaces STR is given.
Reaction: The first occurence of STR is deleted "
      (emtb:with-buf 
	 (:string "abcdef" :point-replaces "cd")
	 (equal
	    (buffer-string)
	    "abef")))

   ("Param: :point-replaces STR is given.
Reaction: Point is now where STR was "
      (emtb:with-buf 
	 (:string "abcdef" :point-replaces "cd")
	 (looking-at "ef$")))
   
   ("Shows: :point-replaces can cause parts of an object to be unprinted."
      (equal
	 (emtb:with-buf 
	    (:sexp '(ab cd ef)
	       :point-replaces "cd")
	    (emtb:buffer-object))
	 '(ab ef))
      )

   ("Restores the original buffer even in case of error."
      (let
	 ((win
	     (current-window-configuration)))
	    
	 (ignore
	    (rtest-one-probe-0
	       '((emtb:with-buf (:string "abc")
		    (error "A deliberate error")))))
	 (equal 
	    win
	    (current-window-configuration))))

   ;;YAGNI: Also mark-replaces and markers-replace (which takes and
   ;;sets an alist of markers, so they can be known by name).  


   ;;Exercise the `visited-name' feature.
   ;;Missing the directory
   (  "Exercise the `visited-name' feature."

      (emtg:with emtb:thd:examples 
	 ((project emtest)(library mockbuf))
	 (let 
	    (  (master-file (emtg (role master)(type filename)))
	       (slave-file  (emtg (role slave)(type filename))))
	    ;;Remove the slave file if it already exists.
	    (when (file-exists-p slave-file)
	       (delete-file slave-file))
	    ;;Visit slave-file, but with contents from master-file.
	    (emtb:with-buf 
	       (:file master-file
		  :visited-name slave-file)
	       ;;Save the file (and don't back it up)
	       (save-buffer 0))
	    ;;The files' contents match
	    (emtb:files-match master-file slave-file))))
   
   ;;Missing the directory
   (  "Situation: The slave file already exists.
Response: Works the same."
      (emtg:with emtb:thd:examples 
	 ((project emtest)(library mockbuf))
	 (let 
	    (  (master-file (emtg (role master)(type filename)))
	       (slave-file  (emtg (role slave)(type filename))))
	    ;;Create the slave file if it doesn't exist
	    (unless (file-exists-p slave-file)
	       (with-temp-buffer
		  (insert "Some unmatched text")
		  (write-file slave-file nil)))
	    ;;Visit slave-file, but with contents from master-file.
	    (emtb:with-buf 
	       (:file master-file
		  :visited-name slave-file)
	       ;;Save the file (and don't back it up)
	       (save-buffer 0))
	    ;;The files' contents match
	    (emtb:files-match master-file slave-file))))


   (  "Param: the symbol `tmp' is given as `:visited-name'.
Behavior: Creates a temporary file."
      (emtg:with emtb:thd:examples ((project emtest)(library mockbuf))
	 (let 
	    (  slave-file
	       (master-file (emtg (role master)(type filename))))
	    ;;Visit slave-file, but with contents from master-file.
	    (emtb:with-buf 
	       (:file master-file
		  :visited-name 'tmp)
	       ;;Get its name.
	       (setq slave-file (buffer-file-name))
	       ;;Save the file (and don't back it up)
	       (save-buffer 0))
	    ;;The files' contents match
	    (emtb:files-match master-file slave-file))))

   ;;Missing the directory
   (  "Param: the symbol `tmp' is given as `:visited-name'.
Neither `dir' nor `file' is given.
Behavior: Creates a temporary file with those contents."
      (emtg:with emtb:thd:examples ((project emtest)(library mockbuf))
	 (let 
	    (  slave-file
	       (master-file (emtg (role master)(type filename))))
	    ;;Visit slave-file, but with contents from master-file.
	    (emtb:with-buf 
	       (:string "abc"
		  :visited-name 'tmp)
	       ;;Get its name.
	       (setq slave-file (buffer-file-name))
	       ;;Save the file (and don't back it up)
	       (save-buffer 0))
	    ;;The files' contents match
	    (assert
	       (string=
		  "abc"
		  (emtb:file-contents-absname slave-file))
	       t))))


   (  "Shows: `sequence' works.
Param: sequence with two strings
Response: Contents of buffer are as expected."
      (emtb:with-buf 
	 (:sequence 
	    (
	       (string "abc")
	       (string "def")))
	 (equal
	    (buffer-string)
	    "abcdef")))

   ;;$$Point

   ;;$$Named mark

   )

;;;_   , emtb:with-file-f

(rtest:deftest emtb:with-file-f
   (  "Proves: Body form is run - it changes the value of `x'."
      (let
	 ((str "abc def ghi")
	    x)
	 (emtb:with-file-f (:string str)
	    filename
	    (setq x 12))
	 (assert (equal x 12) t)
	 t))

   (  "Proves: The filename is absolute."
      (let
	 ((str "abc def ghi"))
	 (emtb:with-file-f (:string str)
	    filename
	    (assert
	       (file-name-absolute-p filename)
	       t))
	 t))

   (  "Proves: Th file contents match the expected contents."
      (let
	 ((str "abc def ghi")
	    x)
	 (emtb:with-file-f (:string str)
	    filename
	    (assert
	       (string=
		  (emtb:file-contents-absname filename)
		  str)
	       t))
	 t))

   (  "Proves: `emtb:with-file-f' returns the result of running
BODY."
      (let
	 ((str "abc def ghi"))
	 (assert
	    (equal
	       (emtb:with-file-f (:string str)
		  filename
		  12)
	       12)
	    t)
	 t))
   
   (  "Proves: After the body has finished running, the file is gone."
      (let
	 (filename-kept)
	 (emtb:with-file-f (:string "abc def ghi") filename
	    (setq filename-kept filename))
	 (assert (not (file-exists-p filename-kept)) t)
	 t))


   ("Param: The flag `:absent' is given and is non-nil.
No file is made."
      (emtb:with-file-f (:absent t) filename
	 (assert (not (file-exists-p filename)) t)
	 t))
   

      ("Param: The flag `:absent' is given and is non-nil.
Situation: That file is created by body.
Response: After body has run, file no longer exists."
	 (let
	    (filename-kept)
	    (emtb:with-file-f (:absent t) filename
	       (setq filename-kept filename)
	       (with-temp-buffer
		  (insert "Some stuff")
		  (write-file filename)))
	    (assert (not (file-exists-p filename-kept)) t)
	    t))
   
   )

;;;_    . emtb:cautious-insert-file
(put 'emtb:cautious-insert-file 'rtest:test-thru
   'emtb:with-buf)

;;;_    . emtb:cautious-load-file (OBSOLESCENT)
'
(put 'emtb:cautious-load-file 'rtest:test-thru
   'emtb:with-buf)

;;;_    . emtb:find-file-goto-text (OBSOLESCENT)

(rtest:deftest emtb:find-file-goto-text
   ("Situation: Filename is not absolute.
Response: Error."
      (rtest:gives-error (emtb:find-file-goto-text ".")))
   )

;;;_    . emtb:find-file-2

;;Tested thru emtb:make-form-find-file, but that's increasingly
;;less adequate.

;;Tested thru ade:deactivate-siblings, but that's obsolete.

;;;_   , emtb:buf-is-file
(rtest:deftest emtb:buf-is-file
   ;;Even when called in a different buffer than BUF, returns correct
   ;;decision.
   '
   (  "Situation: WRITEME.
Response: WRITEME."
      (progn) ;;Test-form
      )
   ;;Even when file is opened with a non-absolute name, returns
   ;;correct decision.
   )
;;;_   ; emtb:buf-is-file-at

(rtest:deftest emtb:buf-is-file-at

   ("Situation: Positions simply don't match.  
Response: return nil."

      (not
	 (let
	    ((filename
		(emtb:virtual-dir:base->filename 
		   "file1" 
		   emtb:check-file:thd:virtual-dir))
	       (str "position=mockbuf-1"))
	    (with-current-buffer
	       (find-file-noselect filename)
	       (goto-char 4)
	       (emtb:buf-is-file-at filename str)))))


   ("Situation: emtb:find-file-goto-text has just been called with
the same arguments. No motion argument is given.
Response: Returns non-nil."

      (let
	 ((filename
	     (emtb:virtual-dir:base->filename 
		"file1" 
		emtb:check-file:thd:virtual-dir))
	    (str "position=mockbuf-1"))
	 (with-current-buffer
	    (emtb:find-file-goto-text filename str)
	    (emtb:buf-is-file-at filename str))))


   ("Situation: MOTION moves to same place (BOB).  
Response: return non-nil."

      (let
	 ((filename
	     (emtb:virtual-dir:base->filename 
		"file1" 
		emtb:check-file:thd:virtual-dir))
	    (str "position=mockbuf-1"))
	 (with-current-buffer
	    (find-file-noselect filename)
	    (goto-char 4)
	    (emtb:buf-is-file-at filename str
	       #'(lambda nil
		    (goto-char
		       (point-min)))))))

   ("Situation: Original positions are in different places, but
MOTION finds a % that precedes both.  
Response: return non-nil."

      (let
	 ((filename
	     (emtb:virtual-dir:base->filename 
		"file2" 
		emtb:check-file:thd:virtual-dir)))
	 (with-current-buffer
	    (emtb:find-file-goto-text filename "position=mockbuf-1")
	    (emtb:buf-is-file-at filename "position=mockbuf-meta-1"
	       #'(lambda nil
		    (search-backward "%"))))))


   ("Situation: MOTION can't be executed.  
Response: error."

      (let
	 ((filename
	     (emtb:virtual-dir:base->filename 
		"file1" 
		emtb:check-file:thd:virtual-dir)))
	 (with-current-buffer
	    (find-file-noselect filename)
	    (not
	       (ignore-errors
		  (progn
		     (emtb:buf-is-file-at filename str
			'(1 2))
		     t))))))

	  
   )

;;;_   , emtb:file-contents-absname

(put 'emtb:file-contents-absname 'rtest:test-thru
   'emtb:buf-contents-matches)

;;;_   , emtb:buf-contents-matches

(rtest:deftest emtb:buf-contents-matches

   ;;Comparison to string

   ( "Param: `string' is given.
Action: Treat string as the expected value." 
      (emtb:with-buf
	 (:string "abc def")
	 (emtb:buf-contents-matches 
	    :string "abc def")))

   ( "Param: `string' is given.
Action: Treat string as the expected value." 
      (not
	 (emtb:with-buf
	    (:string "abc def")
	    (emtb:buf-contents-matches 
	       :string "Do not match\n"))))

   ( "Param: `string' and `regex-marks' are given.
Action: Treat string as containing bounded regexps, as for
emtb:string-matches." 
      (emtb:with-buf
	 (:string "abc def")
	 (emtb:buf-contents-matches 
	    :string "abc [.*]"
	    :regex-marks '("[""]"))))

   
   ( "Behavior: Point is not moved." 
      (emtb:with-buf
	 (:string "abc def")
	 (goto-char 3)
	 (emtb:buf-contents-matches 
	    :string "abc def")
	 (= (point) 3)))
   
   ;;Comparison to file
   
   (  "Situation: Filename is not absolute
Response: Error."
      ;;Set directory and be sure file exists, so it's not missing
      ;;file error.
      (let ((default-directory emtb:buf-contents-matches:thd:dir))
	 (rtest:gives-error
	    (emtb:buf-contents-matches 
	       :file "yes.txt"))))

   ;;Param: `dir' is given.  Action: file is expanded wrt that dir.
   ;;Could create another example in another dir to demo this, but
   ;;YAGNI)

   (  "Situation: `dir' is given but expanded filename is not absolute,
Response: Error."
      (rtest:gives-error
	 (emtb:buf-contents-matches 
	    :file "yes.txt" 
	    :dir ".")))

   (  "Situation: File does not exist
Response: Error."
      (rtest:gives-error
	 (emtb:buf-contents-matches 
	    :file "invalid.txt" 
	    :dir emtb:buf-contents-matches:thd:dir)))

   ( "Param: `buf' is given.
Action: Compare file to contents of `buf'."
      (emtb:with-buf
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (let
	    ((buf (current-buffer)))
	    (with-temp-buffer
	       (insert "This buffer would not match the file")
	       (emtb:buf-contents-matches 
		  :file "yes.txt" 
		  :dir emtb:buf-contents-matches:thd:dir
		  :buf buf)))))
   
   ( "Param: `buf' is not given.
Action: Compare file to contents of current buffer."
      (emtb:with-buf
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (emtb:buf-contents-matches 
	    :file "yes.txt" 
	    :dir emtb:buf-contents-matches:thd:dir)))

   ( "Situation: Buffer contents and file contents do not match.
Response: Comparison fails."
      (not 
	 (emtb:with-buf
	    (:string emtb:buf-contents-matches:thd:yes.txt)
	    (emtb:buf-contents-matches 
	       :file "no.txt" 
	       :dir emtb:buf-contents-matches:thd:dir))))

   ( "Param: `regex-marks' is given.
Action: Treat file as containing bounded regexps, as for
emtb:string-matches." 
      (emtb:with-buf
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (emtb:buf-contents-matches 
	    :file "regexp-yes.txt" 
	    :dir emtb:buf-contents-matches:thd:dir
	    :regex-marks '("[""]"))))
   

   ;;Comparison to sexp object
   ( "Param: `sexp' is given.
Action: Succeed just if the buffer contains the sexp representation
of the object." 
      (emtb:with-buf
	 (:string "(a b c)")
	 (emtb:buf-contents-matches 
	    :sexp '(a b c))))

   ( "Param: `sexp' is given.
Behavior: Point is not moved" 
      (emtb:with-buf
	 (:string "(a b c)")
	 (goto-char 3)
	 (emtb:buf-contents-matches 
	    :sexp '(a b c))
	 (= (point) 3)))
   


   ;;YAGNI: and not if it contains something readable after that
   ;;object.

   ;;YAGNI: object-list

   ;;** Subgroup: Args for validation. **
   ;;Param: `:regex-marks's value contains the key `:validate-re',
   ;;whose value is a list of validators.

   ;;Background: "regexp-yes.txt" matches "yes.txt" and
   ;;matches emtb:buf-contents-matches:thd:yes.txt.

   ;;$$And a test should test that directory should be the same.
   ("Situation: Validator is given. It's a file that matches the
regexp pattern. 
Response: Proceed normally."
      (emtb:with-buf
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (emtb:buf-contents-matches 
	    :file "regexp-yes.txt" 
	    :dir emtb:buf-contents-matches:thd:dir
	    :regex-marks 
	    '("[""]" )
	    :validate-re
	    '((:file "yes.txt")))))
   

   ("Situation: Validator is given. It's a file that does not match
the regexp pattern. 
Response: Error."
      (emtb:with-buf
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (rtest:gives-error
	    (emtb:buf-contents-matches 
	       :file "regexp-yes.txt" 
	       :dir emtb:buf-contents-matches:thd:dir
	       :regex-marks 
	       '("[""]")
	       :validate-re
	       '((:file "no.txt"))))))
   

   ( "Situation: Validator given. It's a string that matches the regexp
pattern. 
Response: Proceed normally."
      (emtb:with-buf
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (emtb:buf-contents-matches 
	    :file "regexp-yes.txt" 
	    :dir emtb:buf-contents-matches:thd:dir
	    :regex-marks 
	    '("[""]" )
	    :validate-re
	    `((:string ,emtb:buf-contents-matches:thd:yes.txt)))))
   
   )

;;;_   , emtb:extract-regexp

(rtest:deftest emtb:extract-regexp
   ;;All slightly too narrow because they test the form of the return
   ;;value, not semantics. YAGNI fix.
   ("Situation: An empty string.
Response: regexp is just ^$ to only accept the empty string"
      (equal
	 (emtb:extract-regexp "" "[" "]")
	 "^$"))

   
   (  "Situation: String is just one regexp
Return: that regexp surrounded by ^$ to match a whole string."
      (equal
	 (emtb:extract-regexp "[b+]" "[" "]")
	 "^b+$"))
   
   (  "Situation: String is several parts, regexps are trivial.
Return: those parts in order, surrounded by ^$ to match a whole string."
      (equal
	 (emtb:extract-regexp "a[b]c" "[" "]")
	 "^abc$"))

   ( "Situation: A character class is given in bracket form.
Recognizers are brackets.
Response: Gives error"

      (rtest:gives-error
	 (emtb:extract-regexp "[[:digit:]*]" "[" "]")))
   
   ( "Situation: A character class is given in bracket form.
Recognizers are brackets.
Response: Extracts the character class indicator correctly"

      (equal
	 (emtb:extract-regexp "%[[:digit:]*%]" "%[" "%]")
	 "^[:digit:]*$"))
   
   )

;;;_   , emtb:string-matches-re
(put 'emtb:string-matches-re 'rtest:test-thru
   'emtb:string-matches)

;;;_   , emtb:string-matches

(rtest:deftest emtb:string-matches

   ;;Would be nice to apply the "Param" doc to a section, but not
   ;;currently easy.
   ( "Param: `regex-marks' is not given.  
Action: Compare to expected as a string."
      (emtb:string-matches
	 "abc def"
	 "abc def"))
   
   ( "Situation: `string-got' and `expected' do not match.
Response: Comparison fails."
      (not 
	 (emtb:string-matches
	    "abc"
	    "def")))

   ( "Param: `regex-marks' is a pair of strings.
Action: Compare to a regex formed from file.  Any part between those
strings as fore/after markers."

	 (emtb:string-matches
	    "abbbc"
	    "a[b+]c"
	    '("[""]")))
   
   
   ( "Situation: regex-style expected string which doesn't match
Response: Comparison fails."
      (not
	 (emtb:string-matches
	 "abbbd"
	 "a[b+]c"
	 '("[""]"))))
   
   ;;A common case.  Note that in Elisp modes the "space" character
   ;;class does not match \n. 
   ( "Situation: regex matches any space 
Response: Comparison succeeds."

      (emtb:string-matches
	 "a b\tc"
	 ;;Note double escapes.
	 "a[\\s-*]b[\\s-*]c"
	 '("[""]")))

   ;;Note the need to use different regex delimiters than ("[""]")
   ( "Situation: regex matches any space.  It's given in
character-class form.
Response: Comparison succeeds."

      (emtb:string-matches
	 "a b\tc"
	 ;;Note double escapes.
	 "a%[[[:space:]]*%]b%[[[:space:]]*%]c"
	 '("%[""%]")))

   ( "Situation: regex matches any number 
Response: Comparison succeeds."

      (emtb:string-matches
	 "a0123b456"
	 "a%[[0-9]*%]b%[[0-9]+%]"
	 '("%[""%]")))

   ( "Situation: regex (in char class form) matches any number 
Response: Comparison succeeds."

      (emtb:string-matches
	 "a0123b456"
	 "a%[[[:digit:]]*%]b%[[[:digit:]]+%]"
	 '("%[""%]")))

   ( "Situation: regex-style file has, as a non-regexp section, some
magic regexp characters.
Response: Comparison still treats that section as literal."

	 (emtb:string-matches "a+b+|c" "a+b+|c"  '("[""]")))

   ;;(YAGNI) If `regex-marks' is `t', use file as regex with default
   ;;markers (TBD)

   ( "Param: `regex-marks' is something else.  
Action: Error."
      (rtest:gives-error
	 (emtb:string-matches "a" "a"  "Just one string")))

   
   )

;;;_    . emtb:buffer-object

(rtest:deftest emtb:buffer-object

   ("Shows: `emtb:buffer-object' reads the lisp object whose
printed representation the current buffer contains."
      (equal
	 (emtb:with-buf (:string "")
	    (pp '(1 5) (current-buffer))
	    (emtb:buffer-object))
	 '(1 5)))

   ("Shows: `emtb:buffer-object' and
`emtb:with-buf' are complementary."
      (equal
	 (emtb:with-buf 
	    (:sexp '(ab ef))
	    (emtb:buffer-object))
	 '(ab ef)))
   )




;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/filebuf/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/filebuf/rtest.el ends here
