;;;_ emtest/testhelp/mocks/filebuf.el --- Test helper: Buffer visiting file, point controlled

;; Copyright (C) 2007, 2008 Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@localhost.localdomain>
;; Keywords: lisp

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

;;; Commentary:

;;Meta: mockbuf could support virtual directories better.  What it
;;does doesn't quite fit how they are used.

;;PLAN:

;;Structurally, perhaps split into an implementation (just remember a
;;list of data) and interface (makes it do fancy stuff)

;;Support conversion-pairs, where one is changed and compared to the
;;other.  Allow specifying them by relative suffixes, relative
;;directories, possibly other ways.  Support informing this comparison
;;via the data

;;New PLAN
;;

;;Use `with-buffer-containing-object' as the main entry point here.
;;This is largely done now.

;;Better document its args.

;;The virtual directory stuff before was probably a mistake.  We
;;largely just don't want to mess up master files and directories when
;;we alter file contents.  That's easier.

;;One thing we may need is trying to read canonically-named files, eg
;;config files.  They mustn't be in the canonical places, so we may
;;need to know the path that we copied stuff to.

;;Maybe make read-final an available function rather than an argument
;;to `with-buffer-containing-object'?  Clearer and more flexible.
;;Some informational functions around here are applicable too:
;;emtb:files-match, emtb:buf-is-file-at, emtb:buf-is-file.

;; When we do want saving to go somewhere else, that should be
;;effected by giving further args to `emtb:cautious-load-file'.  A
;;tmp-dir argument computed by mockbuf itself?  Used just if
;;`with-buffer-containing-object' gets an additional argument.  And
;;(actually found this needed earlier) sometimes we need the visited
;;filename for informational purposes.



;;;_. Headers

;;;_ , Requires

(require 'cl)
(require 'utility/accumulator)

;;;_ , Config
(defgroup emtest/testhelp/mocks/filebuf ()
   "Customization for the Emtest filebuf mock"
   :prefix 'emtb:
   :group 'emtest/testhelp)

(defcustom emtb:slave-root
   "/tmp/"
   "The directory where mockbuf is to place temporary files.
You may want to use a ramdisk if available"
   :type 'file
   :group 'emtest/testhelp/mocks/filebuf)

(defcustom emtb:slave-init-hook
   nil
   "Function(s) to call before using `emtb:slave-root'.
You may want to use this to mount a ramdisk" 
   :type 'hook
   :group 'emtest/testhelp/mocks/filebuf)

(defcustom emtb:slave-term-hook
   nil
   "Function(s) to call after using `emtb:slave-root'.
You may want to use this to umount a ramdisk" 
   :type 'hook
   :group 'emtest/testhelp/mocks/filebuf)

;;;_: Code:
;;;_ , emtb:expand-filename-by-load-file
;;;###autoload
(defun emtb:expand-filename-by-load-file (filename)
   ""
   (expand-file-name filename
      (if load-file-name
	 (file-name-directory load-file-name))))


;;;_ , Setup file

;;;_  . Buffer containing known contents
;;;_   , Formdata structure emtb:bufcontaining:formdata

;;;_   , with-buffer-containing-object

(defmacro with-buffer-containing-object (args &rest body)
   ""
   (apply #'with-buffer-containing-buildform body args))

;;;_    . Worker `with-buffer-containing-buildform'

;;Key `:printed-object' has been replaced by key`:sexp'
(defun* with-buffer-containing-buildform 
   (body &key printed-object sexp string file dir visited-name
      point-replaces
      sequence)
   ""

   `
   (with-temp-buffer
      ;;Do all the inserting
      ,(cond
	  (sexp
	     (emtb:build-insertform dir 'sexp sexp))
	  (file
	     (emtb:build-insertform dir 'file file))
	  (sequence
	     (emtb:build-insertform dir 'sequence sequence))
	  (t 
	     (emtb:build-insertform dir 'string string)))

      ;;Set up file, if reasonable.
      ,(when (or visited-name file)
	  `(emtb:cautious-setup-file 
	      ,visited-name 
	      ,(or dir
		  (if file
		     `(file-name-directory ,file))
		  nil)))
      
      ,(if point-replaces
	  `(emtb:goto-text ,point-replaces t)
	  '(goto-char (point-min)))
      
      ,@body))
;;;_   , Worker emtb:build-insertform
;;DIR may become a general data element.
(defun emtb:build-insertform (dir governor &rest args)
   "Build an inserter form wrt GOVERNOR."
   
   (apply
      (case governor
	 (sexp
	    #'(lambda (sexp)
		 `(pp ,sexp (current-buffer))))
	 (string
	    #'(lambda (string)
		 `(insert ,string)))
	 (file
	    #'(lambda (file)
		 `(emtb:cautious-insert-file 
		     (expand-file-name ,file ,dir) 
		     t)))
	 (sequence
	    #'(lambda (sequence)
		 `(progn
		     ,@(mapcar
			  #'(lambda (x)
			       (apply #'emtb:build-insertform 
				  dir
				  x))
			  sequence))))

	 ;;$$Add (mark name &optional insert-before)
	 ;;(Save marks in buffer-local var emtb:named-marks)
	 ;;$$Add point
	 
	 )
      args))

;;;_  . File containing known contents
(defmacro with-file-containing (args var &rest body)
   ""
   (let
      ((temp-file-sym (make-symbol "temp-file")))
      `(let*
	  (  
	     (--absent
		(apply #'emtb:absent-flag ',args))
	     (,temp-file-sym 
		(if --absent
		   (make-temp-name ,emtb:slave-root)
		   (make-temp-file ,emtb:slave-root)))
	     (,var ,temp-file-sym))
	  
	  
	  (unless
	     --absent
	     (with-buffer-containing-object ,args 
		(write-file ,temp-file-sym)))
	  
	  (unwind-protect
	     (progn ,@body)
	     (when
		(file-exists-p ,temp-file-sym)
		(delete-file ,temp-file-sym))))))
;;;_  . emtb:string-containing-object 

(defmacro emtb:string-containing-object (spec)
   ""

   `(with-buffer-containing-object ,spec
      (buffer-string)))

;;;_   , Tests

'
(assert
   (string=
      (emtb:string-containing-object (:string "abc"))
      "abc"))

;;;_  . emtb:string-containing-object-f
(defun emtb:string-containing-object-f (spec)
   ""
   (eval
      `(emtb:string-containing-object ,spec)))

;;;_   , Helper emtb:absent-flag
(defun* emtb:absent-flag (&rest dummy &key absent &allow-other-keys)
   ""
   absent)


;;;_  . Find file
;;;_   , emtb:goto-mark (Not written yet and YAGNI)

;;Take an optional alist associating string to mark.
;;If not passed, use current buffer's local value.  If there's none,
;;error and explain.
;;`with-buffer-containing-object' should set that list up.

;;    Approach: Have a file marking that means to delete the marking,
;;    text and all, but keep a mark and an alist relating text to mark.
;;    Most natural way is to make that a buffer-local variable - but
;;    could make hard-to-find error if local vars get deleted later, eg
;;    if we enter a mode.  Say, throw a clear error in that case and
;;    suggest that one capture the variable in question and feed it in
;;    explicitly.


;;;_   , emtb:goto-text

;;$$RETHINK ME Want to do this differently: `emtb:goto-mark' will be
;;another function.  Any replacement will be done by setting it for
;;it; this no longer will do replacement.
(defun emtb:goto-text (loc-string &optional replace)
   ""

   (goto-char (point-min))
   (search-forward loc-string)
   (if 
      replace
      (replace-match "")
      (goto-char (match-beginning 0))))

;;;_   , emtb:cautious-insert-file
(defun emtb:cautious-insert-file (filename &optional visit beg end replace)
   ""

   (unless
      (file-name-absolute-p filename)
      (error "Filename %s is not absolute" filename))

   (unless
      (file-exists-p filename)
      (error "File %s doesn't exist" filename))

   ;;We're enforcing never inserting into a full buffer.
   ;;Alternatively, this could be left to `insert-file-contents',
   ;;which will error if buffer is non-empty and visit is non-nil.
   ;;TBD.
   (unless
      (= (buffer-size) 0)
      (error "Buffer is not empty"))
   
   (insert-file-contents filename visit beg end replace))

;;;_   , emtb:cautious-setup-file
(defun emtb:cautious-setup-file (visited-name dir)
   ""

   (when dir (after-find-file))
   (case visited-name
      ;;By default we do not want to risk saving changes made during a
      ;;test into the original file, but we do want it to know
      ;;default-directory like an ordinary buffer.  So set the buffer
      ;;read-only.  Managing `default-directory' here seems
      ;;unavoidable.
      ((nil)
	 (setq default-directory dir)
	 (setq buffer-read-only t))

      ;;Make this buffer mutable but visit nowhere.
      ((t)
	 (set-visited-file-name nil t nil))
      ;;Automatically create a temp file.
      ((tmp)
	 ;;Could have used `make-temp-name' but it's said to open
	 ;;security holes.
	 (set-visited-file-name 
	    (make-temp-file emtb:slave-root) t nil))
      (t
	 (let
	    ((abs-name
		(if
		   (file-name-absolute-p visited-name)
		   visited-name
		   (expand-file-name visited-name dir))))
	    (set-visited-file-name abs-name t nil)))))
;;;_   , emtb:find-file-goto-text

;;Deprecated.  This has been split into `emtb:cautious-find-file'
;;and the part that manages loc-string and (now) prepare-f

;;May become only a test helper.  Arg LOC-STRING need not be optional.

;;$$OBSOLESCENT Prepare to replace this.  Several tests use it and
;;emtb:find-file-2 uses it with no options.  `emtb:find-file-2' itself
;;is obsolescent.

(defun emtb:find-file-goto-text (filename &optional loc-string)
   ""

   (let
      ((buf (emtb:cautious-find-file filename)))
      (with-current-buffer buf
	 ;;Almost replaceable.
	 (if loc-string
	    (emtb:goto-text loc-string)
	    (goto-char (point-min))))
      
      buf))


;;;_ , Check file is what's expected

;;;_  . emtb:buf-is-file
;;;###autoload
(defun emtb:buf-is-file (buf filename)
   ""
   (if
      (not (file-name-absolute-p filename))
      (error "emtb:buf-is-file: FILENAME should be absolute")
      (string= (buffer-file-name buf) filename)))

;;;_  . emtb:buf-is-file-at

;;;###autoload
(defun emtb:buf-is-file-at (filename loc-string &optional motion)
   "Return non-nil if current buffer is visiting FILENAME and point is
at LOC-STRING.

If MOTION is given, start point at the beginning of LOC-STRING and
execute MOTION, then check that point is at the same place.  MOTION,
if given, must be a function."

   (and
      (emtb:buf-is-file (current-buffer) filename)
      (if
	 (null motion)
	 (looking-at loc-string)
	 (if
	    (not (functionp motion))
	    (error 
	       "emtb:buf-is-file-at: MOTION is not a function")
	    (save-excursion
	       (let 
		  ((point-1
		      (progn
			 (funcall motion)
			 (point)))
		     (point-2
			(progn
			   (goto-char (point-min))
			   (search-forward loc-string)
			   (funcall motion)
			   (point))))
		  ;;Return whether the points are the same
		  (= point-1 point-2)))))))



;;;_  . emtb:file-contents

(defun emtb:file-contents (filename)
   ""
   (with-temp-buffer
      (emtb:cautious-insert-file filename)
      (buffer-string)))


;;;_  . emtb:file-contents-absname
;;;###autoload
(defun emtb:file-contents-absname (file &optional dir)
   "Return the contents of a file given by absolute name"
   (let
      (
	 (filename
	    (if dir
	       (expand-file-name file dir)
	       file)))
      (unless (file-name-absolute-p filename)
	 (error "File name should be absolute"))
      (emtb:file-contents filename)))

;;;_  . emtb:contents
;;$$RENAME ME emtb:get-contents
(defun* emtb:contents (default-dir &key file dir string &allow-other-keys)
   "Get contents, according to arguments.
For internal use by filebuf.  Outside callers probably want
`emtb:file-contents-absname'."

   (cond
      (file
	 (emtb:file-contents-absname file (or dir default-dir)))
      (string
	 string)))

;;;_  . emtb:files-match

;;;###autoload
(defun emtb:files-match (a b)
   "Return nil if the files differ, non-nil if they don't"
   (string-equal (emtb:file-contents a) (emtb:file-contents b)))

;;;_  . Structures for holding comparison data

;;$$DESIGN ME Merge these with TESTRAL comparison holders
(defstruct emtb:direct-comparison-data
   ""
   got
   expected)


(defstruct emtb:regex-comparison-data
   ""
   got
   pattern-expected)


;;;_  . emtb:last-bad-comparison
;;$$DESIGN ME Merge these with TESTRAL result lists
(defvar emtb:last-bad-comparison nil 
   "" )
;;;_  . emtb:ediff-last-comparison
;;Maybe belong in an associated file instead.
;;;###autoload
(defun emtb:ediff-last-comparison (comparison)
   ""
   (interactive (list emtb:last-bad-comparison))

   (cond
      ((null comparison)
	 (error "Can't find any previous comparison"))
      ((emtb:direct-comparison-data-p comparison)
	 (let 
	    ;;Make a buffer for each
	    ((buf-expected (generate-new-buffer "expected"))
	       (buf-got    (generate-new-buffer "got")))
	    ;;Put each into its buffer 
	    (with-current-buffer buf-expected
	       (insert (emtb:direct-comparison-data-expected comparison)))
	    (with-current-buffer buf-got
	       (insert
		  (emtb:direct-comparison-data-got comparison)))
	    ;;Run ediff on those buffers.
	    (ediff-buffers buf-expected buf-got)))
      
      ((emtb:regex-comparison-data-p comparison)
	 ;;For now, just show a buffer
	 (with-current-buffer (generate-new-buffer "regex compare")
	    (insert "Comparing vs a regex\n")
	    (insert "\n-----------------------------------\n")
	    (insert "Expected\n")
	    (insert 
	       (emtb:regex-comparison-data-pattern-expected
		  comparison))
	    (insert "\n-----------------------------------\nGot\n")
	    (insert 
	       (emtb:regex-comparison-data-got
		  comparison))
	    ))))


;;;_   , Tests
;;Has to be tested interactively
'
(let
   ((emtb:last-bad-comparison 
       (make-emtb:direct-comparison-data 
	  :got "abc\ndef\nghi"
	  :expected "abc\nabf\nghi")))
   (emtb:ediff-last-comparison emtb:last-bad-comparison))


;;;_  . emtb:buf-contents-matches

;;Key `:object' was replaced by key `:sexp'
;;;###autoload
(defun* emtb:buf-contents-matches (&rest args &key file dir string
				     sexp buf
				     regex-marks validate-re)
   "Return non-nil just if the buffer's contents match what is given.

What is given can be:
	:file a filename.
	:string a string
	:sexp an object represented as a sexp"
   
   (let*
      (
	 (buf-in
	     (or buf (current-buffer)))
	 (str
	    (with-current-buffer buf-in
	       (buffer-string))))
      (cond
	 ((or file string)
	    (emtb:string-matches
	       str
	       (apply #'emtb:contents nil args)
	       regex-marks
	       (if regex-marks
		  (mapcar
		     #'(lambda (val)
			  (apply #'emtb:contents dir val))
		     validate-re))))
	 
	 (sexp
	    (equal
	       sexp
	       (with-current-buffer buf-in
		  (save-excursion
		     (emtb:buffer-object))))))))

;;;_  . emtb:extract-regexp

(defun emtb:extract-regexp (pattern-str foremark aftermark)
   "Extract the regexp specified by the string.
Sections not between regex-marks are treated as literal matches."

   (let
      (
	 ;;The collected parts will become a regex bounded by
	 ;;"^...$"
	 (sub-expressions '("^"))
	 (position 0))
      (macrolet
	 ((push-rx (piece)
	     `(push ,piece sub-expressions))
	    (push-literal-match (start end)
	       `(push-rx
		   (regexp-quote
		      (substring pattern-str ,start ,end)))))
	 (while
	    ;;Search for beginning mark
	    (string-match (regexp-quote foremark) pattern-str position)
	    (let
	       ((beg-foremark
		   (match-beginning 0))
		  (end-foremark
		     (match-end 0)))
	       ;;Any matching string first must match the string up to
	       ;;that point
	       (push-literal-match position beg-foremark)
	       ;;Search for aftermark.  
	       (if
		  (string-match (regexp-quote aftermark) pattern-str end-foremark)
		  (let
		     ((beg-aftermark
			 (match-beginning 0))
			(end-aftermark
			   (match-end 0)))
		     ;;The fore-mark must not occur again before the
		     ;;aftermark.  
		     (when
			(and
			   (string-match (regexp-quote foremark) pattern-str
			      end-foremark)
			   (<= (match-beginning 0) beg-aftermark))
			(error 
			   "Nested regexps are not supported"))
		     
		     ;;Any matching string must then match the
		     ;;internal part as a regex.
		     (push-rx
			(substring pattern-str end-foremark beg-aftermark))
		     (setq position end-aftermark))
		  (error "Unterminated regex section, beginning %s" 
		     beg-foremark))))
	 ;;Any matching string must then match the remainder of the
	 ;;string literally
	 (push-literal-match position nil)
	 (push "$" sub-expressions))
      (apply #'concat
	 (reverse sub-expressions))))

;;;_  . emtb:string-matches-re

(defun* emtb:string-matches-re
   (string-got expected validators foremark aftermark)
   ""
   (let
      ((regexp (emtb:extract-regexp expected foremark aftermark)))
      (dolist (validation-string validators)
	 (unless
	    (string-match regexp validation-string)
	    (error "Regular expression failed validation")))
      
      (if
	 (string-match regexp string-got)
	 t)))

;;;_  . emtb:string-matches

(defun emtb:string-matches (string-got expected &optional
				 regex-marks validators)
   ""
      
   (if
      regex-marks
      (progn
	 (unless
	    (consp regex-marks)
	    (error "regex-marks needs to be a list"))
	 (if
	    (apply #'emtb:string-matches-re string-got expected
	       validators regex-marks)
	    t
	    (progn
	       (setq emtb:last-bad-comparison
		  (make-emtb:regex-comparison-data
		     :got string-got
		     :pattern-expected expected))
	       nil)))
      
      (if
	 (string-equal string-got expected)
	 t
	 (progn
	    (setq emtb:last-bad-comparison
	       (make-emtb:direct-comparison-data
		  :got string-got
		  :expected expected))
	    nil))))


;;;_   , emtb:buffer-object

;;;###autoload
(defun emtb:buffer-object () 
   "Return the object, if any, described by current buffer"
   (goto-char (point-min))
   (read (current-buffer)))


;;;_: Footers
;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + mode: allout
;;;_  + outline-regexp: ";;;_ *"
;;;_  + End:

;;;_ , Provide

(provide 'emtest/testhelp/mocks/filebuf)
;;; emtest/testhelp/mocks/filebuf.el ends here
