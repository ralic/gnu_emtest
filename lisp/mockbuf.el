;;;_ mockbuf.el --- Test helper: Buffer visiting file, point controlled

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
;;mockbuf:files-match, mockbuf:buf-is-file-at, mockbuf:buf-is-file.

;; When we do want saving to go somewhere else, that should be
;;effected by giving further args to `mockbuf:cautious-load-file'.  A
;;tmp-dir argument computed by mockbuf itself?  Used just if
;;`with-buffer-containing-object' gets an additional argument.  And
;;(actually found this needed earlier) sometimes we need the visited
;;filename for informational purposes.



;;;_. Headers

;;;_ , Requires

(require 'cl)
(require 'emt-accumulator)
(require 'rtest-util)  ;;For testing, IIRC
(rtest:if-avail
   ;;Only a few examples use emt:eg.  Most or all should.
   (require 'eg)

   )
;;;_ , Test config

(defconst mockbuf:th-examples-dir
   (rtest:expand-filename-by-load-file "examples/mockbuf") 
   "" )

;;;_ , Config

(defcustom mockbuf:slave-root
   "/tmp/"
   "The directory where mockbuf is to place temporary files.
You may want to use a ramdisk if available"
   :type 'file)

(defcustom mockbuf:slave-init-hook
   nil
   "Function(s) to call before using `mockbuf:slave-root'.
You may want to use this to mount a ramdisk" 
   :type 'hook)

(defcustom mockbuf:slave-term-hook
   nil
   "Function(s) to call after using `mockbuf:slave-root'.
You may want to use this to umount a ramdisk" 
   :type 'hook)

;;;_: Code:
;;;_ , Test data

;;;_  . Master/slave files

(emt:eg:define xmp:07298be7-85b8-414b-b9ae-e3a1edde2e07
   ((project emtest)(library mockbuf))
   (item
      ((role master)(type filename))
      (expand-file-name "file3" mockbuf:th-examples-dir))
   ;;This item may go away as we automatically make tmp.
   (item
      ((role slave)(type filename))
      (expand-file-name "slave-dir/file3" mockbuf:th-examples-dir)))


;;;_ , Virtual directory
;;Deprecated

;;;_  . Structures

(defstruct 
   (mockbuf:virtual-dir
      (:constructor make-mockbuf:virtual-dir
	 (root 
	    &key 
	    dir file-basenames mutable
	    &aux 
	    (dir-2 (expand-file-name dir root))
	    (abs-dir
	       (if
		  mutable
		  (progn
		     (assert (not (file-name-absolute-p dir)))
		     (assert (not
				(equal
				   root
				   mockbuf:slave-root)))
		     (expand-file-name dir mockbuf:slave-root))
		  dir-2))
	    (abs-master-dir
	       (if
		  mutable
		  dir-2
		  nil)))
	 ))
   
   
   "Example files"
   abs-dir
   abs-master-dir
   file-basenames)

;;;_   , Tests (of the ctor)

(rtest:deftest make-mockbuf:virtual-dir

   ;;Lest master and slave be the same filename:

   ("Disallow an absolute DIR argument"
      (rtest:gives-error
	 (make-mockbuf:virtual-dir 
	    mockbuf:virtual-dir:th:example-root
	    :mutable t 
	    :dir "/foo")))
   
   ("Disallow root = mockbuf:slave-root."
      (rtest:gives-error
	 (make-mockbuf:virtual-dir 
	    mockbuf:slave-root 
	    :mutable t 
	    :dir ".")))

   ;;YAGNI: Avoid collision between the same name in different
   ;;projects, but that's unlikely to occur within the same test,
   ;;which is the only time it would matter.

   )


;;;_  . Interactive help creating it

(require 'handhold)


;;;_   , Test data

(rtest:if-avail
   (defconst mockbuf:virtual-dir:ctor-arglist
      '(mockbuf:virtual-dir:th:example-root
	  :dir
	  "bar" 
	  :file-basenames
	  '("1.txt" "2.txt"))
      "An example arglist to `make-mockbuf:virtual-dir'")

   (defconst mockbuf:virtual-dir:ctor-widget-value
      '(mockbuf:virtual-dir:th:example-root
	  "bar" 
	  ;;"" ;;For now, master-dir still exists as a string.
	  ("1.txt" "2.txt"))
      "An example widget-value corresponding to
`mockbuf:virtual-dir:ctor-arglist'"))

;;;_   , make-mockbuf:virtual-dir:sexp->widget-value

;;This could be derived from the ctor definition and the widget
;;definition.


;;Check-type only seems to accept symbols, not lists, so we spread the
;;type check of file-basenames over several calls.
(defun* make-mockbuf:virtual-dir:sexp->widget-value 
   (root &key dir 
      ;;master-dir 
      file-basenames)
   ""
   (check-type root symbol)
   (check-type dir string)
   ;;master-dir is deprecated.
   (check-type file-basenames cons)
   (check-type (first file-basenames) (member quote))
   (check-type (second file-basenames) list)
   (list root dir 
      ;;"this-is-deprecated" 
      (eval file-basenames)))

;;;_    . Tests

(rtest:deftest make-mockbuf:virtual-dir:sexp->widget-value
   
   (  "Situation: Normal.  
ROOT is a symbol.
DIR is a string.
FILE-BASENAMES is a quoted list. 
Response: Accepts it."
      (not
	 (rtest:gives-error
	    (make-mockbuf:virtual-dir:sexp->widget-value
	       'mockbuf:virtual-dir:th:example-root
	       :dir 
	       '"bar" 
	       :file-basenames
	       ''("1.txt" "2.txt")))))

   ("Accepts the example"
      (not
	 (rtest:gives-error
	    (apply #'make-mockbuf:virtual-dir:sexp->widget-value
	       mockbuf:virtual-dir:ctor-arglist))))

   ;;We'd like to test that it transforms into
   ;;`mockbuf:virtual-dir:ctor-widget-value', but as long as we're
   ;;still handling master-dir, it won't (won't fairly, anyways).
   
   (
      "Situation: Root is not a symbol"
      (rtest:gives-error
	 (make-mockbuf:virtual-dir:sexp->widget-value
	    mockbuf:virtual-dir:th:example-root
	    :dir 
	    '"bar" 
	    :file-basenames
	    ''("1.txt" "2.txt"))))
   
   
   (  "Situation: Dir is not a string"
      (rtest:gives-error
	 (make-mockbuf:virtual-dir:sexp->widget-value
	    'mockbuf:virtual-dir:th:example-root
	    :dir 
	    'mockbuf:virtual-dir:th:example-root
	    :file-basenames
	    ''("1.txt" "2.txt"))))
   
   ("Situation: File basenames is not a quoted list"
      (rtest:gives-error
	 (make-mockbuf:virtual-dir:sexp->widget-value
	    'mockbuf:virtual-dir:th:example-root
	    :dir 
	    '"bar" 
	    :file-basenames
	    'mockbuf:virtual-dir:th:example-root)))
   )

;;;_   , make-mockbuf:virtual-dir:widget-value->sexp

;;This could be trivially derived from the ctor definition and the
;;widget definition.
(defun make-mockbuf:virtual-dir:widget-value->sexp
   (root dir file-basenames) 
   ""
   `(make-mockbuf:virtual-dir
       ,root
       ,@(if dir
	    (list ':dir dir))
;;        ,@(if master-dir
;; 	    (list ':master-dir master-dir))
       ,@(if file-basenames
	    (list ':file-basenames `'(,@file-basenames)))))

;;;_    . Tests

(rtest:deftest make-mockbuf:virtual-dir:widget-value->sexp

   ;;This can't just be tested by simple comparison, because the forms
   ;;might mean the same but differ in particulars.  Instead we
   ;;compare values that have both been transformed - any particulars
   ;;ought to "fall the same way" both times.
   ("Shows: `make-mockbuf:virtual-dir:widget-value->sexp' reverses
`make-mockbuf:virtual-dir:sexp->widget-value'.  Both are to be applied
to forms."

      (let 
	 ((wid-value
	     (apply
		#'make-mockbuf:virtual-dir:sexp->widget-value
		mockbuf:virtual-dir:ctor-arglist)))
	 (equal
	    wid-value
	    (apply
	       #'make-mockbuf:virtual-dir:sexp->widget-value
	       (cdr
		  (apply 
		     #'make-mockbuf:virtual-dir:widget-value->sexp
		     wid-value)))))))

;;;_   , mockbuf:virtual-dir:widget-type


(defconst mockbuf:virtual-dir:widget-type
   '(list
       :tag "Ctor call to make a virtual directory"
       ;;A symbol that refers to a string.  
       (symbol :tag "Root directory symbol"
	  :match 
	  (lambda (widget value)
	     (and
		(symbolp value)
		(if (boundp value)
		   (stringp (symbol-value value)))))
 	  :complete-function
 	  (lambda ()
 	     (interactive)
 	     (lisp-complete-symbol
		#'(lambda (x)
		     (if (boundp x)
			(stringp (symbol-value x)))))))
       
       (directory :tag "Directory relative to project root"
	  :complete-function
	  (lambda ()
	     (interactive)
	     (let*
		(  (widget
		      (widget-field-find (point)))
		   (directory 
		      (symbol-value 
			 (car 
			    (tehom-widget-nth-parent-value 1)))))
		(tehom-widget-filename-complete widget directory))))

       (repeat 
	  :tag "List of relative file names" 
	  (file
	     :complete-function
	     (lambda ()
		(interactive)
		(let*
		   (  (widget
			 (widget-field-find (point)))
		      (value
			 (tehom-widget-nth-parent-value 2)))
		   
		   (tehom-widget-filename-complete 
		      widget 
		      (expand-file-name
			 (second value)
			 (symbol-value (car value)))))))))
   "" )
;;;_    @ decisions/7 Describing these objects easier 
;;;_    . Tests

;;Tested thru handhold (And only informally tested even there)

;;;_   , `make-mockbuf:virtual-dir' properties

;;Format is alpha, might change drastically.
(put 'make-mockbuf:virtual-dir 'handhold:data
   (list
      mockbuf:virtual-dir:widget-type
      #'make-mockbuf:virtual-dir:widget-value->sexp
      #'make-mockbuf:virtual-dir:sexp->widget-value))

;;;_  . Test setup

(defconst mockbuf:virtual-dir:th:example-root 
   mockbuf:th-examples-dir
   "Dummy symbol referring to the root directory of a dummy project" )
'  ;;Obsolete.
(defmacro mockbuf:virtual-dir:th:usuals-x (&rest body)
   ""
   
   `(symbol-macrolet
       (
	  (filesthing-list 
	     mockbuf:virtual-dir:thd:example-multi)
	  )
       ,@body))

;;;_  . Example objects

(defconst mockbuf:virtual-dir:thd:example-simple
   (make-mockbuf:virtual-dir
      mockbuf:virtual-dir:th:example-root
      :dir "bar" 
      :file-basenames
      '("1.txt" "2.txt"))
   "Example virtual directory, simple case" )

(defconst mockbuf:virtual-dir:thd:example-slaved
   (make-mockbuf:virtual-dir
      mockbuf:th-examples-dir
      :dir "bar"
      :mutable t
      :file-basenames
      '("3.txt"))
   "Example slaved files" )

(defconst mockbuf:virtual-dir:thd:example-multi
   (list
      mockbuf:virtual-dir:thd:example-simple
      mockbuf:virtual-dir:thd:example-slaved
      (make-mockbuf:virtual-dir
	 mockbuf:virtual-dir:th:example-root
	 :mutable t
	 :dir "bar/single-master/"
	 :file-basenames
	 '("alone.txt")))

   
   "Example virtual directory" )

;;;_  . mockbuf:virtual-dir:get-match

(defun mockbuf:virtual-dir:get-match (basename filesthing-list)
   ""
   (unless (listp filesthing-list)
      (if
	 (mockbuf:virtual-dir-p filesthing-list)
	 (error "Arg Filesthing-List should be a list of filesthing,
not a single filesthing..")
	 (error "Arg filesthing-list is not a list")))
   
   (find basename filesthing-list 
      :test 
      #'(lambda (key el)
	   (member key 
	      (mockbuf:virtual-dir-file-basenames el)))))

;;;_   , Tests

(rtest:deftest mockbuf:virtual-dir:get-match

   ("Situation: name is one of the names in first object.  
Response: Return first object"
      (equal
	 (mockbuf:virtual-dir:get-match 
	    "1.txt" 
	    mockbuf:virtual-dir:thd:example-multi)
	 mockbuf:virtual-dir:thd:example-simple))
   
   ("Situation: name is one of the names in second object.  
Response: Return second object"

      (equal
	 (mockbuf:virtual-dir:get-match 
	    "3.txt" 
	    mockbuf:virtual-dir:thd:example-multi)
	 mockbuf:virtual-dir:thd:example-slaved))

   ("Situation: name is not one of the names.  
Response: Return nil"
      (not
	 (mockbuf:virtual-dir:get-match 
	    "nomatch.txt" 
	    mockbuf:virtual-dir:thd:example-multi))))


;;;_  . mockbuf:virtual-dir:suspected-list->singleton
(defun mockbuf:virtual-dir:suspected-list->singleton (basename maybe-list)
   ""
   
   (if
      (listp maybe-list)
      (mockbuf:virtual-dir:get-match basename maybe-list)
      maybe-list))

;;;_   , Tests

(put 'mockbuf:virtual-dir:suspected-list->singleton
   'rtest:test-thru
   'mockbuf:virtual-dir:base->filename)

;;Also thru mockbuf:virtual-dir:base->master-name 

;;;_  . -> true filenames

;;;_   , mockbuf:virtual-dir:base->filename

(defun mockbuf:virtual-dir:base->filename (basename filesthing)
   "Return the absolute filename corresponding to BASENAME.
FILESTHING is a filesthing"
   (expand-file-name 
      basename 
      (mockbuf:virtual-dir-abs-dir 
	 (mockbuf:virtual-dir:suspected-list->singleton 
	    basename 
	    filesthing))))

;;;_    . Tests

(rtest:deftest mockbuf:virtual-dir:base->filename

   ("Situation: Given basename is one of the basenames.
Response: Returns a filename expanded vs both root and dir"

      (equal
	 (mockbuf:virtual-dir:base->filename "1.txt"
	    mockbuf:virtual-dir:thd:example-simple)
	 (expand-file-name "1.txt"
	    (expand-file-name "bar" 
	       mockbuf:virtual-dir:th:example-root))))


   ;;YAGNI: Situation: Basename is not one of the basenames.

   )

;;;_   , mockbuf:virtual-dir:base->master-name

(defun mockbuf:virtual-dir:base->master-name (basename filesthing)
   ""
   
   (let
      ((master-dir (mockbuf:virtual-dir-abs-master-dir 
		      (mockbuf:virtual-dir:suspected-list->singleton
			 basename
			 filesthing))))
      (if
	 (null master-dir)
	 (error "Basename %s is not slaved" basename)
	 (expand-file-name basename master-dir))))


;;;_    . Tests
(rtest:deftest mockbuf:virtual-dir:base->master-name
   ("Situation: Given basename is one of the basenames and filesthing
is slaved.
Response: Returns a filename expanded vs both root and master dir"

      (equal
	 (mockbuf:virtual-dir:base->master-name "3.txt"
	    mockbuf:virtual-dir:thd:example-slaved)
	 (expand-file-name "3.txt"
	    (expand-file-name "bar" 
	       mockbuf:virtual-dir:th:example-root))))

   
   ;;YAGNI: Situation: filesthing is not slaved.
   
   )


;;;_ , Setup file

;;;_  . Master/slave copying

;;;_   , Test data

(defconst mockbuf:thd:a-string 
   "Some text that won't match real file"
   "Example text for writing into files for tests" )

;;;_   , Test helper

(defmacro* mockbuf:th:let-slave-master-pair
   ((slave-name master-name basename virtual-dir) &rest body)
   "`let' the slave and master filenames around BODY.
If MASTER-NAME is nil, no master filename object is created"
   
   `(let
       (  
	  (,slave-name
	     (mockbuf:virtual-dir:base->filename 
		,basename
		,virtual-dir))
	  ,@(if master-name
	       `((,master-name
		    (mockbuf:virtual-dir:base->master-name
		       ,basename
		       ,virtual-dir)))
	       ()))
       ,@body))


;;;_   , mockbuf:fresh-copy

(defun mockbuf:fresh-copy (slave master)
   "Make a fresh copy from the master file"
   (make-directory (file-name-directory slave) t)
   (let 
      ((buf (get-file-buffer slave)))
      (when buf
	 ;;To avoid interaction.
	 (with-current-buffer buf
	    (set-buffer-modified-p nil))
	 (kill-buffer buf)))
   (copy-file master slave t))

;;;_    . Tests

;;Also tested thru mockbuf:virtual-dir:constantize.

(rtest:deftest mockbuf:fresh-copy
   ;;NOT AUTOMATABLE. The test here is that no user interaction
   ;;happens.  There is no simple way of checking that directly.
   ("Situation: Buffer already exists and has been modified.  
Response: Even so, there is no interaction.
IF THERE WAS USER INTERACTION, this test may have failed."

      (mockbuf:th:let-slave-master-pair
	 (file3-slave file3-master	 
	    "3.txt" 
	    mockbuf:virtual-dir:thd:example-slaved)

	 (with-current-buffer
	    (find-file-noselect file3-slave)
	    (insert mockbuf:thd:a-string))
	 (mockbuf:fresh-copy file3-slave file3-master)
	 t))

   ("Situation: slave-file doesn't exist and its parent directory
doesn't exist.
Response: Still opens without a problem"
      (mockbuf:th:let-slave-master-pair
	 (filename-slave filename-master 
	    "alone.txt"
	    mockbuf:virtual-dir:thd:example-multi)

	 ;;Delete that file if it exists.
	 (if (file-exists-p filename-slave)
	    (delete-file filename-slave))
	 
	 ;;Delete that directory if it exists (and if it seems safe)
	 (let
	    ((dir (file-name-directory filename-slave)))

	    (when (file-exists-p dir)
	       ;;Check that it contains no files now, other than ".."
	       ;;and "."
	       (unless
		  (= 
		     (length (directory-files dir nil "[^\.]+" t))
		     0)
		  (error "Other files are in directory so maybe we don't own it"))
	       (delete-directory dir)))

	 (not
	    (rtest:gives-error
	       (mockbuf:fresh-copy 
		  filename-slave
		  filename-master)))))

   )


;;;_   , mockbuf:virtual-dir:constantize

(defun mockbuf:virtual-dir:constantize (basename filesthing)
   ""
   (let
      ((filename
	  (mockbuf:virtual-dir:base->filename basename filesthing)))
      
      (if
	 ;;If there is a master dir...
	 (mockbuf:virtual-dir-abs-master-dir filesthing)
	 ;;Get a fresh copy.
	 (mockbuf:fresh-copy
	    filename
	    (mockbuf:virtual-dir:base->master-name basename
	       filesthing))
	 ;;Otherwise make buffer read-only.  Rationale: A test file
	 ;;shouldn't have other uses, and user can M-x
	 ;;toggle-read-only to modify it.
	 (let
	    ((buf (get-file-buffer filename)))
	    (when buf
	       (with-current-buffer buf
		  (setq buffer-read-only t)))))))

;;;_    . Tests

;;Also tested thru mockbuf:find-file-2

(rtest:deftest mockbuf:virtual-dir:constantize
   
   ("A slaved file is replaced by the master copy."
   
      (let
	 ((virtual-dir mockbuf:virtual-dir:thd:example-slaved))
	 (mockbuf:th:let-slave-master-pair
	    (slave master "3.txt" virtual-dir)
	    (with-temp-file slave
	       (insert mockbuf:thd:a-string))
	    (mockbuf:virtual-dir:constantize "3.txt" virtual-dir)
	    (mockbuf:files-match master slave))))

   
   ("A non-slaved file remains as it is."
      (let
	 ((virtual-dir mockbuf:virtual-dir:thd:example-simple))
	 (mockbuf:th:let-slave-master-pair
	    (file nil "1.txt" virtual-dir)
	    (with-temp-file file
	       (insert mockbuf:thd:a-string))
	    (mockbuf:virtual-dir:constantize "1.txt" virtual-dir)
	    (let
	       ((slave-str
		   (with-temp-buffer
		      (insert-file-contents file)
		      (buffer-string))))
	       (string-equal slave-str mockbuf:thd:a-string)))))

   ;;It's direct.
   '("Situation: Not slaved. buffer is not read-only.  Afterwards:
buffer is read-only."  
       
       )

   )
;;;_  . Buffer containing known contents
;;;_   , Formdata structure mockbuf:bufcontaining:formdata

;;;_   , with-buffer-containing-object

(defmacro with-buffer-containing-object (args &rest body)
   ""
   (apply #'with-buffer-containing-buildform body args))

;;;_    . Worker `with-buffer-containing-buildform'

;;Key `:printed-object' only exists for historical reasons.  Use
;;`:sexp' instead.
;;Add `sequence', which appends all of these and allows "point" and
;;"(mark NAME)" (Saved in buffer-local var mockbuf:named-marks)
(defun* with-buffer-containing-buildform 
   (body &key printed-object sexp string file dir visited-name
      point-replaces
      sequence)
   ""

   `
   (with-temp-buffer
      ;;Do all the inserting
      ,(cond
	  ((or sexp printed-object)
	     (mockbuf:build-insertform dir 'sexp (or sexp printed-object)))
	  (file
	     (mockbuf:build-insertform dir 'file file))
	  (sequence
	     (mockbuf:build-insertform dir 'sequence sequence))
	  (t 
	     (mockbuf:build-insertform dir 'string string)))

      ;;Set up file, if reasonable.
      ,(when (or visited-name file)
	  `(mockbuf:cautious-setup-file 
	      ,visited-name 
	      ,(or dir
		  `(file-name-directory ,file))))
      

      ,(if point-replaces
	  `(mockbuf:goto-text ,point-replaces t)
	  '(goto-char (point-min)))
      
      ,@body))

(put 'with-buffer-containing-buildform 'rtest:test-thru
   'with-buffer-containing-object)

;;;_   , Worker mockbuf:build-insertform
;;DIR may become a general data element.
(defun mockbuf:build-insertform (dir governor &rest args)
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
		 `(mockbuf:cautious-insert-file 
		     (expand-file-name ,file ,dir) 
		     t)))
	 
	 (sequence
	    #'(lambda (sequence)
		 `(progn
		     ,@(mapcar
			  #'(lambda (x)
			       (apply #'mockbuf:build-insertform 
				  dir
				  x))
			  sequence))))

	 ;;$$Add (mark name &optional insert-before)
	 ;;$$Add point
	 
	 )
      args))

;;;_    . Tests

(rtest:deftest with-buffer-containing-object
   ("Param: :string STRING is given
Reaction: Fill the buffer with exactly STRING"
      (with-buffer-containing-object (:string "abc")
	 (equal
	    (buffer-string)
	    "abc")))
   ;;$$The key read-final is obsolete; use mockbuf:buffer-object
   ;;directly
   ;;Removed :dir-root in favor of :dir

   ("Param: :sexp OBJECT is given.
Reaction: Fill the buffer with exactly a printed representation of
OBJECT"
      (equal
	 (with-buffer-containing-object 
	    (:sexp '(1 5))
	    (mockbuf:buffer-object))
	 '(1 5)))
   
   ("Param: :point-replaces STR is given.
Reaction: The first occurence of STR is deleted "
      (with-buffer-containing-object 
	 (:string "abcdef" :point-replaces "cd")
	 (equal
	    (buffer-string)
	    "abef")))

   ("Param: :point-replaces STR is given.
Reaction: Point is now where STR was "
      (with-buffer-containing-object 
	 (:string "abcdef" :point-replaces "cd")
	 (looking-at "ef$")))
   
   ("Shows: :point-replaces can cause parts of an object to be unprinted."
      (equal
	 (with-buffer-containing-object 
	    (:sexp '(ab cd ef)
	       :point-replaces "cd")
	    (mockbuf:buffer-object))
	 '(ab ef))
      )

   ("Restores the original buffer even in case of error."
      (let
	 ((win
	     (current-window-configuration)))
	    
	 (ignore
	    (rtest-one-probe-0
	       '((with-buffer-containing-object (:string "abc")
		    (error "A deliberate error")))))
	 (equal 
	    win
	    (current-window-configuration))))

   ;;YAGNI: Also mark-replaces and markers-replace (which takes and
   ;;sets an alist of markers, so they can be known by name).  


   ;;Exercise the `visited-name' feature.

   (  "Exercise the `visited-name' feature."

      (emt:eg:narrow ((project emtest)(library mockbuf))
	 (let 
	    (  (master-file (emt:eg (role master)(type filename)))
	       (slave-file  (emt:eg (role slave)(type filename))))
	    ;;Remove the slave file if it already exists.
	    (when (file-exists-p slave-file)
	       (delete-file slave-file))
	    ;;Visit slave-file, but with contents from master-file.
	    (with-buffer-containing-object 
	       (:file master-file
		  :visited-name slave-file)
	       ;;Save the file (and don't back it up)
	       (save-buffer 0))
	    ;;The files' contents match
	    (mockbuf:files-match master-file slave-file))))
   

   (  "Situation: The slave file already exists.
Response: Works the same."
      (emt:eg:narrow ((project emtest)(library mockbuf))
	 (let 
	    (  (master-file (emt:eg (role master)(type filename)))
	       (slave-file  (emt:eg (role slave)(type filename))))
	    ;;Create the slave file if it doesn't exist
	    (unless (file-exists-p slave-file)
	       (with-temp-buffer
		  (insert "Some unmatched text")
		  (write-file slave-file nil)))
	    ;;Visit slave-file, but with contents from master-file.
	    (with-buffer-containing-object 
	       (:file master-file
		  :visited-name slave-file)
	       ;;Save the file (and don't back it up)
	       (save-buffer 0))
	    ;;The files' contents match
	    (mockbuf:files-match master-file slave-file))))


   (  "Param: the symbol `tmp' is given as `:visited-name'.
Behavior: Creates a temporary file."

      (emt:eg:narrow ((project emtest)(library mockbuf))
	 (let 
	    (  slave-file
	       (master-file (emt:eg (role master)(type filename))))
	    ;;Visit slave-file, but with contents from master-file.
	    (with-buffer-containing-object 
	       (:file master-file
		  :visited-name 'tmp)
	       ;;Get its name.
	       (setq slave-file (buffer-file-name))
	       ;;Save the file (and don't back it up)
	       (save-buffer 0))
	    ;;The files' contents match
	    (mockbuf:files-match master-file slave-file))))


   (  "Shows: `sequence' works.
Param: sequence with two strings
Response: Contents of buffer are as expected."
      (with-buffer-containing-object 
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

;;;_  . File containing known contents
(defmacro with-file-containing (args var &rest body)
   ""
   (let
      ((temp-file-sym (make-symbol "temp-file")))
      `(let*
	  (  
	     (--absent
		(apply #'mockbuf:absent-flag ',args))
	     (,temp-file-sym 
		(if --absent
		   (make-temp-name ,mockbuf:slave-root)
		   (make-temp-file ,mockbuf:slave-root)))
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

;;;_   , Helper mockbuf:absent-flag
(defun* mockbuf:absent-flag (&rest dummy &key absent &allow-other-keys)
   ""
   absent)


;;;_   , Tests

(rtest:deftest with-file-containing
   (  "Proves: Body form is run - it changes the value of `x'."
      (let
	 ((str "abc def ghi")
	    x)
	 (with-file-containing (:string str)
	    filename
	    (setq x 12))
	 (assert (equal x 12) t)
	 t))

   (  "Proves: The filename is absolute."
      (let
	 ((str "abc def ghi"))
	 (with-file-containing (:string str)
	    filename
	    (assert
	       (file-name-absolute-p filename)
	       t))
	 t))

   (  "Proves: Th file contents match the expected contents."
      (let
	 ((str "abc def ghi")
	    x)
	 (with-file-containing (:string str)
	    filename
	    (assert
	       (string=
		  (mockbuf:file-contents-absname filename)
		  str)
	       t))
	 t))

   (  "Proves: `with-file-containing' returns the result of running
BODY."
      (let
	 ((str "abc def ghi"))
	 (assert
	    (equal
	       (with-file-containing (:string str)
		  filename
		  12)
	       12)
	    t)
	 t))
   
   (  "Proves: After the body has finished running, the file is gone."
      (let
	 (filename-kept)
	 (with-file-containing (:string "abc def ghi") filename
	    (setq filename-kept filename))
	 (assert (not (file-exists-p filename-kept)) t)
	 t))


   ("Param: The flag `:absent' is given and is non-nil.
No file is made."
      (with-file-containing (:absent t) filename
	 (assert (not (file-exists-p filename)) t)
	 t))
   

      ("Param: The flag `:absent' is given and is non-nil.
Situation: That file is created by body.
Response: After body has run, file no longer exists."
	 (let
	    (filename-kept)
	    (with-file-containing (:absent t) filename
	       (setq filename-kept filename)
	       (with-temp-buffer
		  (insert "Some stuff")
		  (write-file filename)))
	    (assert (not (file-exists-p filename-kept)) t)
	    t))
   
   )

;;;_  . Find file
;;;_   , mockbuf:goto-mark (Not written yet and YAGNI)

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


;;;_   , mockbuf:goto-text

;;$$Want to do this differently:  `mockbuf:goto-mark' will be another
;;function.  Any replacement will be done by setting it for it; this
;;no longer will do replacement.
(defun mockbuf:goto-text (loc-string &optional replace)
   ""

   (goto-char (point-min))
   (search-forward loc-string)
   (if 
      replace
      (replace-match "")
      (goto-char (match-beginning 0))))

;;;_   , mockbuf:cautious-find-file

;;Obsolescent
(defun mockbuf:cautious-find-file (filename)
   ""

   (unless
      (file-name-absolute-p filename)
      (error "Filename %s is not absolute" filename))

   (unless
      (file-exists-p filename)
      (error "File %s doesn't exist" filename))

   (find-file filename))
;;;_   , mockbuf:cautious-insert-file
(defun mockbuf:cautious-insert-file (filename &optional visit beg end replace)
   ""

   (unless
      (file-name-absolute-p filename)
      (error "Filename %s is not absolute" filename))

   (unless
      (file-exists-p filename)
      (error "File %s doesn't exist" filename))

   (unless
      (= (buffer-size) 0)
      (error "Buffer is not empty"))
   
   (insert-file-contents filename visit beg end replace))

;;;_    . Tests
(put 'mockbuf:cautious-insert-file 'rtest:test-thru
   'with-buffer-containing-object)

;;We're enforcing never inserting into a full buffer.  Alternatively,
;;this could be left to `insert-file-contents', which will error if
;;buffer is non-empty and visit is non-nil.  TBD.
;;;_   , mockbuf:cautious-setup-file
(defun mockbuf:cautious-setup-file (visited-name dir)
   ""

   (after-find-file)
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
	    (make-temp-file mockbuf:slave-root) t nil))
      (t
	 (let
	    ((abs-name
		(if
		   (file-name-absolute-p visited-name)
		   visited-name
		   (expand-file-name visited-name dir))))
	    (set-visited-file-name abs-name t nil)))))
;;;_   , mockbuf:cautious-load-file

;;Replaced mockbuf:cautious-find-file and will help replace the
;;find-file thing.
;;Obsolescent: Now we call its pieces at different times.
(defun mockbuf:cautious-load-file (filename visited-name)
   ""

   ;;`mockbuf:cautious-insert-file' will error if buffer is non-empty,
   ;;so we don't have to provide that.

   ;;Have to set VISITED arg so that within
   ;;`mockbuf:cautious-setup-file', `after-find-file' sees a filename
   ;;as it expects to.
   (mockbuf:cautious-insert-file filename t)
   (mockbuf:cautious-setup-file 
      visited-name 
      (file-name-directory filename)))


;; If caller wants to test saving files - perhaps for code that
;; creates a project directory, or to test that some file can be read
;; back - caller could set a name by passing it in `visited-name'.

;;;_    . Tests

(put 'mockbuf:cautious-load-file 'rtest:test-thru
   'with-buffer-containing-object)


;;;_   , mockbuf:find-file-goto-text

;;Deprecated.  This has been split into `mockbuf:cautious-find-file'
;;and the part that manages loc-string and (now) prepare-f

;;$$Prepare to replace this.  Several tests use it and
;;mockbuf:find-file-2 uses it with no options.

(defun mockbuf:find-file-goto-text (filename &optional loc-string)
   ""
   '
   (unless
      (file-name-absolute-p filename)
      (error "Filename %s is not absolute" filename))
   '
   (unless
      (file-exists-p filename)
      (error "File %s doesn't exist" filename))

   (let
      ((buf (mockbuf:cautious-find-file filename)))
      (with-current-buffer buf
	 ;;Almost replaceable.
	 (if loc-string
	    (mockbuf:goto-text loc-string)
	    (goto-char (point-min))))
      
      buf))

;;;_    . Tests

(rtest:deftest mockbuf:find-file-goto-text
   ("Situation: Filename is not absolute.
Response: Error."
      (rtest:gives-error (mockbuf:find-file-goto-text ".")))
   )

;;;_   , mockbuf:find-file

;;mockbuf:find-file got obsolete.

;;$$Change callers to call thru mockbuf:find-file-3.  Then remove
;;loc-string param and its related code.
(defun mockbuf:find-file-2 (filesthing-0 basename &optional loc-string)
   ""
	 
   (let*
      (
	 (filesthing 
	    (mockbuf:virtual-dir:suspected-list->singleton
	       basename
	       filesthing-0))

	 (filename
	    (mockbuf:virtual-dir:base->filename basename filesthing)))
      
      (mockbuf:virtual-dir:constantize basename filesthing)
      (mockbuf:find-file-goto-text filename loc-string)))


;;;_    . Tests

;;Tested thru mockbuf:make-form-find-file, but that's increasingly
;;less adequate.

;;Tested thru ade:deactivate-siblings.

;;;_   , mockbuf:find-file-3

;;$$Rename this `mockbuf:find-file'
;;;###autoload
(defun* mockbuf:find-file-3 (context filebase &key at-str prepare-f) 
   "Find file FILEBASE, as expanded in filecontext CONTEXT.

If PREPARE-F is given, call it with no args.
If AT-STR is given, find it in the buffer and put point at the
beginning of it."
   
   (mockbuf:find-file-2 context filebase)
   (when prepare-f (funcall prepare-f))
   (if at-str 
      (mockbuf:goto-text at-str)
      (goto-char (point-min))))

;;;_    . Tests

;;Tested thru `allout-tehom:operate-on-siblings'

;;;_  . mockbuf:make-form-find-file

;;$$Rename back
;;;###autoload
(defun mockbuf:make-form-find-file-2 (options key context)
   "Return a form that interprets KEY in OPTIONS as an instruction to
switch to a buffer visiting a given file.  A second arg to KEY is
interpreted as a string to place point at, as in
`find-file-goto-text'.

OPTIONS and KEY can be symbols.  They are interpreted when the form is
evalled."

   (form-by-option options key
      #'(lambda (x)
	   `(mockbuf:find-file-2
	       ,context
	       ,@(cdr x)))))

;;;_   , Tests

;;Tested thru ade:links:let-the-usual

;;;_ , Check file is what's expected

;;;_  . Test data

(defconst mockbuf:check-file:thd:virtual-dir
   (list
      (make-mockbuf:virtual-dir 
	 mockbuf:th-examples-dir
	 :dir "." 
	 :file-basenames
	 '("file1" "file2")))
   
   "File context for mockbuf check-file functionality" )

;;;_  . mockbuf:buf-is-file

(defun mockbuf:buf-is-file (buf filename)
   ""
   (if
      (not (file-name-absolute-p filename))
      (error "mockbuf:buf-is-file: FILENAME should be absolute")
      (string= (buffer-file-name buf) filename)))
;;;_   , Tests
(rtest:deftest mockbuf:buf-is-file
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

;;;_  . mockbuf:buf-is-file-at

;;;###autoload
(defun mockbuf:buf-is-file-at (filename loc-string &optional motion)
   "Return non-nil if current buffer is visiting FILENAME and point is
at LOC-STRING.

If MOTION is given, start point at the beginning of LOC-STRING and
execute MOTION, then check that point is at the same place.  MOTION,
if given, must be a function."

   (and
      (mockbuf:buf-is-file (current-buffer) filename)
      (if
	 (null motion)
	 (looking-at loc-string)
	 (if
	    (not (functionp motion))
	    (error 
	       "mockbuf:buf-is-file-at: MOTION is not a function")
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


;;;_   ; Tests

(rtest:deftest mockbuf:buf-is-file-at

   ("Situation: Positions simply don't match.  
Response: return nil."

      (not
	 (let
	    ((filename
		(mockbuf:virtual-dir:base->filename 
		   "file1" 
		   mockbuf:check-file:thd:virtual-dir))
	       (str "position=mockbuf-1"))
	    (with-current-buffer
	       (find-file-noselect filename)
	       (goto-char 4)
	       (mockbuf:buf-is-file-at filename str)))))


   ("Situation: mockbuf:find-file-goto-text has just been called with
the same arguments. No motion argument is given.
Response: Returns non-nil."

      (let
	 ((filename
	     (mockbuf:virtual-dir:base->filename 
		"file1" 
		mockbuf:check-file:thd:virtual-dir))
	    (str "position=mockbuf-1"))
	 (with-current-buffer
	    (mockbuf:find-file-goto-text filename str)
	    (mockbuf:buf-is-file-at filename str))))


   ("Situation: MOTION moves to same place (BOB).  
Response: return non-nil."

      (let
	 ((filename
	     (mockbuf:virtual-dir:base->filename 
		"file1" 
		mockbuf:check-file:thd:virtual-dir))
	    (str "position=mockbuf-1"))
	 (with-current-buffer
	    (find-file-noselect filename)
	    (goto-char 4)
	    (mockbuf:buf-is-file-at filename str
	       #'(lambda nil
		    (goto-char
		       (point-min)))))))

   ("Situation: Original positions are in different places, but
MOTION finds a % that precedes both.  
Response: return non-nil."

      (let
	 ((filename
	     (mockbuf:virtual-dir:base->filename 
		"file2" 
		mockbuf:check-file:thd:virtual-dir)))
	 (with-current-buffer
	    (mockbuf:find-file-goto-text filename "position=mockbuf-1")
	    (mockbuf:buf-is-file-at filename "position=mockbuf-meta-1"
	       #'(lambda nil
		    (search-backward "%"))))))


   ("Situation: MOTION can't be executed.  
Response: error."

      (let
	 ((filename
	     (mockbuf:virtual-dir:base->filename 
		"file1" 
		mockbuf:check-file:thd:virtual-dir)))
	 (with-current-buffer
	    (find-file-noselect filename)
	    (not
	       (ignore-errors
		  (progn
		     (mockbuf:buf-is-file-at filename str
			'(1 2))
		     t))))))

	  
   )


;;;_  . mockbuf:file-contents

(defun mockbuf:file-contents (filename)
   ""
   (with-temp-buffer
      (mockbuf:cautious-insert-file filename)
      (buffer-string)))

;;;_  . mockbuf:files-match

;;;###autoload
(defun mockbuf:files-match (a b)
   "Return nil if the files differ, non-nil if they don't"
   (string-equal (mockbuf:file-contents a) (mockbuf:file-contents b)))



;;;_   , Tests

;;Tested thru mockbuf:virtual-dir:constantize's tests

;;;_  . (Dep) mockbuf:w/context:buf-matches-file

;;Obsolescent
(defun mockbuf:w/context:buf-matches-file (context basename)
   "Return non-nil if buffer's contents are the same as contents of
the file BASENAME in context CONTEXT"
   
   (let*
      ((filename
	  (mockbuf:virtual-dir:base->filename basename context)))
      (string-equal 
	 (buffer-string) 
	 (mockbuf:file-contents filename))))
;;;_  . mockbuf:file-contents-1

;;NB, the Name-1 convention here is the opposite of the usual
(defun mockbuf:file-contents-absname (file &optional dir)
   "Return the contents of a file given by absolute name"
   (let
      (
	 (filename
	    (if dir
	       (expand-file-name file dir)
	       file)))
      (unless (file-name-absolute-p filename)
	 (error "File name should be absolute"))
      (mockbuf:file-contents filename)))

;;;_   , Tests

(put 'mockbuf:file-contents-absname 'rtest:test-thru
   'mockbuf:buf-contents-matches)

;;;_  . Structures for holding comparison data

;;Merge these with emtest's comparison holders
(defstruct mockbuf:direct-comparison-data
   ""
   got
   expected)


(defstruct mockbuf:regex-comparison-data
   ""
   got
   pattern-expected)


;;;_  . mockbuf:last-bad-comparison
(defvar mockbuf:last-bad-comparison nil 
   "" )
;;;_  . mockbuf:ediff-last-comparison
;;;###autoload
(defun mockbuf:ediff-last-comparison (comparison)
   ""
   (interactive (list mockbuf:last-bad-comparison))

   (cond
      ((null comparison)
	 (error "Can't find any previous comparison"))
      ((mockbuf:direct-comparison-data-p comparison)
	 (let 
	    ;;Make a buffer for each
	    ((buf-expected (generate-new-buffer "expected"))
	       (buf-got    (generate-new-buffer "got")))
	    ;;Put each into its buffer 
	    (with-current-buffer buf-expected
	       (insert (mockbuf:direct-comparison-data-expected comparison)))
	    (with-current-buffer buf-got
	       (insert
		  (mockbuf:direct-comparison-data-got comparison)))
	    ;;Run ediff on those buffers.
	    (ediff-buffers buf-expected buf-got)))
      
      ((mockbuf:regex-comparison-data-p comparison)
	 ;;For now, just show a buffer
	 (with-current-buffer (generate-new-buffer "regex compare")
	    (insert "Comparing vs a regex\n")
	    (insert "\n-----------------------------------\n")
	    (insert "Expected\n")
	    (insert 
	       (mockbuf:regex-comparison-data-pattern-expected
		  comparison))
	    (insert "\n-----------------------------------\nGot\n")
	    (insert 
	       (mockbuf:regex-comparison-data-got
		  comparison))
	    ))))


;;;_   , Tests
;;Has to be tested interactively
'
(let
   ((mockbuf:last-bad-comparison 
       (make-mockbuf:direct-comparison-data 
	  :got "abc\ndef\nghi"
	  :expected "abc\nabf\nghi")))
   (mockbuf:ediff-last-comparison mockbuf:last-bad-comparison))


;;;_  . mockbuf:buf-contents-matches

;;Key `:object' only exists for historical reasons.
;;;###autoload
(defun* mockbuf:buf-contents-matches (&rest args &key file dir string
					object sexp buf
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
	    (mockbuf:string-matches
	       str
	       (apply #'mockbuf:contents nil args)
	       regex-marks
	       (if regex-marks
		  (mapcar
		     #'(lambda (val)
			  (apply #'mockbuf:contents dir val))
		     validate-re))))
	 
	 ((or object sexp)
	    (equal
	       (or object sexp)
	       (with-current-buffer buf-in
		  (save-excursion
		     (mockbuf:buffer-object))))))))

;;;_   , Test helper data

(defconst mockbuf:buf-contents-matches:thd:dir
   (expand-file-name "matches-file/" mockbuf:th-examples-dir)
   "Directory of example files for mockbuf:buf-contents-matches." )

(defconst mockbuf:buf-contents-matches:thd:yes.txt 
   "line A
line B
"
   "The contents of the buffers used in the tests." )

;;;_   , Tests

(rtest:deftest mockbuf:buf-contents-matches

   ;;Comparison to string

   ( "Param: `string' is given.
Action: Treat string as the expected value." 
      (with-buffer-containing-object
	 (:string "abc def")
	 (mockbuf:buf-contents-matches 
	    :string "abc def")))

   ( "Param: `string' is given.
Action: Treat string as the expected value." 
      (not
	 (with-buffer-containing-object
	    (:string "abc def")
	    (mockbuf:buf-contents-matches 
	       :string "Do not match\n"))))

   ( "Param: `string' and `regex-marks' are given.
Action: Treat string as containing bounded regexps, as for
mockbuf:string-matches." 
      (with-buffer-containing-object
	 (:string "abc def")
	 (mockbuf:buf-contents-matches 
	    :string "abc [.*]"
	    :regex-marks '("[""]"))))

   
   ( "Behavior: Point is not moved." 
      (with-buffer-containing-object
	 (:string "abc def")
	 (goto-char 3)
	 (mockbuf:buf-contents-matches 
	    :string "abc def")
	 (= (point) 3)))
   
   ;;Comparison to file
   
   (  "Situation: Filename is not absolute
Response: Error."
      ;;Set directory and be sure file exists, so it's not missing
      ;;file error.
      (let ((default-directory mockbuf:buf-contents-matches:thd:dir))
	 (rtest:gives-error
	    (mockbuf:buf-contents-matches 
	       :file "yes.txt"))))

   ;;Param: `dir' is given.  Action: file is expanded wrt that dir.
   ;;Could create another example in another dir to demo this, but
   ;;YAGNI)

   (  "Situation: `dir' is given but expanded filename is not absolute,
Response: Error."
      (rtest:gives-error
	 (mockbuf:buf-contents-matches 
	    :file "yes.txt" 
	    :dir ".")))

   (  "Situation: File does not exist
Response: Error."
      (rtest:gives-error
	 (mockbuf:buf-contents-matches 
	    :file "invalid.txt" 
	    :dir mockbuf:buf-contents-matches:thd:dir)))

   ( "Param: `buf' is given.
Action: Compare file to contents of `buf'."
      (with-buffer-containing-object
	 (:string mockbuf:buf-contents-matches:thd:yes.txt)
	 (let
	    ((buf (current-buffer)))
	    (with-temp-buffer
	       (insert "This buffer would not match the file")
	       (mockbuf:buf-contents-matches 
		  :file "yes.txt" 
		  :dir mockbuf:buf-contents-matches:thd:dir
		  :buf buf)))))
   
   ( "Param: `buf' is not given.
Action: Compare file to contents of current buffer."
      (with-buffer-containing-object
	 (:string mockbuf:buf-contents-matches:thd:yes.txt)
	 (mockbuf:buf-contents-matches 
	    :file "yes.txt" 
	    :dir mockbuf:buf-contents-matches:thd:dir)))

   ( "Situation: Buffer contents and file contents do not match.
Response: Comparison fails."
      (not 
	 (with-buffer-containing-object
	    (:string mockbuf:buf-contents-matches:thd:yes.txt)
	    (mockbuf:buf-contents-matches 
	       :file "no.txt" 
	       :dir mockbuf:buf-contents-matches:thd:dir))))

   ( "Param: `regex-marks' is given.
Action: Treat file as containing bounded regexps, as for
mockbuf:string-matches." 
      (with-buffer-containing-object
	 (:string mockbuf:buf-contents-matches:thd:yes.txt)
	 (mockbuf:buf-contents-matches 
	    :file "regexp-yes.txt" 
	    :dir mockbuf:buf-contents-matches:thd:dir
	    :regex-marks '("[""]"))))
   

   ;;Comparison to sexp object
   ( "Param: `sexp' is given.
Action: Succeed just if the buffer contains the sexp representation
of the object." 
      (with-buffer-containing-object
	 (:string "(a b c)")
	 (mockbuf:buf-contents-matches 
	    :sexp '(a b c))))

   ( "Param: `sexp' is given.
Behavior: Point is not moved" 
      (with-buffer-containing-object
	 (:string "(a b c)")
	 (goto-char 3)
	 (mockbuf:buf-contents-matches 
	    :sexp '(a b c))
	 (= (point) 3)))
   


   ;;YAGNI: and not if it contains something readable after that
   ;;object.

   ;;YAGNI: object-list

   ;;** Subgroup: Args for validation. **
   ;;Param: `:regex-marks's value contains the key `:validate-re',
   ;;whose value is a list of validators.

   ;;Background: "regexp-yes.txt" matches "yes.txt" and
   ;;matches mockbuf:buf-contents-matches:thd:yes.txt.

   ;;$$And a test should test that directory should be the same.
   ("Situation: Validator is given. It's a file that matches the
regexp pattern. 
Response: Proceed normally."
      (with-buffer-containing-object
	 (:string mockbuf:buf-contents-matches:thd:yes.txt)
	 (mockbuf:buf-contents-matches 
	    :file "regexp-yes.txt" 
	    :dir mockbuf:buf-contents-matches:thd:dir
	    :regex-marks 
	    '("[""]" )
	    :validate-re
	    '((:file "yes.txt")))))
   

   ("Situation: Validator is given. It's a file that does not match
the regexp pattern. 
Response: Error."
      (with-buffer-containing-object
	 (:string mockbuf:buf-contents-matches:thd:yes.txt)
	 (rtest:gives-error
	    (mockbuf:buf-contents-matches 
	       :file "regexp-yes.txt" 
	       :dir mockbuf:buf-contents-matches:thd:dir
	       :regex-marks 
	       '("[""]")
	       :validate-re
	       '((:file "no.txt"))))))
   

   ( "Situation: Validator given. It's a string that matches the regexp
pattern. 
Response: Proceed normally."
      (with-buffer-containing-object
	 (:string mockbuf:buf-contents-matches:thd:yes.txt)
	 (mockbuf:buf-contents-matches 
	    :file "regexp-yes.txt" 
	    :dir mockbuf:buf-contents-matches:thd:dir
	    :regex-marks 
	    '("[""]" )
	    :validate-re
	    `((:string ,mockbuf:buf-contents-matches:thd:yes.txt)))))
   
   )

;;;_  . mockbuf:contents
;;Need better name
;;Move it near other file/string figurer.
;;Generally shouldn't call it from outside.  
(defun* mockbuf:contents (default-dir &key file dir string &allow-other-keys)
   "Get contents, according to arguments.
For internal use by mockbuf.  Outside callers probably want
`mockbuf:file-contents-absname'."

   (cond
      (file
	 (mockbuf:file-contents-absname file (or dir default-dir)))
      (string
	 string)))

;;;_   , Tests

;;;_  . mockbuf:extract-regexp

(defun mockbuf:extract-regexp (pattern-str foremark aftermark)
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

;;;_   , Tests

(rtest:deftest mockbuf:extract-regexp
   ;;All slightly too narrow because they test the form of the return
   ;;value, not semantics. YAGNI fix.
   ("Situation: An empty string.
Response: regexp is just ^$ to only accept the empty string"
      (equal
	 (mockbuf:extract-regexp "" "[" "]")
	 "^$"))

   
   (  "Situation: String is just one regexp
Return: that regexp surrounded by ^$ to match a whole string."
      (equal
	 (mockbuf:extract-regexp "[b+]" "[" "]")
	 "^b+$"))
   
   (  "Situation: String is several parts, regexps are trivial.
Return: those parts in order, surrounded by ^$ to match a whole string."
      (equal
	 (mockbuf:extract-regexp "a[b]c" "[" "]")
	 "^abc$"))

   ( "Situation: A character class is given in bracket form.
Recognizers are brackets.
Response: Gives error"

      (rtest:gives-error
	 (mockbuf:extract-regexp "[[:digit:]*]" "[" "]")))
   
   ( "Situation: A character class is given in bracket form.
Recognizers are brackets.
Response: Extracts the character class indicator correctly"

      (equal
	 (mockbuf:extract-regexp "%[[:digit:]*%]" "%[" "%]")
	 "^[:digit:]*$"))
   
   )
;;;_  . mockbuf:string-matches-re

(defun* mockbuf:string-matches-re
   (string-got expected validators foremark aftermark)
   ""
   (let
      ((regexp (mockbuf:extract-regexp expected foremark aftermark)))
      (dolist (validation-string validators)
	 (unless
	    (string-match regexp validation-string)
	    (error "Regular expression failed validation")))
      
      (if
	 (string-match regexp string-got)
	 t)))

;;;_   , Tests
(put 'mockbuf:string-matches-re 'rtest:test-thru
   'mockbuf:string-matches)

;;;_  . mockbuf:string-matches


(defun mockbuf:string-matches (string-got expected &optional
				 regex-marks validators)
   ""
      
   (if
      regex-marks
      (progn
	 (unless
	    (consp regex-marks)
	    (error "regex-marks needs to be a list"))
	 (if
	    (apply #'mockbuf:string-matches-re string-got expected
	       validators regex-marks)
	    t
	    (progn
	       (setq mockbuf:last-bad-comparison
		  (make-mockbuf:regex-comparison-data
		     :got string-got
		     :pattern-expected expected))
	       nil)))
      
      (if
	 (string-equal string-got expected)
	 t
	 (progn
	    (setq mockbuf:last-bad-comparison
	       (make-mockbuf:direct-comparison-data
		  :got string-got
		  :expected expected))
	    nil))))


;;;_   , Tests

(rtest:deftest mockbuf:string-matches

   ;;Would be nice to apply the "Param" doc to a section, but not
   ;;currently easy.
   ( "Param: `regex-marks' is not given.  
Action: Compare to expected as a string."
      (mockbuf:string-matches
	 "abc def"
	 "abc def"))
   
   ( "Situation: `string-got' and `expected' do not match.
Response: Comparison fails."
      (not 
	 (mockbuf:string-matches
	    "abc"
	    "def")))

   ( "Param: `regex-marks' is a pair of strings.
Action: Compare to a regex formed from file.  Any part between those
strings as fore/after markers."

	 (mockbuf:string-matches
	    "abbbc"
	    "a[b+]c"
	    '("[""]")))
   
   
   ( "Situation: regex-style expected string which doesn't match
Response: Comparison fails."
      (not
	 (mockbuf:string-matches
	 "abbbd"
	 "a[b+]c"
	 '("[""]"))))
   
   ;;A common case.  Note that in Elisp modes the "space" character
   ;;class does not match \n. 
   ( "Situation: regex matches any space 
Response: Comparison succeeds."

      (mockbuf:string-matches
	 "a b\tc"
	 ;;Note double escapes.
	 "a[\\s-*]b[\\s-*]c"
	 '("[""]")))

   ;;Note the need to use different regex delimiters than ("[""]")
   ( "Situation: regex matches any space.  It's given in
character-class form.
Response: Comparison succeeds."

      (mockbuf:string-matches
	 "a b\tc"
	 ;;Note double escapes.
	 "a%[[[:space:]]*%]b%[[[:space:]]*%]c"
	 '("%[""%]")))

   ( "Situation: regex matches any number 
Response: Comparison succeeds."

      (mockbuf:string-matches
	 "a0123b456"
	 "a%[[0-9]*%]b%[[0-9]+%]"
	 '("%[""%]")))

   ( "Situation: regex (in char class form) matches any number 
Response: Comparison succeeds."

      (mockbuf:string-matches
	 "a0123b456"
	 "a%[[[:digit:]]*%]b%[[[:digit:]]+%]"
	 '("%[""%]")))

   ( "Situation: regex-style file has, as a non-regexp section, some
magic regexp characters.
Response: Comparison still treats that section as literal."

	 (mockbuf:string-matches "a+b+|c" "a+b+|c"  '("[""]")))

   ;;(YAGNI) If `regex-marks' is `t', use file as regex with default
   ;;markers (TBD)

   ( "Param: `regex-marks' is something else.  
Action: Error."
      (rtest:gives-error
	 (mockbuf:string-matches "a" "a"  "Just one string")))

   
   )

;;;_   , mockbuf:buffer-object

;;;###autoload
(defun mockbuf:buffer-object () 
   "Return the object, if any, described by current buffer"
   (goto-char (point-min))
   (read (current-buffer)))

;;;_    . Tests

(rtest:deftest mockbuf:buffer-object

   ("Shows: `mockbuf:buffer-object' reads the lisp object whose
printed representation the current buffer contains."
      (equal
	 (with-buffer-containing-object (:string "")
	    (pp '(1 5) (current-buffer))
	    (mockbuf:buffer-object))
	 '(1 5)))

   ("Shows: `mockbuf:buffer-object' and
`with-buffer-containing-object' are complementary."
      (equal
	 (with-buffer-containing-object 
	    (:sexp '(ab ef))
	    (mockbuf:buffer-object))
	 '(ab ef)))
   )



;;;_ , (Deprecated) with-undone-file

;;Deprecated.  Use master/slave copying instead.
'
(defmacro with-undone-file (filename &rest body)
   ""
   
   `(with-current-buffer
       (find-file-noselect ,filename)
       (buffer-enable-undo)
       (prog1
	  (progn ,@body)
		 
	  ;;Now undo the change(s), which requires making
	  ;;an undo-boundary first.
	  (undo-boundary)
	  (undo))))

;;;_ , Skeletons

;;They are in rtest-insert

;;;_: Footers
;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + mode: allout
;;;_  + outline-regexp: ";;;_ *"
;;;_  + ade-project-root: "."
;;;_  + End:

;;;_ , Provide

(provide 'mockbuf)
;;; mockbuf.el ends here
