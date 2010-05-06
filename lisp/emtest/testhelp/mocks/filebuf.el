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
(require 'rtest-util)  ;;For testing, IIRC.  Or obsoelte, for loading
;;the expander. 
(rtest:if-avail
   ;;Only a few examples use emt:eg.  Most or all should.
   (require 'emtest/testhelp/eg)

   )
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

;;;_ , Test data
;;$$MOVE ME into testhelp
;;;_  . Test config
(defconst emtb:th-examples-dir
   (emtb:expand-filename-by-load-file "filebuf/examples/") 
   "" )


;;;_  . Master/slave files
(defconst emtb:thd:examples
   (emt:eg:define+ ;;xmp:07298be7-85b8-414b-b9ae-e3a1edde2e07
      ((project emtest)(library mockbuf))
      (item
	 ((role master)(type filename))
	 (expand-file-name "file3" emtb:th-examples-dir))
      ;;This item may go away as we automatically make tmp.
      (item
	 ((role slave)(type filename))
	 (expand-file-name "slave-dir/file3" emtb:th-examples-dir))))

;;;_ , Virtual directory
;;Deprecated

;;;_  . Structures

(defstruct 
   (emtb:virtual-dir
      (:constructor make-emtb:virtual-dir
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
				   emtb:slave-root)))
		     (expand-file-name dir emtb:slave-root))
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

(rtest:deftest make-emtb:virtual-dir

   ;;Lest master and slave be the same filename:

   ("Disallow an absolute DIR argument"
      (rtest:gives-error
	 (make-emtb:virtual-dir 
	    emtb:virtual-dir:th:example-root
	    :mutable t 
	    :dir "/foo")))
   
   ("Disallow root = emtb:slave-root."
      (rtest:gives-error
	 (make-emtb:virtual-dir 
	    emtb:slave-root 
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
   (defconst emtb:virtual-dir:ctor-arglist
      '(emtb:virtual-dir:th:example-root
	  :dir
	  "bar" 
	  :file-basenames
	  '("1.txt" "2.txt"))
      "An example arglist to `make-emtb:virtual-dir'")

   (defconst emtb:virtual-dir:ctor-widget-value
      '(emtb:virtual-dir:th:example-root
	  "bar" 
	  ;;"" ;;For now, master-dir still exists as a string.
	  ("1.txt" "2.txt"))
      "An example widget-value corresponding to
`emtb:virtual-dir:ctor-arglist'"))

;;;_   , make-emtb:virtual-dir:sexp->widget-value

;;This could be derived from the ctor definition and the widget
;;definition.


;;Check-type only seems to accept symbols, not lists, so we spread the
;;type check of file-basenames over several calls.
(defun* make-emtb:virtual-dir:sexp->widget-value 
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
;;Fails.
(rtest:deftest make-emtb:virtual-dir:sexp->widget-value
   
   (  "Situation: Normal.  
ROOT is a symbol.
DIR is a string.
FILE-BASENAMES is a quoted list. 
Response: Accepts it."
      (not
	 (rtest:gives-error
	    (make-emtb:virtual-dir:sexp->widget-value
	       'emtb:virtual-dir:th:example-root
	       :dir 
	       '"bar" 
	       :file-basenames
	       ''("1.txt" "2.txt")))))

   ("Accepts the example"
      (not
	 (rtest:gives-error
	    (apply #'make-emtb:virtual-dir:sexp->widget-value
	       emtb:virtual-dir:ctor-arglist))))

   ;;We'd like to test that it transforms into
   ;;`emtb:virtual-dir:ctor-widget-value', but as long as we're
   ;;still handling master-dir, it won't (won't fairly, anyways).
   
   (
      "Situation: Root is not a symbol"
      (rtest:gives-error
	 (make-emtb:virtual-dir:sexp->widget-value
	    emtb:virtual-dir:th:example-root
	    :dir 
	    '"bar" 
	    :file-basenames
	    ''("1.txt" "2.txt"))))
   
   
   (  "Situation: Dir is not a string"
      (rtest:gives-error
	 (make-emtb:virtual-dir:sexp->widget-value
	    'emtb:virtual-dir:th:example-root
	    :dir 
	    'emtb:virtual-dir:th:example-root
	    :file-basenames
	    ''("1.txt" "2.txt"))))
   
   ("Situation: File basenames is not a quoted list"
      (rtest:gives-error
	 (make-emtb:virtual-dir:sexp->widget-value
	    'emtb:virtual-dir:th:example-root
	    :dir 
	    '"bar" 
	    :file-basenames
	    'emtb:virtual-dir:th:example-root)))
   )

;;;_   , make-emtb:virtual-dir:widget-value->sexp

;;This could be trivially derived from the ctor definition and the
;;widget definition.
(defun make-emtb:virtual-dir:widget-value->sexp
   (root dir file-basenames) 
   ""
   `(make-emtb:virtual-dir
       ,root
       ,@(if dir
	    (list ':dir dir))
;;        ,@(if master-dir
;; 	    (list ':master-dir master-dir))
       ,@(if file-basenames
	    (list ':file-basenames `'(,@file-basenames)))))

;;;_    . Tests
;;Fails
(rtest:deftest make-emtb:virtual-dir:widget-value->sexp

   ;;This can't just be tested by simple comparison, because the forms
   ;;might mean the same but differ in particulars.  Instead we
   ;;compare values that have both been transformed - any particulars
   ;;ought to "fall the same way" both times.
   ("Shows: `make-emtb:virtual-dir:widget-value->sexp' reverses
`make-emtb:virtual-dir:sexp->widget-value'.  Both are to be applied
to forms."

      (let 
	 ((wid-value
	     (apply
		#'make-emtb:virtual-dir:sexp->widget-value
		emtb:virtual-dir:ctor-arglist)))
	 (equal
	    wid-value
	    (apply
	       #'make-emtb:virtual-dir:sexp->widget-value
	       (cdr
		  (apply 
		     #'make-emtb:virtual-dir:widget-value->sexp
		     wid-value)))))))

;;;_   , emtb:virtual-dir:widget-type


(defconst emtb:virtual-dir:widget-type
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

;;;_   , `make-emtb:virtual-dir' properties

;;Format is alpha, might change drastically.
(put 'make-emtb:virtual-dir 'handhold:data
   (list
      emtb:virtual-dir:widget-type
      #'make-emtb:virtual-dir:widget-value->sexp
      #'make-emtb:virtual-dir:sexp->widget-value))

;;;_  . Test setup

(defconst emtb:virtual-dir:th:example-root 
   emtb:th-examples-dir
   "Dummy symbol referring to the root directory of a dummy project" )
'  ;;Obsolete.
(defmacro emtb:virtual-dir:th:usuals-x (&rest body)
   ""
   
   `(symbol-macrolet
       (
	  (filesthing-list 
	     emtb:virtual-dir:thd:example-multi)
	  )
       ,@body))

;;;_  . Example objects

(defconst emtb:virtual-dir:thd:example-simple
   (make-emtb:virtual-dir
      emtb:virtual-dir:th:example-root
      :dir "bar" 
      :file-basenames
      '("1.txt" "2.txt"))
   "Example virtual directory, simple case" )

(defconst emtb:virtual-dir:thd:example-slaved
   (make-emtb:virtual-dir
      emtb:th-examples-dir
      :dir "bar"
      :mutable t
      :file-basenames
      '("3.txt"))
   "Example slaved files" )

(defconst emtb:virtual-dir:thd:example-multi
   (list
      emtb:virtual-dir:thd:example-simple
      emtb:virtual-dir:thd:example-slaved
      (make-emtb:virtual-dir
	 emtb:virtual-dir:th:example-root
	 :mutable t
	 :dir "bar/single-master/"
	 :file-basenames
	 '("alone.txt")))

   
   "Example virtual directory" )

;;;_  . emtb:virtual-dir:get-match

(defun emtb:virtual-dir:get-match (basename filesthing-list)
   ""
   (unless (listp filesthing-list)
      (if
	 (emtb:virtual-dir-p filesthing-list)
	 (error "Arg Filesthing-List should be a list of filesthing,
not a single filesthing..")
	 (error "Arg filesthing-list is not a list")))
   
   (find basename filesthing-list 
      :test 
      #'(lambda (key el)
	   (member key 
	      (emtb:virtual-dir-file-basenames el)))))

;;;_   , Tests

(rtest:deftest emtb:virtual-dir:get-match

   ("Situation: name is one of the names in first object.  
Response: Return first object"
      (equal
	 (emtb:virtual-dir:get-match 
	    "1.txt" 
	    emtb:virtual-dir:thd:example-multi)
	 emtb:virtual-dir:thd:example-simple))
   
   ("Situation: name is one of the names in second object.  
Response: Return second object"

      (equal
	 (emtb:virtual-dir:get-match 
	    "3.txt" 
	    emtb:virtual-dir:thd:example-multi)
	 emtb:virtual-dir:thd:example-slaved))

   ("Situation: name is not one of the names.  
Response: Return nil"
      (not
	 (emtb:virtual-dir:get-match 
	    "nomatch.txt" 
	    emtb:virtual-dir:thd:example-multi))))


;;;_  . emtb:virtual-dir:suspected-list->singleton
(defun emtb:virtual-dir:suspected-list->singleton (basename maybe-list)
   ""
   
   (if
      (listp maybe-list)
      (emtb:virtual-dir:get-match basename maybe-list)
      maybe-list))

;;;_   , Tests

(put 'emtb:virtual-dir:suspected-list->singleton
   'rtest:test-thru
   'emtb:virtual-dir:base->filename)

;;Also thru emtb:virtual-dir:base->master-name 

;;;_  . -> true filenames

;;;_   , emtb:virtual-dir:base->filename

(defun emtb:virtual-dir:base->filename (basename filesthing)
   "Return the absolute filename corresponding to BASENAME.
FILESTHING is a filesthing"
   (expand-file-name 
      basename 
      (emtb:virtual-dir-abs-dir 
	 (emtb:virtual-dir:suspected-list->singleton 
	    basename 
	    filesthing))))

;;;_    . Tests

(rtest:deftest emtb:virtual-dir:base->filename

   ("Situation: Given basename is one of the basenames.
Response: Returns a filename expanded vs both root and dir"

      (equal
	 (emtb:virtual-dir:base->filename "1.txt"
	    emtb:virtual-dir:thd:example-simple)
	 (expand-file-name "1.txt"
	    (expand-file-name "bar" 
	       emtb:virtual-dir:th:example-root))))


   ;;YAGNI: Situation: Basename is not one of the basenames.

   )

;;;_   , emtb:virtual-dir:base->master-name

(defun emtb:virtual-dir:base->master-name (basename filesthing)
   ""
   
   (let
      ((master-dir (emtb:virtual-dir-abs-master-dir 
		      (emtb:virtual-dir:suspected-list->singleton
			 basename
			 filesthing))))
      (if
	 (null master-dir)
	 (error "Basename %s is not slaved" basename)
	 (expand-file-name basename master-dir))))


;;;_    . Tests
(rtest:deftest emtb:virtual-dir:base->master-name
   ("Situation: Given basename is one of the basenames and filesthing
is slaved.
Response: Returns a filename expanded vs both root and master dir"

      (equal
	 (emtb:virtual-dir:base->master-name "3.txt"
	    emtb:virtual-dir:thd:example-slaved)
	 (expand-file-name "3.txt"
	    (expand-file-name "bar" 
	       emtb:virtual-dir:th:example-root))))

   
   ;;YAGNI: Situation: filesthing is not slaved.
   
   )


;;;_ , Setup file

;;;_  . Master/slave copying

;;;_   , Test data

(defconst emtb:thd:a-string 
   "Some text that won't match real file"
   "Example text for writing into files for tests" )

;;;_   , Test helper

(defmacro* emtb:th:let-slave-master-pair
   ((slave-name master-name basename virtual-dir) &rest body)
   "`let' the slave and master filenames around BODY.
If MASTER-NAME is nil, no master filename object is created"
   
   `(let
       (  
	  (,slave-name
	     (emtb:virtual-dir:base->filename 
		,basename
		,virtual-dir))
	  ,@(if master-name
	       `((,master-name
		    (emtb:virtual-dir:base->master-name
		       ,basename
		       ,virtual-dir)))
	       ()))
       ,@body))


;;;_   , emtb:fresh-copy

(defun emtb:fresh-copy (slave master)
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

;;Also tested thru emtb:virtual-dir:constantize.

(rtest:deftest emtb:fresh-copy
   ;;NOT AUTOMATABLE. The test here is that no user interaction
   ;;happens.  There is no simple way of checking that directly.
   ("Situation: Buffer already exists and has been modified.  
Response: Even so, there is no interaction.
IF THERE WAS USER INTERACTION, this test may have failed."

      (emtb:th:let-slave-master-pair
	 (file3-slave file3-master	 
	    "3.txt" 
	    emtb:virtual-dir:thd:example-slaved)

	 (with-current-buffer
	    (find-file-noselect file3-slave)
	    (insert emtb:thd:a-string))
	 (emtb:fresh-copy file3-slave file3-master)
	 t))

   ("Situation: slave-file doesn't exist and its parent directory
doesn't exist.
Response: Still opens without a problem"
      (emtb:th:let-slave-master-pair
	 (filename-slave filename-master 
	    "alone.txt"
	    emtb:virtual-dir:thd:example-multi)

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
	       (emtb:fresh-copy 
		  filename-slave
		  filename-master)))))

   )


;;;_   , emtb:virtual-dir:constantize

(defun emtb:virtual-dir:constantize (basename filesthing)
   ""
   (let
      ((filename
	  (emtb:virtual-dir:base->filename basename filesthing)))
      
      (if
	 ;;If there is a master dir...
	 (emtb:virtual-dir-abs-master-dir filesthing)
	 ;;Get a fresh copy.
	 (emtb:fresh-copy
	    filename
	    (emtb:virtual-dir:base->master-name basename
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

;;Also tested thru emtb:find-file-2

(rtest:deftest emtb:virtual-dir:constantize
   
   ("A slaved file is replaced by the master copy."
   
      (let
	 ((virtual-dir emtb:virtual-dir:thd:example-slaved))
	 (emtb:th:let-slave-master-pair
	    (slave master "3.txt" virtual-dir)
	    (with-temp-file slave
	       (insert emtb:thd:a-string))
	    (emtb:virtual-dir:constantize "3.txt" virtual-dir)
	    (emtb:files-match master slave))))

   
   ("A non-slaved file remains as it is."
      (let
	 ((virtual-dir emtb:virtual-dir:thd:example-simple))
	 (emtb:th:let-slave-master-pair
	    (file nil "1.txt" virtual-dir)
	    (with-temp-file file
	       (insert emtb:thd:a-string))
	    (emtb:virtual-dir:constantize "1.txt" virtual-dir)
	    (let
	       ((slave-str
		   (with-temp-buffer
		      (insert-file-contents file)
		      (buffer-string))))
	       (string-equal slave-str emtb:thd:a-string)))))

   ;;It's direct.
   '("Situation: Not slaved. buffer is not read-only.  Afterwards:
buffer is read-only."  
       
       )

   )
;;;_  . Buffer containing known contents
;;;_   , Formdata structure emtb:bufcontaining:formdata

;;;_   , with-buffer-containing-object

(defmacro with-buffer-containing-object (args &rest body)
   ""
   (apply #'with-buffer-containing-buildform body args))

;;;_    . Worker `with-buffer-containing-buildform'

;;Key `:printed-object' only exists for historical reasons.  Use
;;`:sexp' instead.
;;Add `sequence', which appends all of these and allows "point" and
;;"(mark NAME)" (Saved in buffer-local var emtb:named-marks)
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
	     (emtb:build-insertform dir 'sexp (or sexp printed-object)))
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

(put 'with-buffer-containing-buildform 'rtest:test-thru
   'with-buffer-containing-object)

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
   ;;$$The key read-final is obsolete; use emtb:buffer-object
   ;;directly
   ;;Removed :dir-root in favor of :dir

   ("Param: :sexp OBJECT is given.
Reaction: Fill the buffer with exactly a printed representation of
OBJECT"
      (equal
	 (with-buffer-containing-object 
	    (:sexp '(1 5))
	    (emtb:buffer-object))
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
	    (emtb:buffer-object))
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
   ;;Missing the directory
   (  "Exercise the `visited-name' feature."

      (emt:eg:with emtb:thd:examples 
	 ((project emtest)(library mockbuf))
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
	    (emtb:files-match master-file slave-file))))
   
   ;;Missing the directory
   (  "Situation: The slave file already exists.
Response: Works the same."
      (emt:eg:with emtb:thd:examples 
	 ((project emtest)(library mockbuf))
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
	    (emtb:files-match master-file slave-file))))


   (  "Param: the symbol `tmp' is given as `:visited-name'.
Behavior: Creates a temporary file."
      (emt:eg:with emtb:thd:examples ((project emtest)(library mockbuf))
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
	    (emtb:files-match master-file slave-file))))

   ;;Missing the directory
   (  "Param: the symbol `tmp' is given as `:visited-name'.
Neither `dir' nor `file' is given.
Behavior: Creates a temporary file with those contents."
      (emt:eg:with emtb:thd:examples ((project emtest)(library mockbuf))
	 (let 
	    (  slave-file
	       (master-file (emt:eg (role master)(type filename))))
	    ;;Visit slave-file, but with contents from master-file.
	    (with-buffer-containing-object 
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
		  (emtb:file-contents-absname filename)
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

;;$$Want to do this differently:  `emtb:goto-mark' will be another
;;function.  Any replacement will be done by setting it for it; this
;;no longer will do replacement.
(defun emtb:goto-text (loc-string &optional replace)
   ""

   (goto-char (point-min))
   (search-forward loc-string)
   (if 
      replace
      (replace-match "")
      (goto-char (match-beginning 0))))

;;;_   , emtb:cautious-find-file

;;Obsolescent
(defun emtb:cautious-find-file (filename)
   ""

   (unless
      (file-name-absolute-p filename)
      (error "Filename %s is not absolute" filename))

   (unless
      (file-exists-p filename)
      (error "File %s doesn't exist" filename))

   (find-file filename))
;;;_   , emtb:cautious-insert-file
(defun emtb:cautious-insert-file (filename &optional visit beg end replace)
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
(put 'emtb:cautious-insert-file 'rtest:test-thru
   'with-buffer-containing-object)

;;We're enforcing never inserting into a full buffer.  Alternatively,
;;this could be left to `insert-file-contents', which will error if
;;buffer is non-empty and visit is non-nil.  TBD.
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
;;;_   , emtb:cautious-load-file

;;Replaced emtb:cautious-find-file and will help replace the
;;find-file thing.
;;Obsolescent: Now we call its pieces at different times.
(defun emtb:cautious-load-file (filename visited-name)
   ""

   ;;`emtb:cautious-insert-file' will error if buffer is non-empty,
   ;;so we don't have to provide that.

   ;;Have to set VISITED arg so that within
   ;;`emtb:cautious-setup-file', `after-find-file' sees a filename
   ;;as it expects to.
   (emtb:cautious-insert-file filename t)
   (emtb:cautious-setup-file 
      visited-name 
      (file-name-directory filename)))


;; If caller wants to test saving files - perhaps for code that
;; creates a project directory, or to test that some file can be read
;; back - caller could set a name by passing it in `visited-name'.

;;;_    . Tests

(put 'emtb:cautious-load-file 'rtest:test-thru
   'with-buffer-containing-object)


;;;_   , emtb:find-file-goto-text

;;Deprecated.  This has been split into `emtb:cautious-find-file'
;;and the part that manages loc-string and (now) prepare-f

;;$$Prepare to replace this.  Several tests use it and
;;emtb:find-file-2 uses it with no options.

(defun emtb:find-file-goto-text (filename &optional loc-string)
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
      ((buf (emtb:cautious-find-file filename)))
      (with-current-buffer buf
	 ;;Almost replaceable.
	 (if loc-string
	    (emtb:goto-text loc-string)
	    (goto-char (point-min))))
      
      buf))

;;;_    . Tests

(rtest:deftest emtb:find-file-goto-text
   ("Situation: Filename is not absolute.
Response: Error."
      (rtest:gives-error (emtb:find-file-goto-text ".")))
   )

;;;_   , emtb:find-file

;;emtb:find-file got obsolete.

;;$$Change callers to call thru emtb:find-file-3.  Then remove
;;loc-string param and its related code.
(defun emtb:find-file-2 (filesthing-0 basename &optional loc-string)
   ""
	 
   (let*
      (
	 (filesthing 
	    (emtb:virtual-dir:suspected-list->singleton
	       basename
	       filesthing-0))

	 (filename
	    (emtb:virtual-dir:base->filename basename filesthing)))
      
      (emtb:virtual-dir:constantize basename filesthing)
      (emtb:find-file-goto-text filename loc-string)))


;;;_    . Tests

;;Tested thru emtb:make-form-find-file, but that's increasingly
;;less adequate.

;;Tested thru ade:deactivate-siblings.

;;;_   , emtb:find-file-3

;;$$Rename this `emtb:find-file'
;;;###autoload
(defun* emtb:find-file-3 (context filebase &key at-str prepare-f) 
   "Find file FILEBASE, as expanded in filecontext CONTEXT.

If PREPARE-F is given, call it with no args.
If AT-STR is given, find it in the buffer and put point at the
beginning of it."
   
   (emtb:find-file-2 context filebase)
   (when prepare-f (funcall prepare-f))
   (if at-str 
      (emtb:goto-text at-str)
      (goto-char (point-min))))

;;;_    . Tests

;;Tested thru `allout-tehom:operate-on-siblings'

;;;_  . emtb:make-form-find-file

;;$$Rename back
;;;###autoload
(defun emtb:make-form-find-file-2 (options key context)
   "Return a form that interprets KEY in OPTIONS as an instruction to
switch to a buffer visiting a given file.  A second arg to KEY is
interpreted as a string to place point at, as in
`find-file-goto-text'.

OPTIONS and KEY can be symbols.  They are interpreted when the form is
evalled."

   (form-by-option options key
      #'(lambda (x)
	   `(emtb:find-file-2
	       ,context
	       ,@(cdr x)))))

;;;_   , Tests

;;Tested thru ade:links:let-the-usual

;;;_ , Check file is what's expected

;;;_  . Test data

(defconst emtb:check-file:thd:virtual-dir
   (list
      (make-emtb:virtual-dir 
	 emtb:th-examples-dir
	 :dir "." 
	 :file-basenames
	 '("file1" "file2")))
   
   "File context for mockbuf check-file functionality" )

;;;_  . emtb:buf-is-file

(defun emtb:buf-is-file (buf filename)
   ""
   (if
      (not (file-name-absolute-p filename))
      (error "emtb:buf-is-file: FILENAME should be absolute")
      (string= (buffer-file-name buf) filename)))
;;;_   , Tests
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


;;;_   ; Tests

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


;;;_  . emtb:file-contents

(defun emtb:file-contents (filename)
   ""
   (with-temp-buffer
      (emtb:cautious-insert-file filename)
      (buffer-string)))

;;;_  . emtb:files-match

;;;###autoload
(defun emtb:files-match (a b)
   "Return nil if the files differ, non-nil if they don't"
   (string-equal (emtb:file-contents a) (emtb:file-contents b)))



;;;_   , Tests

;;Tested thru emtb:virtual-dir:constantize's tests

;;;_  . (Dep) emtb:w/context:buf-matches-file

;;Obsolescent
(defun emtb:w/context:buf-matches-file (context basename)
   "Return non-nil if buffer's contents are the same as contents of
the file BASENAME in context CONTEXT"
   
   (let*
      ((filename
	  (emtb:virtual-dir:base->filename basename context)))
      (string-equal 
	 (buffer-string) 
	 (emtb:file-contents filename))))
;;;_  . emtb:file-contents-1

;;NB, the Name-1 convention here is the opposite of the usual
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

;;;_   , Tests

(put 'emtb:file-contents-absname 'rtest:test-thru
   'emtb:buf-contents-matches)

;;;_  . Structures for holding comparison data

;;Merge these with emtest's comparison holders
(defstruct emtb:direct-comparison-data
   ""
   got
   expected)


(defstruct emtb:regex-comparison-data
   ""
   got
   pattern-expected)


;;;_  . emtb:last-bad-comparison
(defvar emtb:last-bad-comparison nil 
   "" )
;;;_  . emtb:ediff-last-comparison
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

;;Key `:object' only exists for historical reasons.
;;;###autoload
(defun* emtb:buf-contents-matches (&rest args &key file dir string
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
	    (emtb:string-matches
	       str
	       (apply #'emtb:contents nil args)
	       regex-marks
	       (if regex-marks
		  (mapcar
		     #'(lambda (val)
			  (apply #'emtb:contents dir val))
		     validate-re))))
	 
	 ((or object sexp)
	    (equal
	       (or object sexp)
	       (with-current-buffer buf-in
		  (save-excursion
		     (emtb:buffer-object))))))))

;;;_   , Test helper data

(defconst emtb:buf-contents-matches:thd:dir
   (expand-file-name "matches-file/" emtb:th-examples-dir)
   "Directory of example files for emtb:buf-contents-matches." )

(defconst emtb:buf-contents-matches:thd:yes.txt 
   "line A
line B
"
   "The contents of the buffers used in the tests." )

;;;_   , Tests

(rtest:deftest emtb:buf-contents-matches

   ;;Comparison to string

   ( "Param: `string' is given.
Action: Treat string as the expected value." 
      (with-buffer-containing-object
	 (:string "abc def")
	 (emtb:buf-contents-matches 
	    :string "abc def")))

   ( "Param: `string' is given.
Action: Treat string as the expected value." 
      (not
	 (with-buffer-containing-object
	    (:string "abc def")
	    (emtb:buf-contents-matches 
	       :string "Do not match\n"))))

   ( "Param: `string' and `regex-marks' are given.
Action: Treat string as containing bounded regexps, as for
emtb:string-matches." 
      (with-buffer-containing-object
	 (:string "abc def")
	 (emtb:buf-contents-matches 
	    :string "abc [.*]"
	    :regex-marks '("[""]"))))

   
   ( "Behavior: Point is not moved." 
      (with-buffer-containing-object
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
      (with-buffer-containing-object
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
      (with-buffer-containing-object
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (emtb:buf-contents-matches 
	    :file "yes.txt" 
	    :dir emtb:buf-contents-matches:thd:dir)))

   ( "Situation: Buffer contents and file contents do not match.
Response: Comparison fails."
      (not 
	 (with-buffer-containing-object
	    (:string emtb:buf-contents-matches:thd:yes.txt)
	    (emtb:buf-contents-matches 
	       :file "no.txt" 
	       :dir emtb:buf-contents-matches:thd:dir))))

   ( "Param: `regex-marks' is given.
Action: Treat file as containing bounded regexps, as for
emtb:string-matches." 
      (with-buffer-containing-object
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (emtb:buf-contents-matches 
	    :file "regexp-yes.txt" 
	    :dir emtb:buf-contents-matches:thd:dir
	    :regex-marks '("[""]"))))
   

   ;;Comparison to sexp object
   ( "Param: `sexp' is given.
Action: Succeed just if the buffer contains the sexp representation
of the object." 
      (with-buffer-containing-object
	 (:string "(a b c)")
	 (emtb:buf-contents-matches 
	    :sexp '(a b c))))

   ( "Param: `sexp' is given.
Behavior: Point is not moved" 
      (with-buffer-containing-object
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
      (with-buffer-containing-object
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
      (with-buffer-containing-object
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
      (with-buffer-containing-object
	 (:string emtb:buf-contents-matches:thd:yes.txt)
	 (emtb:buf-contents-matches 
	    :file "regexp-yes.txt" 
	    :dir emtb:buf-contents-matches:thd:dir
	    :regex-marks 
	    '("[""]" )
	    :validate-re
	    `((:string ,emtb:buf-contents-matches:thd:yes.txt)))))
   
   )

;;;_  . emtb:contents
;;Need better name
;;Move it near other file/string figurer.
;;Generally shouldn't call it from outside.  
(defun* emtb:contents (default-dir &key file dir string &allow-other-keys)
   "Get contents, according to arguments.
For internal use by mockbuf.  Outside callers probably want
`emtb:file-contents-absname'."

   (cond
      (file
	 (emtb:file-contents-absname file (or dir default-dir)))
      (string
	 string)))

;;;_   , Tests

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

;;;_   , Tests

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

;;;_   , Tests
(put 'emtb:string-matches-re 'rtest:test-thru
   'emtb:string-matches)

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


;;;_   , Tests

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

;;;_   , emtb:buffer-object

;;;###autoload
(defun emtb:buffer-object () 
   "Return the object, if any, described by current buffer"
   (goto-char (point-min))
   (read (current-buffer)))

;;;_    . Tests

(rtest:deftest emtb:buffer-object

   ("Shows: `emtb:buffer-object' reads the lisp object whose
printed representation the current buffer contains."
      (equal
	 (with-buffer-containing-object (:string "")
	    (pp '(1 5) (current-buffer))
	    (emtb:buffer-object))
	 '(1 5)))

   ("Shows: `emtb:buffer-object' and
`with-buffer-containing-object' are complementary."
      (equal
	 (with-buffer-containing-object 
	    (:sexp '(ab ef))
	    (emtb:buffer-object))
	 '(ab ef)))
   )



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
