;;;_ emtest/testhelp/mocks/libversion.el --- Mock library versioning

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, maint

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
(eval-when-compile
   (require 'cl))
;;;_. Body
;;;_ , Customizations
(defgroup emtest/testhelp/mocks/libversion ()
   "Customization for Emtest libversion, part of testhelp"
   :group 'emtest/testhelp)

(defcustom emtmv:vc-list 
   '((git 
	emtest/testhelp/mocks/libversion/vc/git 
	emtmv:vc:git:insert-file)) 
   "List of available version control systems."
   :type '(repeat
	     (list
		:tag "Data for this version control system"
		(symbol 
		   :tag "Its symbol" 
		   :doc "Need not be bound")
		(symbol 
		   :tag "Symbol of the support library to load")
		(symbol 
		   :tag "Entry point"
		   :doc "Symbol of a function taking 3 args:
 * buffer to be inserted into.
 * branch-name of the stable branch.
 * full path to the library, as it exists currently."
		   )))
   :group 'emtest/testhelp/mocks/libversion)


(defcustom emtmv:stable-config () 
   "List of info about stable versions of libraries
Format:
 * Lib symbol
 * Stable branch name
 * VC symbol
 * Extra args (ignored for now)"
   :type '(repeat 
	    (list 
	       (symbol :tag "Library symbol")
	       (string :tag "Stable branch name")
	       (symbol :tag "Version control symbol")
	       (repeat :tag "Extra arguments"
		  (sexp))))
   :group 'emtest/testhelp/mocks/libversion)


;;;_ , Types
;;;_  . emtmv:hl-el
(deftype emtmv:hl-el ()
   "The type of a spec-list element.  
Same as a history-list element"
   t)
;;;_  . emtmv:t
(defstruct (emtmv:t
	      (:constructor emtmv:make-t)
	      (:conc-name emtmv:t->))
   "State of a versioned library"
   (name       "Unnamed" :type string)
   (new-values () :type (repeat (list emtmv:hl-el * *)))
   (old-values () :type (repeat (list emtmv:hl-el * *)))
   (version    () :type (member nil old new))
   (specs      () :type (repeat emtmv:hl-el)))

;;;_  . emtmv:lib-as-spec
(defstruct (emtmv:lib-as-spec
	      (:constructor emtmv:make-lib-as-spec)
	      (:conc-name emtmv:lib-as-spec->))
   "A library to be transformed to a spec"
   sym 
   str 
   extra-syms
   stable-version
   vc-func
   hist-key)


;;;_ , Variables
(defvar emtmv:t nil
   "The current versioned library, or `nil'" )

;;Keep these variables in sync with test insulator

;;;_ , Entry points
;;;_  . For code
;;;_   , emtmv:with-version
(defmacro emtmv:with-version (version dummy &rest body)
   "Evaluate BODY with bindings for VERSION.
VERSION should be `old' or `new'.
Arg DUMMY is reserved in case we ever support multiple invocations."
   (when dummy (error "Passing a value for DUMMY is reserved"))
   (let
      ((ov-sym (make-symbol "old-version")))
      `(progn
	  (when (not emtmv:t)
	     (error "Need to set up first"))
	  (let
	     ((,ov-sym (emtmv:t->version emtmv:t)))
	     (unwind-protect
		(progn
		   ;;OK even if new state is the same as old.
		   (emtmv:change-state ,version ,dummy)
		   ,@body)
       
		(emtmv:change-state ,ov-sym ,dummy))))))
;;;_   , emtmv:add-advice
(defmacro emtmv:add-advice (func &optional version)
   "Advise FUNC to always use a particular version.
FUNC will generally be an entry point"
   
   `(defadvice ,func 
       (around 
	  ,(intern 
	      (concat 
		 (symbol-name func)
		 "advised-by-libversion"))
	  activate)
       ;;Advise handles docstrings and interactive forms smartly, so
       ;;we don't need to do much.
       ,(concat "Using the %s library version"
	   (case version 
	      (old "old")
	      (new "new")
	      (otherwise "UNKNOWN")))
       (emtmv:with-version
	  ,(or version 'old) ()
	  ad-do-it)))
;;;_   , emtmv:require
(defun emtmv:require (&rest r)
   "Like `require', but manage versions.  
Intended for use in vtest.el files."
   
   (error "`emtmv:require' is not available yet"))
;;;_   , emtmv:require-x
(defun emtmv:require-x (lib-sym-list advised-list)
   "Load stable versions of LIB-SYM-LIST and advise ADVISED-LIST to
use them.
LIB-SYM-LIST is a list of symbols of the required libraries.
ADVISED-LIST is a list of symbols of the advised functions."

   (let*
      ((las-list
	  (mapcar #'emtmv:sym->lib-as-spec lib-sym-list)))
      (dolist (las las-list)
	 (emtmv:load-stable las))

      ;;Now we're confident we can build the spec, since we've loaded
      ;;all the files.
      ;;For now, we set the global object here too.
      (setq emtmv:t 
	 (emtmv:create-obj-2 
	    (apply #'append
	       (mapcar
		  #'emtmv:lib-as-spec->spec
		  las-list))))
      (emtmv:change-state 'old nil)
      (emtmv:change-state 'new nil)

      (dolist (func advised-list)
	 (eval
	    `(emtmv:add-advice ,func 'old)))))
;;;_   , emtmv:insert-version
(defun emtmv:insert-version (buf las)
   "Insert a stable version of a library into buffer BUF.
LAS must be a `emtmv:lib-as-spec'"

   (let
      ((base-path 
	  (file-name-sans-extension
	     (locate-library (emtmv:lib-as-spec->str las))))
	 (vc-func 
	    (emtmv:lib-as-spec->vc-func las))
	 (stable-version
	    (emtmv:lib-as-spec->stable-version las)))
      (catch 'emtmv:inserted
	 (progn
	    (dolist (suffix (get-load-suffixes))
	       (when
		  (funcall 
		     vc-func
		     buf
		     stable-version
		     ;;Suffix has a leading dot
		     (concat base-path suffix))
		  (throw 'emtmv:inserted (concat base-path suffix))))
	    (error "Could not find the %s version of %s"
	       stable-version
	       base-path)))))

;;;_   , emtmv:load-stable
(defun emtmv:load-stable (las)
   "Load a stable version of a library as specified by LAS.
LAS must be a `emtmv:lib-as-spec'"
   (with-temp-buffer
      (erase-buffer)
      (let
	 ((load-file-name
	     (emtmv:insert-version (current-buffer) las)))
      
	 (unless buffer-file-name
	    (error "Buffer file name was not defined"))
	 (setf (emtmv:lib-as-spec->hist-key las) 
	    buffer-file-name)

	 ;;Could optionally byte-compile but YAGNI.
	 (eval-buffer)
	 (set-buffer-modified-p nil))))

;;;_   , emtmv:lib-as-spec->spec
(defun emtmv:lib-as-spec->spec (las)
   "Get real spec from LAS"
   (emtmv:get-history-line
      (emtmv:lib-as-spec->hist-key las)))

;;;_   , emtmv:sym->lib-as-spec
(defun emtmv:sym->lib-as-spec (sym)
   ""
   (let*
      ((str (symbol-name sym))
	 (cell (assoc sym emtmv:stable-config))
	 (vc-sym (third cell))
	 (vc-cell (assoc vc-sym emtmv:vc-list)))
      (require (second vc-cell))
      (emtmv:make-lib-as-spec
	 :sym sym
	 :str str
	 :extra-syms     (fourth cell)
	 :stable-version (second cell)
	 :vc-func (third vc-cell))))

;;;_  . Interactivity help
;;;_   , emtmv:read-object
(defun emtmv:read-object (prompt)
   "Interactively read a libversion object."
   ;;For now, just return the only one that ever exists.
   emtmv:t)
;;;_   , emtmv:read-version
(defun emtmv:read-version (prompt)
   "Interactively read `old' or `new', defaulting to `old'"
   (intern
      (completing-read prompt
	 '("old" "new") nil t nil nil "old")))

;;;_  . For user
;;;_   , emtmv:advise-function
(defun emtmv:advise-function (func version)
   "Advise FUNC to use VERSION instead of the global version."
   
   (interactive
      (list
	 (intern
	    (completing-read "Advise which function: "
	       obarray #'functionp t))
	 (emtmv:read-version "Should use which version: ")))
   (emtmv:add-advice func version))
;;;_   , Removing advice: just use `ad-unadvise'
;;;_   , emtmv:start
(defun emtmv:start (lib-filename version)
   "Start emtmv.
Assumes that spec is completely based on filenames
Assumes that LIB-FILENAME has already been loaded.
Leaves emtmv in state VERSION."
   
   (interactive
      (list
	 (completing-read-multiple
	    "Which modules are being versioned? "
	    load-history nil t)
	 (emtmv:read-version "Current version is: ")))
   '  ;;$$USE this, but will error until `emtmv:change-state' allows dummy
   (setq
      emtmv:t
      (emtmv:create-obj lib-filename version))
   (emtmv:change-state version nil lib-filename))

;;;_   , Start it, giving module symbol-name
;;Another entry point to the same functionality
;;;_   , Start it, loading original file
;;Another entry point to essentially the same functionality
;;"Load and version which file? "

;;;_   , emtmv:toggle-state
(defun emtmv:toggle-state (&optional new-state)
   ""
   (interactive)
   (emtmv:change-state
      (or
	 new-state
	 (case (emtmv:t->version emtmv:t)
	    (old 'new)
	    (new 'old)
	    ((nil) (error "libversion hasn't been started"))))
      nil))
;;;_   , Add a file to what is controlled
;;Entry point for `emtmv:add-spec'
;;;_   , Add symbol at point to obarrays
;;Entry point for `emtmv:add-spec'

;;;_ , Functions
;;;_  . emtmv:get-history-line
;;Similar to file-loadhist-lookup but uses file-truename
(defun emtmv:get-history-line (fullpath)
   ""
   (cdr
      (assoc 
	 (file-truename fullpath) 
	 load-history)))

;;;_  . emtmv:create-obj-2
(defun emtmv:create-obj-2 (specs &optional initial-version)
   "Create a libversion object from SPEC-SPEC.
Set it to INITIAL-VERSION if non-nil.

SPEC-SPEC can be:
 (spec SPEC), meaning to use SPEC just as it is
 (lib-filenames FILENAME-LIST) meaning to build the spec from
 FILENAME-LIST."
   (let
      ((obj
	  (emtmv:make-t
	     :new-values nil
	     :old-values nil
	     :version nil
	     :specs specs)))
      
      (when initial-version
	 (emtmv:change-state initial-version obj))
      obj))

;;;_  . emtmv:create-obj
(defun emtmv:create-obj (lib-filename-list &optional initial-version)
   "Create an object *by list of filenames*"
   ;;$$USE ME
   '(emtmv:create-obj-2 
       (apply #'append
	  (mapcar
	     #'emtmv:get-history-line
	     lib-filename-list)))
   (let
      ((obj
	  (emtmv:make-t
	     :new-values nil
	     :old-values nil
	     :version nil
	     :specs
	     (apply #'append
		(mapcar
		   #'emtmv:get-history-line
		   lib-filename-list)))))
      
      (when initial-version
	 (emtmv:change-state initial-version obj))
      obj))

;;$$USE ME  This factors out part of emtmv:change-state.  Use it in
;;tests and in emtmv:start
;;But will error until `emtmv:change-state' allows dummy

;;;_  . emtmv:change-state 
;;$$UPDATE ME  Allow dummy to be passed, and not lib-filename-list,
;;and don't use this to init any more.
(defun emtmv:change-state (new-version dummy &optional lib-filename-list)
   "Change the current state"
   ;;Temporary
   (when dummy (error "Passing a value for DUMMY is reserved"))
   (unless emtmv:t
      (setq emtmv:t
	 (emtmv:create-obj lib-filename-list)))
   (unless (memq new-version '(old new))
      (error "Invalid state %s" new-version))

   (let
      ((old-version (emtmv:t->version emtmv:t)))
      (unless (eq new-version old-version)
	 (when old-version
	    (emtmv:save-version old-version emtmv:t))
	 (emtmv:activate-version new-version emtmv:t)
	 (setf (emtmv:t->version emtmv:t) new-version))))

;;;_  . emtmv:set-values
(defun emtmv:set-values (version obj values)
   ""
   (case version
	 (new
	    (setf (emtmv:t->new-values obj) values))
	 (old
	    (setf (emtmv:t->old-values obj) values))))
;;;_  . emtmv:get-values
(defun emtmv:get-values (version obj)
   ""
   (case version
      (new
	 (emtmv:t->new-values obj))
      (old
	 (emtmv:t->old-values obj))))
;;;_  . emtmv:save-version
(defun emtmv:save-version (version obj)
   "Save current values into obarray OA and return OA.
OA can be nil in which case a new obarray is created and returned.
If initialized, it will be from the module loaded from FILENAME."

   (emtmv:set-values version obj
      (delq nil
	 (mapcar
	    #'emtmv:zip-w/value
	    (emtmv:t->specs obj)))))

;;;_  . emtmv:activate-version
(defun emtmv:activate-version (version obj)
   "Restore current values from obarray OA and return OA.
OA can be nil in which case a new obarray is created and returned.
If initialized, it will be from the module loaded from FILENAME."
   (mapcar
      #'emtmv:restore-value
      (emtmv:get-values version obj)))


;;;_  . Manage value lists

;;;_   , emtmv:zip-w/value
(defun emtmv:zip-w/value (entry)
   "Given ENTRY, create the cell (ENTRY VALUE PLIST) accordingly."
   
   (cond
      ((symbolp entry)
	 (list
	       entry
	       (symbol-value entry)
	       (copy-list (symbol-plist entry))))
      ((and
	  (consp entry)
	  (memq (car entry) '(defun autoload)))
	 (let
	    ((real-sym (cdr entry)))
	    (list
	       entry
	       (symbol-function real-sym)
	       (copy-list (symbol-plist real-sym)))))))
;;;_   , emtmv:restore-value-x
(defun emtmv:restore-value-x (entry value plist)
   ""
   (cond
      ((symbolp entry)
	 (set entry value)
	 (setplist entry plist))
      ((and
	  (consp entry)
	  (memq (car entry) '(defun autoload)))
	 (let
	    ((real-sym (cdr entry)))
	    (fset real-sym value)
	    (setplist real-sym plist)))))


;;;_   , emtmv:restore-value
(defun emtmv:restore-value (cell)
   "Given (ENTRY VALUE PLIST), set symbol ENTRY accordingly"
   (apply #'emtmv:restore-value-x cell))

;;;_  . Adding specs

;;;_   , emtmv:add-spec
;;$$WRITE ME
(defun emtmv:add-spec (obj spec)
   "Add another spec to OBJ"

   (let*
      ()
      
      ))


;;;_   , Add all symbols from a particular list


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/libversion)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/libversion.el ends here
