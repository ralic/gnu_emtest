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
;;;_ , Customization
(defconst emtmv:extra-affected-syms
   ()
   "Alist from library symbol to list of extra affected symbols.
Unused for now." )
;;;_ , Structures
(defstruct (emtmv:t
	      (:constructor emtmv:make-t)
	      (:conc-name emtmv:t->))
   "State of a versioned library"
   (new-obarray () :type vector)
   (old-obarray () :type vector)
   (version     () :type (member nil old new))
   ;;May want to capture&swap respective load-history lines too.
   ;;May want to have two version of this.
   (filename    () :type string))

;;;_ , Variables
(defvar emtmv:t nil
   "The current versioned library, or `nil'" )
;;Keep this in sync with test insulator

'
(defvar emtmv:new-obarray nil
   "Objects from the new version of the module" )
'
(defvar emtmv:old-obarray nil
   "Objects from the old version of the module"  )
'
(defvar emtmv:state 
   nil
   "Which obarray currently corresponds to the real obarray state.
Should be `nil', `old', or `new'" )
'
(defvar emtmv:filename nil 
   "Full true loadname of file being synced to" )


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
Assumes that LIB-FILENAME has already been loaded.
Leaves emtmv in state VERSION."
   
   (interactive
      (list
	 (completing-read
	    "Which module is being versioned? "
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
(defun emtmv:toggle-state ()
   ""
   (interactive)
   (emtmv:change-state
      (case (emtmv:t->version emtmv:t)
	 (old new)
	 (new old)
	 ((nil) (error "libversion hasn't been started")))
      nil))
;;;_   , Add a file to what is controlled
;;;_   , Add symbol at point to obarrays
;;Mostly just calls add symbol to obarrays

;;;_ , Functions
;;;_  . emtmv:get-history-line
(defun emtmv:get-history-line (fullpath)
   ""
   (cdr
      (assoc 
	 (file-truename fullpath) 
	 load-history)))
;;;_  . emtmv:create-obj
(defun emtmv:create-obj (lib-filename &optional initial-version)
   ""
   (let
      ((obj
	  (emtmv:make-t
	     :new-obarray nil
	     :old-obarray nil
	     :version nil
	     :filename   
	     (progn
		(unless lib-filename (error "No filename passed"))
		lib-filename))))
      (when initial-version
	 (emtmv:change-state initial-version obj))
      obj))
;;$$USE ME  This factors out part of emtmv:change-state.  Use it in
;;tests and in emtmv:start
;;But will error until `emtmv:change-state' allows dummy

;;;_  . emtmv:change-state 
(defun emtmv:change-state (new-version dummy &optional lib-filename)
   "Change the current state"
   ;;Temporary
   (when dummy (error "Passing a value for DUMMY is reserved"))
   (unless emtmv:t
      (setq emtmv:t
	 (emtmv:make-t
	    :new-obarray nil
	    :old-obarray nil
	    :version nil
	    :filename   
	    (progn
	       (unless lib-filename (error "No filename passed"))
	       lib-filename))))
   (unless (memq new-version '(old new))
      (error "Invalid state %s" new-version))

   (let
      ((old-version (emtmv:t->version emtmv:t)))
      (unless (eq new-version old-version)
	 (when old-version
	    (emtmv:save-version old-version emtmv:t))
	 (emtmv:activate-version new-version emtmv:t)
	 (setf (emtmv:t->version emtmv:t) new-version))))

;;;_  . emtmv:set-obarray
(defun emtmv:set-obarray (version oa)
   ""
   (case version
	 (new
	    (setf (emtmv:t->new-obarray obj) oa))
	 (old
	    (setf (emtmv:t->old-obarray obj) oa))))
;;;_  . emtmv:get-obarray
(defun emtmv:get-obarray (version obj)
   ""
   (case version
      (new
	 (emtmv:t->new-obarray obj))
      (old
	 (emtmv:t->old-obarray obj))))
;;;_  . emtmv:save-version
(defun emtmv:save-version (version obj)
   "Save current values into obarray OA and return OA.
OA can be nil in which case a new obarray is created and returned.
If initialized, it will be from the module loaded from FILENAME."
   (let
      ((oa
	  (emtmv:get-obarray version obj)))
      (emtmv:set-obarray version 
	 (emtmv:sync-obarray 
	    oa (emtmv:t->filename obj) obarray oa))))


;;;_  . emtmv:activate-version
(defun emtmv:activate-version (version obj)
   "Restore current values from obarray OA and return OA.
OA can be nil in which case a new obarray is created and returned.
If initialized, it will be from the module loaded from FILENAME."
   (let
      ((oa
	  (emtmv:get-obarray version obj)))
      (emtmv:set-obarray version 
	 (emtmv:sync-obarray 
	    oa (emtmv:t->filename obj) oa obarray))))

;;;_  . emtmv:sync-obarray
(defun emtmv:sync-obarray (oa filename from to)
   "For the syms in obarray OA, place respective syms from obarray
FROM into obarray TO.  
Return OA.

OA can be nil in which case a new obarray is created and returned.
If initialized, it will be from the module loaded from FILENAME.

Workhorse for `emtmv:activate-obarray' and
`emtmv:save-version'."
   (if
      oa
      (progn
	 (emtmv:refresh-obarray from to oa)
	 oa)
      (emtmv:init-obarray-by-filename filename)))
;;;_  . Set up from file history
;;;_   , emtmv:setup-plist
(defun emtmv:setup-plist (to from)
   ""
   (unless (symbol-plist to)
      (setplist to (copy-list (symbol-plist from)))))

;;;_   , emtmv:set-in-obarray
(defun emtmv:set-in-obarray (oa entry)
   "In obarray OA, set symbol corresponding to ENTRY"
   
   (cond
      ((symbolp entry)
	 (let
	    ((sym
		(intern 
		   (symbol-name entry)
		   oa)))
	    (set sym (symbol-value entry))
	    (emtmv:setup-plist sym entry)))
      ((and
	  (consp entry)
	  (memq (car entry) '(defun autoload)))
	 (let*
	    (  (real-sym (cdr entry))
	       (sym
		  (intern 
		     (symbol-name real-sym)
		     oa)))
	    (fset sym (symbol-function real-sym))
	    (emtmv:setup-plist sym real-sym)))))

;;;_   , emtmv:init-obarray-by-filename
(defun emtmv:init-obarray-by-filename (filename)
   "Return an obarray initted with the current values of all the
   symbols that FILENAME loaded. 
FILENAME must be the name of a file that has already been loaded."
   (unless filename
      (error "No filename passed"))
   (let
      ((hist-line
	  (emtmv:get-history-line filename))
	 (oa (make-vector 255 0)))
      (unless hist-line
	 (error "No load history found for %s" filename))
      (dolist (entry hist-line)
	 (emtmv:set-in-obarray oa entry))
      oa))
;;;_  . Copy one to another
;;;_   , emtmv:copy-sym-by-name
(defun emtmv:copy-sym-by-name (from to name)
   ""
   (let*
      (
	 ;;FROM-SYM need not exist but TO-SYM must.
	 (from-sym (intern-soft name from))
	 (to-sym   (intern      name to)))
      (when
	 (boundp from-sym)
	 (set to-sym (symbol-value from-sym)))
      (when
	 (fboundp from-sym)
	 (fset to-sym (symbol-function from-sym)))
      ;;This is needed even after the first copy - not sure why but
      ;;tests insist it is.
      (setplist to-sym (copy-list (symbol-plist from-sym)))))


;;;_   , emtmv:refresh-obarray
(defun emtmv:refresh-obarray (from to syms-of)
   "Refresh obarray TO with values from obarray FROM.
Obarray SYMS-OF gives the set of values to be refreshed.  It can be
the same obarray as FROM or TO."
   (unless (and from to syms-of)
      (error "At least one obarray was not set up"))
   (mapatoms
      #'(lambda (sym)
	   (emtmv:copy-sym-by-name from to (symbol-name sym)))
      syms-of))

;;;_  . Add another to both

;;;_  . Add symbol to both obarrays
;;No tests yet
;;Current one gets value, other one just gets interned

;;;_  . Add all symbols from a particular list to both obarrays
;;No tests yet
'(when emtmv:extra-affected-syms
    (dolist (sym (cdr (car emtmv:extra-affected-syms)))
       ;;Add that symbol to both obarrays
       ))

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/libversion)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/libversion.el ends here
