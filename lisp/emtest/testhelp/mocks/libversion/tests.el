;;;_ emtest/testhelp/mocks/libversion/tests.el --- Tests for libversion

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

(require 'emtest/main/define)
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/misc)
(require 'emtest/testhelp/mocks/libversion)

;;;_. Body
;;;_ , Data-helper
;;This plus a (provide) statement is the same as the load files'
;;contents.  But we can't just use it because load files need to come
;;from a known place.  But only one test cares about that.
(defun emtmv:th:build-form-of-items ()
   "Build a form which defines all the items in the current `which' group"
   (cons
      'progn
      (emtg:ignore-tags (role)
	 (emtg:map name nil
	    (case (emtg (type metatype))
	       (variable
		  `(defconst ,(emtg (type sym))
		      ,(emtg (type value))
		      ,(emtg (type docstring))))
	       (function
		  `(defun ,(emtg (type sym)) ()
		      ,(emtg (type docstring))
		      ,(emtg (type value))))
	       (set-prop
		  `(put 
		      'foo:var2 
		      'foo:properties 
		      ,(emtg (type value)))))))))

;;;_ , Data
;;;_  . emtmv:th:examples-dir
(defconst emtmv:th:examples-dir
      (emtb:expand-filename-by-load-file "examples") 
      "Directory where examples are" )

;;;_  . emtmv:th:data
(defconst emtmv:th:data
   (emtg:define+
      ()
      (group ((which old))
	 (item ((role filename))
	    (expand-file-name "stable/foo.el" emtmv:th:examples-dir))
	 (group ((role items))
	    (group
	       ((name var1))
	       (item ((type metatype))  'variable)
	       (item ((type sym))       'foo:var1)
	       (item ((type value))     "Old foo var 1")
	       (item ((type docstring)) "Old foo:var1's docstring"))
	    (group
	       ((name var2))
	       (item ((type metatype))  'variable)
	       (item ((type sym))       'foo:var2)
	       (item ((type value))     "Old foo var 2")
	       (item ((type docstring)) "Old foo:var2's docstring"))
	    (group
	       ((name unshared))
	       (item ((type metatype))  'variable)
	       (item ((type sym))       'foo:old:unshared)
	       (item ((type value))     "Old foo unshared variable")
	       (item ((type docstring)) "Old foo:old:unshared's docstring"))
	    (group
	       ((name fun1))
	       (item ((type metatype))  'function)
	       (item ((type sym))       'foo:fun1)
	       (item ((type value))     "Old foo fun 1")
	       (item ((type docstring)) "Old foo:fun1's docstring"))	 
	    (group
	       ((name set-prop))
	       (item ((type metatype))  'set-prop)
	       (item ((type value))     "Old foo")))
	 (item ((role form))
	    (emtmv:th:build-form-of-items)
	    nil))
      

      (group ((which new))
	 (item ((role filename))
	    (expand-file-name "foo.el" emtmv:th:examples-dir))
	 (group ((role items))
	    (group
	       ((name var1))
	       (item ((type metatype))  'variable)
	       (item ((type sym))       'foo:var1)
	       (item ((type value))     "New foo var 1")
	       (item ((type docstring)) "New foo:var1's docstring"))
	    (group
	       ((name var2))
	       (item ((type metatype))  'variable)
	       (item ((type sym))       'foo:var2)
	       (item ((type value))     "New foo var 2")
	       (item ((type docstring)) "New foo:var2's docstring"))
	    (group
	       ((name unshared))
	       (item ((type metatype))  'variable)
	       (item ((type sym))       'foo:new:unshared)
	       (item ((type value))     "New foo unshared variable")
	       (item ((type docstring)) "New foo:new:unshared's docstring"))
	    (group
	       ((name fun1))
	       (item ((type metatype))  'function)
	       (item ((type sym))       'foo:fun1)
	       (item ((type value))     "New foo fun 1")
	       (item ((type docstring)) "New foo:fun1's docstring"))	 
	    (group
	       ((name set-prop))
	       (item ((type metatype))  'set-prop)
	       (item ((type value))     "New foo")))
	 (item ((role form))
	    (emtmv:th:build-form-of-items)
	    nil))))

;;;_ , emtmv:th:load
(defun emtmv:th:load (&optional skip-loading-new)
   "Do the usual setting up.  Leave setup in the state `new'.
If SKIP-LOADING-NEW is non-nil, do not load the new file."
   (if skip-loading-new
      (emt:doc "Setup, but don't load new version.")
      (emt:doc "Setup as usual."))
   (emt:doc "Load old file.")
   (load-file
      (emtg (role filename) (which old)))
   (emt:doc "Operation: Create libversion object.")
   (emt:doc "Start in `old' (which captures contents of old lib)")
   (setq emtmv:t
      (emtmv:create-obj-from-file-list 
	 (list
	    (emtg (role filename) (which old)))
	 'old))
   (emt:doc "Operation: Switch state to `new'")
   (emtmv:change-state 'new emtmv:t)
   (unless skip-loading-new
      (emt:doc "Load the new file")
      (load-file
	 (emtg (role filename) (which new))))
   emtmv:t)

;;;_ , emtmv:th:check-all
(defun emtmv:th:check-all ()
   "Check that all the values are as expected.  
Call this inside a narrowing to (which WHICH)."
   
   ;;Would loop over items (role items), switching on each one's
   ;;metatype.  
   (emtg:narrow ((role items))
      (emtg:map name name
	 ;;For variables:
	 (case (emtg (type metatype))
	    (variable
	       (emt:assert
		  (equal
		     (eval (emtg (type sym)))
		     (emtg (type value)))))
	    (function
	       (emt:assert
		  (equal
		     (funcall (emtg (type sym)))
		     (emtg (type value)))))
	    (set-prop
	       (emt:assert
		  (equal
		     (get
			'foo:var2 
			'foo:properties)
		     (emtg (type value)))))))))
;;;_ , emtmv:th:num-atoms
(defun emtmv:th:num-atoms (oa)
   "Return how many atoms are in obarray OA"

   (let
      ((count 0))
      (mapatoms 
	 #'(lambda
	      (s)
	      (setq count (1+ count)))
	 oa)
      count))
;;;_ , emtmv:th:surrounders
(defconst emtmv:th:surrounders 
   '(
       (emtg:with emtmv:th:data ())
       (let
	  ;;Insulate values
	  (
	     ;;State
	     emtmv:t emtmv:features
	     ;;Configuration
	     emtmv:stable-config
	     (emtmv:vc-list emtmv:th:vc-list)

	     ;;Global config affecting this
	     (load-path (list emtmv:th:examples-dir))
	     ;;Global state altered in loading
	     load-history features

	     ;;Symbols defined in test files that we load
	     compiled:load-file-name
	     foo:load-file-name

	     foo:old:unshared
	     foo:new:unshared
	     foo:var1 foo:var2 foo:fun1))

       ;;Insulate functions
       (flet
	  ((foo:fun1)))

       ;;Insulate properties
       (emth:let-noprops
	  '(foo:old:unshared
	      foo:new:unshared
	      foo:var1 foo:var2 foo:fun1)))
   "Common surrounders for emtmv tests" )
;;;_ , emtmv:with-version
(put 'emtmv:with-version 'emt:test-thru
   'emtmv:change-state)

;;;_ , emtmv:change-state
(emt:deftest-3 
   ((of 'emtmv:change-state)
      (:surrounders emtmv:th:surrounders))
   

   (nil
      (progn
	 (emt:doc "Validates: obarray does reflect let bindings.")
	 (emt:doc "Situation: Symbol is not bound globally.")
	 (emt:assert (not (boundp 'invalid-d535)))
	 (let ((invalid-d535 12)) 
	    (emt:doc "Situation: Symbol is bound locally.")
	    (emt:doc "Response: Symbol can be found in obarray.")
	    (emt:assert (intern-soft "invalid-d535" obarray))
	    (emt:assert (intern-soft "invalid-d535"))
	    (emt:doc "Response: Symbol has the right value.")
	    (emt:assert
	       (equal
		  (symbol-value (intern-soft "invalid-d535"))
		  12)))))


   (nil
      (progn
	 (emt:doc "Situation: Nothing is set up.")
	 (emt:doc "Operation: `emtmv:with-version' given nil.")
	 (emt:assert
	    (emth:gives-error
	       (emtmv:with-version nil nil
		  t)))
	 (emt:doc "Response: Raises error.")))
   (nil
      (progn
	 (emt:doc "Situation: Nothing is set up.")
	 (emt:doc "Operation: `emtmv:with-version' given non-nil.")
	 (emt:doc "In particular: `old'")
	 (emt:assert
	    (emth:gives-error
	       (emtmv:with-version 'old nil
		  t)))
	 (emt:doc "Response: Raises error.")))

   (nil
      (progn
	 (emt:doc "Operation: `emtmv:change-state'")
	 (emt:doc "Param: No libversion object given.")
	 (emt:doc "Response: Makes an error.")
	 (emt:assert
	    (emth:gives-error
	       (emtmv:change-state 'old nil)))))
   (nil
      (progn
	 (emtmv:th:load t)
	 (emt:doc "Operation: `emtmv:change-state'")
	 (emt:doc "Param: Invalid `new-state'.")
	 (emt:doc "Response: Makes an error.")
	 (emt:assert
	    (emth:gives-error
	       (emtmv:change-state 'invalid nil )))))

   (nil
      (let
	 ((lv-obj (emtmv:th:load)))
	 
	 (emtmv:with-version 'old lv-obj
	    (emt:doc "Operation: Call with symbol `old'.")
	    (emt:doc "Response: Has the values of old version.")
	    (emtg:narrow ((which old))
	       (emtmv:th:check-all)))
	 

	 (emtmv:with-version 'new lv-obj
	    (emt:doc "Operation: Call with symbol `new'.")
	    (emt:doc "Response: Has the values of new version.")
	    (emtg:narrow ((which new))
	       (emtmv:th:check-all)))

	 (emtmv:with-version 'new lv-obj
	    (emtmv:with-version 'old lv-obj
	       (emt:doc "Operation: Old one nested in new.")
	       (emt:doc "Response: Has the values of old version.")
	       (emtg:narrow ((which old))
		  (emtmv:th:check-all))))

	 
	 (emtmv:with-version 'old lv-obj
	    (emtmv:with-version 'new lv-obj
	       (emt:doc "Operation: New one nested in old.")
	       (emt:doc "Response: Has the values of new version.")
	       (emtg:narrow ((which new))
		  (emtmv:th:check-all))))))

   (nil
      (let
	 ((lv-obj (emtmv:th:load t)))
	 (emt:doc "Operation: Switch state to `new'")
	 (emtmv:change-state 'new lv-obj)
	 (emt:doc "Operation: Eval new stuff (instead of loading)")
	 (eval
	    (emtg (role form) (which new)))	 

	 (emtmv:with-version 'old lv-obj
	    (emt:doc "Operation: Call with symbol `old'.")
	    (emt:doc "Response: Has the values of old version.")
	    (emtg:narrow ((which old))
	       (emtmv:th:check-all)))
	 

	 (emtmv:with-version 'new lv-obj
	    (emt:doc "Operation: Call with symbol `new'.")
	    (emt:doc "Response: Has the values of new version.")
	    (emtg:narrow ((which new))
	       (emtmv:th:check-all)))))


   
   (nil
      (progn
	 (emt:doc "Proves: Manually settings variables affects only
      the active version.") 
	 (emt:doc "Proves: Evalling affects only the active version.") 
	 
	 (let
	    (  (lv-obj (emtmv:th:load))
	       (value "Another value"))
	    (emt:doc "Situation: In state `new'")
	    (emt:assert (eq (emtmv:t->version lv-obj) 'new))

	    (emt:doc "Assign to a variable")
	    (setq foo:var1 value)
	    ;;And a function, and a property.

	    (emtmv:with-version 'old lv-obj
	       (emt:doc "Operation: Eval it in `old'.")
	       (emt:doc "Response: Its value in old has not changed.")
	       (emtg:narrow ((which old))
		  (emtmv:th:check-all)))
	 
	    (emtmv:with-version 'new lv-obj
	       (emt:doc "Operation: Eval it in `new'.")
	       (emt:doc "Response: In `new' it has the new value.")
	       (emt:assert
		  (equal foo:var1 value)))

	    (emt:doc "Situation: Still in state `new'")
	    (emt:assert (eq (emtmv:t->version lv-obj) 'new))

	    (emt:doc "Re-eval the `new' form")
	    (eval
	       (emtg (role form) (which new)))
	    (emtmv:with-version 'new lv-obj
	       (emt:doc "Response: It no longer has that value in `new'.")
	       (emt:assert
		  (not 
		     (equal foo:var1 value))))

	    (emtmv:with-version 'old lv-obj
	       (emt:doc "Response: Its value in `old' has not changed.")
	       (emtg:narrow ((which old))
		  (emtmv:th:check-all)))
	    
	    (emt:doc "Operation: Change to state `old'.")
	    (emtmv:change-state 'old lv-obj)

	    (emt:doc "Assign to a variable")
	    (setq foo:var1 value)
	    ;;And a function, and a property.

	 
	    (emt:doc "Operation: Eval it in `old'.")
	    (emtmv:with-version 'old lv-obj
	       (emt:doc "Response: In `old' it has the new value.")
	       (emt:assert
		  (equal foo:var1 value)))	    

	    (emtmv:with-version 'new lv-obj
	       (emt:doc "Response: Its value in `new' has not changed.")
	       (emtg:narrow ((which new))
		  (emtmv:th:check-all)))

	    ))))
;;;_ , emtmv:add-advice
(emt:deftest-3 
   ((of 'emtmv:add-advice)
      (:surrounders emtmv:th:surrounders))
   (nil
      (flet
	 ((run-stuff () foo:var1))
	 (emt:doc "Situation: Function run-stuff returns its value of
   `foo:var1'.")
	 (let
	    ((lv-obj
		(emtmv:th:load)))
	    (emt:doc "Situation: In the `new' bindings")
	    (emt:doc "Operation: Advise run-stuff.")
	    (emtmv:add-advice 'run-stuff 'old lv-obj)
	    (emt:doc "Operation: Run run-stuff.")
	    (emt:doc "Response: It returns the `old' value of `foo:var1'.")
	    (emt:assert
	       (equal
		  (run-stuff)
		  (emtg (which old)(name var1)(type value))))))))
;;;_ , emtmv:load-stable
(put 'emtmv:load-stable 'emt:test-thru
   'emtmv:require-x)
;;;_ , Mock VC functions
;;;_  . emtmv:require-x:th:vc:insert-file-x
(defun emtmv:require-x:th:vc:insert-file-x (buf filename)
   "Mock insert function.  Just insert the contents of FILENAME."
   (with-current-buffer buf
      (insert-file-contents filename)
      (setq buffer-file-name filename)))

;;;_  . emtmv:require-x:th:vc:insert-file-by-tag
(defun emtmv:require-x:th:vc:insert-file-by-tag (buf branch-name lib-path)
   "Mock vc function.  Ignore lib-path and insert the contents of the
file named by current (emtg (role filename)(which old))"
   ;;For now, assumes that (which old) is meant.
   (emtmv:require-x:th:vc:insert-file-x 
      buf
      (emtg (role filename)(which old))))

;;;_  . emtmv:require-x:th:vc:insert-file
(defun emtmv:require-x:th:vc:insert-file (buf branch-name lib-path)
   "Mock vc function.  Just insert the contents of the respective file
if it exists."
   (when
      (file-exists-p lib-path)
      (emtmv:require-x:th:vc:insert-file-x buf lib-path)))

;;;_  . emtmv:require-x:th:vc:insert-no-name
(defun emtmv:require-x:th:vc:insert-no-name (buf branch-name lib-path)
   "Mock vc function.  Deliberately buggily leaves the filename `nil."
   (with-current-buffer buf
      (setq buffer-file-name nil)
      t))

(defun emtmv:require-x:th:vc:insert-file-if-source (buf branch-name lib-path)
   "Mock insert function.  Insert the contents of LIB-PATH only if
it's source (el), not compiled.  Otherwise do nothing and return nil."
   (when
      (string=
	 (file-name-extension lib-path)
	 "el")
      (emtmv:require-x:th:vc:insert-file-x buf lib-path)))

;;;_ , Mock configuration
;;;_ , emtmv:require-x:th:stable-config
(defconst emtmv:require-x:th:stable-config 
   (list
      (list
	 'foo
	 "old"
	 'insert-by-tag
	 '()))
   "Testhelp mock list of info about stable versions of libs" )
;;;_ , Values for emtmv:vc-list
;;;_  . emtmv:th:vc-lib-sym
;;NB, the VC (mock) functions live in "tests.el" and not in this
;;pseudo-library, which is just tested for getting loaded.
;;Only to be loaded with load-path set to the examples directory.
(defconst emtmv:th:vc-lib-sym
   'vc
   "Symbol of the pseudo-library for testhelp VC functions" )

;;;_  . emtmv:th:vc-list
(defconst emtmv:th:vc-list 
   '(  (insert-by-tag
	  vc
	  emtmv:require-x:th:vc:insert-file-by-tag)
       (insert-no-name
	  vc
	  emtmv:require-x:th:vc:insert-no-name)
       (insert-if-source
	  vc
	  emtmv:require-x:th:vc:insert-file-if-source)
       (insert
	  vc
	  emtmv:require-x:th:vc:insert-file))
   "Testhelp mock list of VC software (all mocks for special purposes)" )
;;;_ , emtmv:insert-version
(put 'emtmv:insert-version 'emt:test-thru
   'emtmv:require-x)
;;;_ , emtmv:load-stable
(put 'emtmv:load-stable 'emt:test-thru
   'emtmv:require-x)
;;;_ , emtmv:require-x
(emt:deftest-3 
   ((of 'emtmv:require-x)
      (:surrounders emtmv:th:surrounders))

   (nil
      (let
	 ((emtmv:stable-config 
	     emtmv:require-x:th:stable-config)
	    (vc-lib-sym emtmv:th:vc-lib-sym))
	 (emt:doc "Shows: Loads the appropriate VC library if neccessary.")
	 (when (featurep vc-lib-sym)
	    (unload-feature vc-lib-sym t))
	 (emt:doc "Situation: The VC lib is not loaded.")
	 (emt:assert
	    (not (featurep vc-lib-sym)))

	 (emt:doc "Operation: require-x on `foo'")
	 (emtmv:require-x '(foo) '())
	 (emt:doc "Response: The VC lib is now loaded")
	 (emt:assert (featurep vc-lib-sym))))
   
   (nil
      (let* 
	 (  (lib-sym 'foo)
	    (emtmv:stable-config 
	       (list
		  (list
		     lib-sym
		     "master"
		     'insert
		     '()))))
	 
	 (emt:doc "Shows: load-file-name is set up correctly when loading.")
	 (when (featurep lib-sym)
	    (unload-feature lib-sym t))
	 (emt:doc "Situation: The library is not loaded.")
	 (emt:assert
	    (not (featurep lib-sym)))
	 (emt:doc "Operation: require-x on lib-sym.")
	 (emtmv:require-x (list lib-sym) '())
	 (emt:doc "Response: The library is now loaded")
	 (emt:assert (featurep lib-sym))
	 (let* 
	    ((lfn foo:load-file-name))
	    (emt:doc "Response: load-file-name is non-nil")
	    (emt:assert (not (null lfn)))
	    (emt:doc "Response: It points at the right location")
	    (emt:assert
	       (string=
		  lfn
		  (emtg (which new)(role filename)))))))
   
   (nil
      (let*
	 (  (lib-sym 'compiled)
	    (emtmv:stable-config 
	       (list
		  (list
		     lib-sym
		     "master"
		     'insert-if-source
		     '()))))
	 
	 (emt:doc "Shows: When only an .el version of the file exists
in the master, we can retrieve it even when `locate-library' wants to
give us an .elc")
	 (emt:doc "Situation: For this test, the VC will only load source.")
	 (when (featurep lib-sym)
	    (unload-feature lib-sym t))
	 (emt:doc "Situation: The library is not loaded.")
	 (emt:assert
	    (not (featurep lib-sym)))
	 (emt:doc "Situation: locate-library finds it with an .elc extension")
	 (emt:assert
	    (string=
	       (file-name-extension
		  (locate-library (symbol-name lib-sym)))
	       "elc"))
	 (emt:doc "Operation: require-x on lib-sym.")
	 (emtmv:require-x (list lib-sym) '())
	 (emt:doc "Response: The library is now loaded")
	 (emt:assert (featurep lib-sym))
	 (let* 
	    ((lfn compiled:load-file-name))
	    (emt:doc "Response: library is the .el version")
	    (emt:assert
	       (string=
		  (file-name-extension lfn)
		  "el")))))
   

   (nil
      (let
	 ((emtmv:stable-config emtmv:require-x:th:stable-config))
	 (flet
	    ((run-stuff () foo:var1))
	    (emt:doc "Situation: Function run-stuff returns its value of
   `foo:var1'.")
	    (emt:doc "Operation: require-x on `foo' and intercepting
      `run-stuff'") 
	    (emtmv:require-x '(foo) '(run-stuff))
	    (emt:doc "Operation: load the new `foo'")
	    (load-file
	       (emtg (role filename) (which new)))

	    (emt:doc "Response: run-stuff returns the `old' value of
   `foo:var1'.") 
	    (emt:assert
	       (equal
		  (run-stuff)
		  (emtg (which old)(name var1)(type value))))
	    
	    (emt:doc "Response: Inspecting `foo:var1' in the
	    larger context gives the new value.")
	    (emt:assert
	       (equal
		  foo:var1
		  (emtg (which new)(name var1)(type value)))))))
   
   (nil
      (let
	 ((emtmv:stable-config
	     '((foo "old" insert-no-name ()))))
	 (emt:doc
	    "Situation: Buggy VC function which does not set the filename")
	 
	 (flet
	    ((run-stuff () foo:var1))
	    (emt:doc "Situation: Function run-stuff returns its value of
   `foo:var1'.")
	    (emt:doc "Operation: require-x on `foo'")
	    (emt:doc "Response: Error.")
	    (emt:assert
	       (emth:gives-error
		  (emtmv:require-x '(foo) '(run-stuff))))))))


;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/libversion/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/libversion/tests.el ends here
