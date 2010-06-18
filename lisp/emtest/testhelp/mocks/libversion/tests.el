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

(require 'emtest/runner/define)
(require 'emtest/testhelp/standard)
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
;;;_  . 
(defconst emtmv:th:examples-dir
      (emtb:expand-filename-by-load-file "examples") 
      "Directory where examples are" )

;;;_  . emtmv:th:data
(defconst emtmv:th:data
   (emtg:define+
      ()
      (group ((which old))
	 (item ((role filename))
	    (expand-file-name "foo-old.el" emtmv:th:examples-dir))
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
	    (expand-file-name "foo-new.el" emtmv:th:examples-dir))
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
   ;;Would like to suppress these messages when not of interest.
   (emt:doc "Load old file.")
   (load-file
      (emtg (role filename) (which old)))
   (emt:doc "Start in `old' (which captures contents of old lib)")
   (emtmv:change-state 'old nil
      (list
	 (emtg (role filename) (which old))))
   (emt:doc "Operation: Switch state to `new'")
   (emtmv:change-state 'new nil)
   (unless skip-loading-new
      (emt:doc "Load the new file")
      (load-file
	 (emtg (role filename) (which new)))))

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
	       (assert
		  (equal
		     (eval (emtg (type sym)))
		     (emtg (type value)))
		  t))
	    (function
	       (assert
		  (equal
		     (funcall (emtg (type sym)))
		     (emtg (type value)))
		  t))
	    (set-prop
	       (assert
		  (equal
		     (get
			'foo:var2 
			'foo:properties)
		     (emtg (type value)))
		  t))))))
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
	  (emtmv:new-obarray emtmv:old-obarray emtmv:state
	     emtmv:filename
	     emtmv:t 
	     emtmv:stable-config
	     ;;$$OBSOLESCENT
	     emtmv:extra-affected-syms
	     ;;Altered in loading
	     load-history features
	     ;;Defined in foo-old or foo-new
	     foo:old:unshared
	     foo:new:unshared
	     foo:var1 foo:var2 foo:fun1))
       ;;Insulate functions too
       (flet
	  ((foo:fun1)))
       ;;Insulate properties too
       (emth:let-noprops
	  '(foo:old:unshared
	      foo:new:unshared
	      foo:var1 foo:var2 foo:fun1)))
   "Common surrounders for emtmv tests" )


;;;_ , emtest/testhelp/mocks/libversion
;;Of `emtmv:with-version' and `emtmv:change-state'
;;$$ADD ME:  For `emtmv:with-version', test that it restores even when
;;there is an error (`ignore-errors')
(emt:deftest-3 
   ((of 'emtmv:change-state)
      (:surrounders emtmv:th:surrounders))
   

   (nil
      (progn
	 (emt:doc "Validates: obarray does reflect let bindings.")
	 (emt:doc "Situation: Symbol is not bound globally.")
	 (assert (not (boundp 'invalid-d535)))
	 (let ((invalid-d535 12)) 
	    (emt:doc "Situation: Symbol is bound locally.")
	    (emt:doc "Response: Symbol can be found in obarray.")
	    (assert (intern-soft "invalid-d535" obarray))
	    (assert (intern-soft "invalid-d535"))
	    (emt:doc "Response: Symbol has the right value.")
	    (assert
	       (equal
		  (symbol-value (intern-soft "invalid-d535"))
		  12)
	       t))))


   (nil
      (progn
	 (emt:doc "Situation: Nothing is set up.")
	 (emt:doc "Operation: `emtmv:with-version' given nil.")
	 (assert
	    (emth:gives-error
	       (emtmv:with-version nil nil
		  t)))
	 (emt:doc "Response: Raises error.")))
   (nil
      (progn
	 (emt:doc "Situation: Nothing is set up.")
	 (emt:doc "Operation: `emtmv:with-version' given non-nil.")
	 (emt:doc "In particular: `old'")
	 (assert
	    (emth:gives-error
	       (emtmv:with-version 'old nil
		  t)))
	 (emt:doc "Response: Raises error.")))
   (nil
      (progn
	 (emt:doc "Operation: `emtmv:change-state'")
	 (emt:doc "Param: Invalid `new-state'.")
	 (emt:doc "Response: Makes an error.")
	 (assert
	    (emth:gives-error
	       (emtmv:change-state 'invalid nil )))))
   (nil
      (progn
	 (emt:doc "Operation: `emtmv:change-state'")
	 (emt:doc "Param: Initted with no filename given.")
	 (emt:doc "Response: Makes an error.")
	 (assert
	    (emth:gives-error
	       (emtmv:change-state 'old nil)))))

   (nil
      (progn
	 (emtmv:th:load)

	 (emtmv:with-version 'old nil
	    (emt:doc "Operation: Call with symbol `old'.")
	    (emt:doc "Response: Has the values of old version.")
	    (emtg:narrow ((which old))
	       (emtmv:th:check-all)))
	 

	 (emtmv:with-version 'new nil
	    (emt:doc "Operation: Call with symbol `new'.")
	    (emt:doc "Response: Has the values of new version.")
	    (emtg:narrow ((which new))
	       (emtmv:th:check-all)))

	 (emtmv:with-version 'new nil
	    (emtmv:with-version 'old nil
	       (emt:doc "Operation: Old one nested in new.")
	       (emt:doc "Response: Has the values of old version.")
	       (emtg:narrow ((which old))
		  (emtmv:th:check-all))))

	 
	 (emtmv:with-version 'old nil
	    (emtmv:with-version 'new nil
	       (emt:doc "Operation: New one nested in old.")
	       (emt:doc "Response: Has the values of new version.")
	       (emtg:narrow ((which new))
		  (emtmv:th:check-all))))))

   (nil
      (progn
	 (emtmv:th:load t)
	 (emt:doc "Eval new stuff (instead of loading)")
	 (eval
	    (emtg (role form) (which new)))	 

	 (emtmv:with-version 'old nil
	    (emt:doc "Operation: Call with symbol `old'.")
	    (emt:doc "Response: Has the values of old version.")
	    (emtg:narrow ((which old))
	       (emtmv:th:check-all)))
	 

	 (emtmv:with-version 'new nil
	    (emt:doc "Operation: Call with symbol `new'.")
	    (emt:doc "Response: Has the values of new version.")
	    (emtg:narrow ((which new))
	       (emtmv:th:check-all)))))


   
   (nil
      (progn
	 (emt:doc "Proves: Manually settings variables affects only
      the active version.") 
	 (emt:doc "Proves: Evalling affects only the active version.") 
	 (emtmv:th:load)
	 (let
	    ((value "Another value"))
	    (emt:doc "Situation: In state `new'")
	    (assert (eq (emtmv:t->version emtmv:t) 'new))

	    (emt:doc "Assign to a variable")
	    (setq foo:var1 value)
	    ;;And a function, and a property.

	    (emtmv:with-version 'old nil
	       (emt:doc "Operation: Eval it in `old'.")
	       (emt:doc "Response: Its value in old has not changed.")
	       (emtg:narrow ((which old))
		  (emtmv:th:check-all)))
	 
	    (emtmv:with-version 'new nil
	       (emt:doc "Operation: Eval it in `new'.")
	       (emt:doc "Response: In `new' it has the new value.")
	       (assert
		  (equal foo:var1 value)))

	    (emt:doc "Situation: Still in state `new'")
	    (assert (eq (emtmv:t->version emtmv:t) 'new))

	    (emt:doc "Re-eval the `new' form")
	    (eval
	       (emtg (role form) (which new)))
	    (emtmv:with-version 'new nil
	       (emt:doc "Response: It no longer has that value in `new'.")
	       (assert
		  (not 
		     (equal foo:var1 value))
		  t))

	    (emtmv:with-version 'old nil
	       (emt:doc "Response: Its value in `old' has not changed.")
	       (emtg:narrow ((which old))
		  (emtmv:th:check-all)))
	    
	    (emt:doc "Operation: Change to state `old'.")
	    (emtmv:change-state 'old nil)

	    (emt:doc "Assign to a variable")
	    (setq foo:var1 value)
	    ;;And a function, and a property.

	 
	    (emt:doc "Operation: Eval it in `old'.")
	    (emtmv:with-version 'old nil
	       (emt:doc "Response: In `old' it has the new value.")
	       (assert
		  (equal foo:var1 value)
		  t))	    

	    (emtmv:with-version 'new nil
	       (emt:doc "Response: Its value in `new' has not changed.")
	       (emtg:narrow ((which new))
		  (emtmv:th:check-all)))

	    ))))
;;;_ , emtmv:add-advice
(emt:deftest-3 
   ((of 'emtmv:advise-run-old)
      (:surrounders emtmv:th:surrounders))
   (nil
      (flet
	 ((run-stuff () foo:var1))
	 (emt:doc "Situation: Function run-stuff returns its value of
   `foo:var1'.")
	 (emtmv:th:load)
	 (emt:doc "Situation: In the `new' bindings")
	 (emt:doc "Operation: Advise run-stuff.")
	 (emtmv:add-advice run-stuff 'old)
	 (emt:doc "Operation: Run run-stuff.")
	 (emt:doc "Response: It returns the `old' value of `foo:var1'.")
	 (assert
	    (equal
	       (run-stuff)
	       (emtg (which old)(name var1)(type value)))
	    t))))
;;;_ , emtmv:require-x:th:vc:insert-file
(defun emtmv:require-x:th:vc:insert-file (buf branch-name lib-path)
   "Mock vc function.  Just insert the contents of the respective file."
   (with-current-buffer buf
      ;;For now, assumes that (which old) is meant.
      (insert-file-contents
	 (emtg (role filename)(which old)))

      (setq buffer-file-name
	 (emtg (role filename)(which old)))))

;;;_ , emtmv:require-x:th:vc:insert-file-buggy
(defun emtmv:require-x:th:vc:insert-file-buggy (buf branch-name lib-path)
   "Mock vc function.  Deliberately leaves the filename `nil."
   (with-current-buffer buf
      (setq buffer-file-name nil)))

;;;_ , emtmv:require-x:th:stable-config
(defconst emtmv:require-x:th:stable-config 
   (list
      (list
	 'foo
	 "old"
	 #'emtmv:require-x:th:vc:insert-file
	 '()))
   "Testhelp mock list of info about stable versions of libs" )
;;;_ , emtmv:require-x:th:stable-config-buggy
(defconst emtmv:require-x:th:stable-config-buggy
   (list
      (list
	 'foo
	 "old"
	 #'emtmv:require-x:th:vc:insert-file-buggy
	 '()))
   "Testhelp mock list of info about stable versions of libs" )

;;;_ , emtmv:require-x
(emt:deftest-3 
   ((of 'emtmv:require-x)
      (:surrounders emtmv:th:surrounders))
   (nil
      (let
	 ((emtmv:stable-config emtmv:require-x:th:stable-config))
	 (flet
	    ((run-stuff () foo:var1))
	    (emt:doc "Situation: Function run-stuff returns its value of
   `foo:var1'.")
	    (emt:doc "Operation: require-x on `foo'")
	    (emtmv:require-x '(foo) '(run-stuff))
	    (emt:doc "Response: run-stuff returns the `old' value of
   `foo:var1'.") 
	    (assert
	       (equal
		  (run-stuff)
		  (emtg (which old)(name var1)(type value)))
	       t)
	    
	    (emt:doc "Response: Just inspecting `foo:var1' gives the
	    new value.")
	    (assert
	       (equal
		  foo:var1
		  (emtg (which new)(name var1)(type value)))
	       t)
	    )))

   (nil
      (let
	 ((emtmv:stable-config
	     emtmv:require-x:th:stable-config-buggy))
	 (emt:doc
	    "Buggy config where the vc function does not set the filename")
	 
	 (flet
	    ((run-stuff () foo:var1))
	    (emt:doc "Situation: Function run-stuff returns its value of
   `foo:var1'.")
	    (emt:doc "Operation: require-x on `foo'")
	    (emt:doc "Response: Error.")
	    (assert
	       (emt:gives-error
		  (emtmv:require-x '(foo) '(run-stuff))))

	    )))

   )

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/libversion/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/libversion/tests.el ends here
