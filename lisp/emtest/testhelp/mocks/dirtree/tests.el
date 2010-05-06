;;;_ emtest/testhelp/mocks/dirtree/tests.el --- Tests for mocks/dirtree

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

(require 'emtest/testhelp/eg)
(require 'emtest/testhelp/mocks/dirtree)
;;;_. Body
;;;_ , Master examples

(defconst emtmd:thd:master-examples 
   (expand-file-name "examples/" load-file-name)
   "The path to the master copy of the examples" )
;;;_ , Data

;;Invariant:
;;  Everything has a (permutation 0).
;;  No topdir groups compare equal to each other.
;;  Each group has exactly one that's marked (flavor vanilla).
(defconst emtmd:thd:examples
   (emt:eg:define+ ;;xmp:nfnesor01xe0
      ((project emtest)
	 (library mocks))
      (group
	 ((topdir "0"))
	 (doc () "dir 0 is empty")
	 (item ((type nametree)(flavor vanilla))
	    (emtmd:make-repr-contents)))
      
      (group
	 ((topdir "1"))
	 (doc () "dir 1 has 1 subfile, `a'")
	 (item ((type nametree)(flavor vanilla))
	    (emtmd:make-repr-contents 
	       (emtmd:make-repr-file "a"))))
      (group
	 ((topdir "2"))
	 (doc () "dir 2 is a tree of depth 2")
	 (item ((type nametree)(permutation 0)(flavor vanilla))
	    (emtmd:make-repr-contents 
	       (emtmd:make-repr-file "a")
	       (emtmd:make-repr-dir "b-dir" 
		  (emtmd:make-repr-file "b2") 
		  (emtmd:make-repr-file "b1"))))
	 (item ((type nametree)(permutation 1))
	    (emtmd:make-repr-contents 
	       (emtmd:make-repr-dir "b-dir" 
		  (emtmd:make-repr-file "b2") 
		  (emtmd:make-repr-file "b1"))
	       (emtmd:make-repr-file "a"))))

      (group
	 ((topdir "3"))
	 (doc () "dir 1 has 1 subfile, `a' with contents")
	 (item ((type nametree)
		  (contents-right t)
		  (source string)
		  (flavor vanilla))
	    (emtmd:make-repr-contents 
	       (emtmd:make-repr-file "a" '(:string "abc"))))
	 (item ((type nametree)(contents-right t)(source file))
	    (emtmd:make-repr-contents 
	       (emtmd:make-repr-file "a" 
		  `(:file "3/a" 
		      :dir emtmd:thd:master-examples))))
	 (item ((type nametree)(contents-right nil)(source string))
	    (emtmd:make-repr-contents 
	       (emtmd:make-repr-file "a" '(:string "xyz"))))
	 )
      ))


;;;_ , 


;;;_ , emtmd:repr-contents-equal

;;Pre-requisite: validate nametree examples
;;Validate that all example nametrees are the expected type
;;This happens implicitly inside `emtmd:repr-contents-equal'

(emt:deftest-3 emtmd:repr-contents-equal
   (nil
      (progn
	 (emt:doc "Shows: An example's nametree is equal to itself.")
	 (emt:eg:with emtmd:thd:examples ()
	    (emt:eg:map topdir prefix
	       (assert
		  (emtmd:repr-contents-equal
		     (emt:eg (type nametree)(flavor vanilla))
		     (emt:eg (type nametree)(flavor vanilla)))
		  t)))))
   (nil
      (progn
	 (emt:doc 
	    "Shows: an example's nametree compare true to a permutation of itself.")
	 (emt:eg:with emtmd:thd:examples ()
	    (emt:eg:map topdir prefix
	       (let
		  ((perms  
		      ;;This works OK even if the permutation key
		      ;;isn't present for any
		      (emt:eg:all-tag-args 'permutation)))
		  (when
		     (> (length perms) 1)
		     ;;For now, there are at most 2 permutations so we
		     ;;don't loop over the list and we know their
		     ;;names (So there's no need to get fancy with
		     ;;`emt:eg:value')
		     (assert
			(emtmd:repr-contents-equal
			   (emt:eg (type nametree)(permutation 0))
			   (emt:eg (type nametree)(permutation 1)))
			t)))))))
   (nil
      (progn
	 (emt:doc "An example's nametree is not equal to any others.")
	 (emt:doc "Why: We constructed the examples to all be distinct.")
	 (emt:eg:with emtmd:thd:examples ()
	    (emt:eg:map topdir prefix-a
	       (let
		  ((nametree-a
		      (emt:eg (type nametree)(flavor vanilla))))
		  (emt:doc "Loop over examples, filtering out the one
   the outer loop chose.")
		  (emt:eg:with emtmd:thd:examples ()
		     (emt:eg:map topdir prefix-b
			(unless (string= prefix-a prefix-b)
			   (assert
			      (not
				 (emtmd:repr-contents-equal
				    nametree-a
				    (emt:eg (type nametree)(flavor vanilla))))
			      t)))))))))

   )

;;;_ , emtmd:get-repr-contents
'
(emt:eg:with emtmd:thd:examples ()
   (emt:doc "Situation: Using master examples")
   (emt:doc "Operation: Call emtmd:get-repr-contents on examples")
   (let
      ((nametree
	  (emtmd:get-repr-contents
	     (expand-file-name "3" emtmd:thd:master-examples))))
      (emt:doc "Result: Matches what's expected.")
      (assert
	 (emtmd:repr-contents-equal nametree 
	    (emt:eg (project emtest)(library mocks)(topdir "3")
	       (flavor vanilla)
	       (type nametree)))
	 t)))

(emt:deftest-3 
   ((of 'emtmd:get-repr-contents)
      (:surrounders
	 '(
	    (emt:eg:with emtmd:thd:examples 
	       ((project emtest)(library mocks)))
	     (emt:eg:map topdir prefix))))
   
   ;;Pre-requisite: `emtmd:repr-contents-equal'

   (()
      (progn
	 (emt:doc "Situation: Using master examples")
	 (emt:doc "Operation: Call emtmd:get-repr-contents on directory")
	 (let
	    ((nametree
		(emtmd:get-repr-contents
		   (expand-file-name prefix emtmd:thd:master-examples))))
	    (emt:doc "Result: Matches what's expected.")
	    (assert
	       (emtmd:repr-contents-equal nametree 
		  (emt:eg (type nametree)(flavor vanilla)))
	       t)
	       
	    (emt:doc "Cross-validation: Does not match any another's
   nametree.") 
	    (emt:doc "Why: We constructed the examples to all be distinct.")
	    (let
	       ((nametree-a
		   (emt:eg (type nametree)(flavor vanilla))))
	       (emt:eg:with emtmd:thd:examples ()
		  (emt:eg:map topdir prefix-b
		     (unless (string= prefix prefix-b)
			(assert
			   (not
			      (emtmd:repr-contents-equal
				 nametree-a
				 (emt:eg (type nametree)(flavor vanilla))))
			   t))))))))

   (()
      (progn
	 (emt:doc "Prove: Contents retrieved from the file match")
	 (emt:doc "Situation: An example whose contents are from
      the file in question")
	 (emt:doc "Operation: Call emtmd:get-repr-contents on directory")
	 (let
	    (
	       (has-sources  
		  (emt:eg:all-tag-args 'source)))
	    (when
	       (> (length has-sources) 1)
	       (let
		  ((nametree
		      (emtmd:get-repr-contents
			 (expand-file-name prefix
			    emtmd:thd:master-examples))))
		  (emt:doc "Result: Matches the contents.")
		  (assert
		     (emtmd:repr-contents-equal nametree 
			(emt:eg 
			   (type nametree)
			   (source file)))
		     t))))))
   
   (()
      (progn
	 (emt:doc "Prove: Wrong contents don't match")
	 (emt:doc "Situation: An example whose contents are wrong")
	 (emt:doc "Operation: Call emtmd:get-repr-contents on directory")
	 (let
	    (
	       (has-wrong  
		  ;;This works OK even if the permutation key
		  ;;isn't present for any
		  (emt:eg:all-tag-args 'contents-right)))
	    (when
	       (> (length has-wrong) 1)
	       (let
		  ((nametree
		      (emtmd:get-repr-contents
			 (expand-file-name prefix
			    emtmd:thd:master-examples))))
		  (emt:doc "Result: Mismatches the known-wrong contents.")
		  (assert
		     (not
			(emtmd:repr-contents-equal nametree 
			   (emt:eg 
			      (type nametree)
			      (contents-right nil))))
		     t)))
	    )))

   )
;;;_ , emtmd:with-dirtree

;;Initted via a repr, emtmd:with-dirtree should have the same
;;structure. (fetch it and compare it to the original)

;;File contents?  repr would need args to make them.  They'd be the
;;same args as filebuf content-makers.  Then comparison would need to
;;consider contents.  Just make a further example dir with non-empty
;;files.
(emt:deftest-3 
   ((of 'emtmd:with-dirtree)
      (:surrounders
	 '(
	     
	     )))

   (nil
      (emt:eg:with emtmd:thd:examples 
	 ((project emtest)(library mocks))
	 (emt:eg:map topdir prefix
	    (emtmd:with-dirtree
	       (:repr (emt:eg (type nametree)(flavor vanilla)))
	       (emt:doc "Param: Spec is a repr.")
	       (emt:doc "Operation: Get directory contents.")
	       (let* 
		  ((repr-contents
		      (emtmd:get-repr-contents ".")))
	    
		  (emt:doc "Response: Contents are the same as the original repr.")
		  (assert
		     (emtmd:repr-contents-equal 
			repr-contents
			(emt:eg 
			   (type nametree)
			   (flavor vanilla)))
		     t))
	 
	 

	       )))))



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/mocks/dirtree/tests)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/mocks/dirtree/tests.el ends here
