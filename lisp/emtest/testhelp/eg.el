;;;_ emtest/testhelp/eg.el --- Examples managements utility for tester

;;;_. Headers
;;;_ , License
;; Copyright (C) 2009  Tom Breton (Tehom)

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

;;;_ , Commentary:

;; 


;;;_ , Requires
(require 'cl)  ;;cl supplies `remove*'
(require 'utility/accumulator)
(require 'utility/pending)
(require 'emtest/testhelp/standard)
(require 'emtest/testhelp/deep-type-checker nil)

;;;_. Body

;;;_ , Variables (OBSOLESCENT all)

;;;_  . emt:eg:all-examples
(defvar emt:eg:all-examples
   ()
   "List of all examples.
Don't change this except thru `emt:eg:narrow' and its worker functions" )
(defvar emt:eg:*all-prpty-makers*
   ()
   "List of property-makers, which will apply properties to any new element.
Each element is of the type `emt:eg:example'" )
;;;_  . emt:eg:tagset
(defconst emt:eg:tagset nil 
   "Tagset, to make general constraints known to `emt:eg'.
This variable is `let' in appropriate scopes" )

;;;_ , Types
;;;_  . Example structure as it occurs on `emt:eg:all-examples'
(defstruct (emt:example. (:type list))
   "The structure of an example on the list"
   definer-id
   tagset
   value
   property-list
   )

;;Properties' values are a list (Prop-sym value)

;;;_  . Helper return-type
(defstruct (emt:eg:helper-rettype. (:copier nil))
   ""
   (value-info () :type (repeat emt:eg:valuedef-type.))
   (property-info () :type (repeat emt:eg:valuedef-type.)))

(defstruct (emt:eg:valuedef-type. (:copier nil))
   "Data for defining an example's value or a property"
   (tagset () :type (repeat *))
   value-form)


;;;_ , Example definer

;;;_  . emt:eg:combine-tagsets
(defsubst emt:eg:combine-tagsets (tagset +tagset)
   ""
   (append +tagset tagset))
;;;_   , Tests
;;It's direct
;;;_  . Handle individual markings
;;;_   , emt:see-item
(defun emt:see-item (tagset +tagset value-form &rest others)
   ""
   (make-emt:eg:helper-rettype.
      :value-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset
	    (emt:eg:combine-tagsets tagset +tagset)
	    :value-form
	    value-form))))

;;;_   , emt:see-doc
(defun emt:see-doc (tagset +tagset doc-form)
   ""
   (make-emt:eg:helper-rettype.
      :property-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset
	    (emt:eg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'documentation ,doc-form)))))

;;;_   , emt:see-transparent-tags
(defun emt:see-transparent-tags (tagset +tagset form)
   ""
   (make-emt:eg:helper-rettype.
      :property-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset
	    (emt:eg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'transparent-tags ',form)))))
;;;_   , emt:see-type-must-be
(defun emt:see-type-must-be (tagset +tagset type-spec)
   ""
   (make-emt:eg:helper-rettype.
      :property-info
      (list
	 (make-emt:eg:valuedef-type.
	    :tagset
	    (emt:eg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'type-must-be ',type-spec)))))
;;;_   , emt:see-group

(defun emt:see-group (tagset +tagset &rest definers)
   ""

   (let
      (  
	 (full-tagset
	    (emt:eg:combine-tagsets tagset +tagset)))
      
      (emt:accumulator:collect
	 #'(lambda (governor &rest d)
	      (let
		 ((fun
		     (case governor
			(item  #'emt:see-item)
			(doc   #'emt:see-doc)
			(group #'emt:see-group)
			(transparent-tags
			   #'emt:see-transparent-tags)
			(type-must-be 
			   #'emt:see-type-must-be))))
		 (apply fun full-tagset d)))
	 definers
	 'emt:eg:helper-rettype.)))


;;;_  . Handle adding definitions
;;;_   , emt:eg:remove-earlier-defs
(defun emt:eg:remove-earlier-defs (id)
   ""
   (setq emt:eg:all-examples
      (remove* id
	 emt:eg:all-examples
	 :key #'emt:example.-definer-id))
   (setq emt:eg:*all-prpty-makers*
      (remove* id
	 emt:eg:*all-prpty-makers*
	 :key #'emt:example.-definer-id)))

;;;_   , emt:eg:propty-match-ctxt-p
(defun emt:eg:propty-match-ctxt-p (tagset prpty-maker)
   ""

   (every
      #'(lambda (one-kv)
	   (emt:eg:some-kv-matches
	      one-kv
	      tagset))
      (emt:example\.-tagset prpty-maker)))

;;;_   , emt:eg:find-properties
(defun emt:eg:find-properties (target-tagset prpty-makers)
   "Given a tagset, return a list of the properties that apply for it"
   
   (let*
      ;;Filter the property-makers by whether their tagset applies to
      ;;example's tagset (Ie, the reverse of finding example by
      ;;tagset)
      ((relevant-props
	  (remove* target-tagset prpty-makers
	     :test-not
	     #'emt:eg:propty-match-ctxt-p)))
      ;;Return a list of all their values in any order.
      (mapcar
	 #'emt:example.-value
	 relevant-props)))

;;;_   , emt:eg:describe-item-context
(defun emt:eg:describe-item-context (v)
   ""
   (format "Item %s tagged %s in eg definition %S"
      (emt:example.-value v)
      (emt:example.-tagset v)
      (emt:example.-definer-id v)))


;;;_   , emt:eg:apply-proplist-specials-to-example
(defun emt:eg:apply-proplist-specials-to-example (props example)
   ""
   (dolist (p props)
      (when
	 (eq (first p) 'type-must-be)
	 ;;Could have fallbacks here.
	 (unless (featurep 'emtest/testhelp/deep-type-checker)
	    (error "Type-checking requires deep-type-checker"))
	 (condition-case err
	    (emty:check-f
	       (emt:example.-value example)
	       (second p))
	    (wrong-type-argument
	       (signal
		  (car err)
		  (list
		     (concat
			(emt:eg:describe-item-context example)
			(format " is not a %S" (second p))))))))))


;;;_   , emt:eg:apply-prpty-makers-to-examples
(defun emt:eg:apply-prpty-makers-to-examples (prpty-makers examples)
   "Apply the property-makers to each example whose tagset matches.
Return the new list of examples."

   (mapcar
      ;;Adds appropriate properties to one example.
      #'(lambda (x)
	   (let
	      ((props
		  (emt:eg:find-properties 
		     (emt:example\.-tagset x)
		     prpty-makers)))

	      (emt:eg:apply-proplist-specials-to-example props x)
	      
	      (callf append 
		 (emt:example.-property-list x)
		 props)
	      x))
      examples))

;;;_   , emt:eg:see-new-prpty-makers
(defun emt:eg:see-new-prpty-makers (prpty-makers)
   ""
   ;;Apply the property-makers to existing examples. 
   (setq emt:eg:all-examples
      (emt:eg:apply-prpty-makers-to-examples
	     prpty-makers
	     emt:eg:all-examples))

   ;;Record the property-makers.
   (setq emt:eg:*all-prpty-makers*
      (append prpty-makers emt:eg:*all-prpty-makers*)))

;;;_   , emt:eg:see-a-new-example
(defun emt:eg:see-a-new-example (x)
   ""
   (push x emt:eg:all-examples))
;;;_    . Tests
;;It's direct

;;;_   , emt:eg:see-new-examples (Obsolete?)
'
(defun emt:eg:see-new-examples (examples)
   ""
   ;;Assume examples have already been given properties.
   (setq emt:eg:all-examples
      (append
	 examples
	 emt:eg:all-examples)))

;;;_   , emt:eg:valuedef->example

(defun emt:eg:valuedef->example (valuedef id all-prpty-makers)
   ""
   (let* 
      (  (item-tagset 
	    (emt:eg:valuedef-type.-tagset valuedef))
	 (props
	    (emt:eg:find-properties item-tagset
	       all-prpty-makers))
	 (tagset
	    (emt:eg:tagset-strip-transparents 
	       item-tagset
	       props)) 
	 (value-form
	    (emt:eg:narrow-f
	       `',tagset
	       ;;body - the form, inside an emt:eg:narrow that
	       ;;constrains the tagset it uses to find other
	       ;;examples.
	       (list 
		  (emt:eg:valuedef-type.-value-form valuedef))))
	 (value
	    (eval value-form))
	 (x
	    (make-emt:example.
	       :definer-id 
	       id
	       :tagset    
	       item-tagset
	       :value
	       value 
	       :property-list 
	       props)))

      (emt:eg:apply-proplist-specials-to-example props x)
      x))

;;;_  . Error symbols

;;;_   , emt:eg:err:not-found
(put 'emt:eg:err:not-found 'error-conditions
   '(error emt:eg:err emt:eg:err:not-found))
(put 'emt:eg:err:not-found 'error-message
   "Could not find an example")

;;;_   , emt:eg:err:too-many
(put 'emt:eg:err:too-many 'error-conditions
   '(error emt:eg:err emt:eg:err:too-many))
(put 'emt:eg:err:too-many 'error-message
   "More than one example found")

;;;_  . emt:eg:try-valuedef->example (Obsolete)
'
(defun emt:eg:try-valuedef->example (v id all-prpty-makers)
   ""
   (condition-case err
      (emt:eg:see-a-new-example
	 (emt:eg:valuedef->example v id all-prpty-makers))
      ('emt:eg:err:not-found
	 (push v emt:eg:delayed-examples))))



;;;_  . emt:eg:try-valuedef->example-2
(defun emt:eg:try-valuedef->example-2 (v id)
   ""
   (condition-case err
      (progn
	 (emt:eg:see-a-new-example
	    (emt:eg:valuedef->example v id emt:eg:*all-prpty-makers*))
	 '())
      ('emt:eg:err:not-found
	 (list v))))


;;;_  . emt:eg:valuedef->property
(defun emt:eg:valuedef->property (valuedef id)
   ""

   (let
      (  (tagset 
	    (emt:eg:valuedef-type.-tagset    valuedef))
	 (value-form
	    (emt:eg:valuedef-type.-value-form valuedef)))
	      
      (make-emt:example.
	 :definer-id 
	 id
	 :tagset    
	 tagset
	 :value
	 (eval value-form) 
	 :property-list 
	 ())))

;;;_  . emt:eg:tagset-strip

(defun emt:eg:tagset-strip (tagset transparent-tags)
   ""
   (remove* transparent-tags tagset 
      :test
      #'(lambda
	   (tts 1-tagset)
	   (memq
	      (if
		 (consp 1-tagset)
		 (car 1-tagset)
		 1-tagset)
	      tts))))

;;;_  . emt:eg:tagset-strip-transparents
(defun emt:eg:tagset-strip-transparents (tagset props)
   "Return TAGSET with any transparent tags removed.

TAGSET must be a kv-list suitable for `emt:eg:narrow'.
Transparent tags are exactly those tags named in the
`transparent-tags' property (if any) in PROPS."

   '
   (let
      ((cell (assoc 'transparent-tags props)))
      (if cell (second cell) ()))
   
   (let*
      (  ;;Get them even if they are in several applying properties.
	 (transparent-tags
	       (apply #'append
		  (mapcar 
		     #'(lambda (cell)
			  (if (eq (first cell) 'transparent-tags)
			     (second cell)
			     ()))
		     props)))
	 ;;Subtract the tags named in transparent-tags.
	 (new-tagset
	    (emt:eg:tagset-strip tagset transparent-tags)))
      
      new-tagset))

;;;_  . emt:eg:define-f
(defun emt:eg:define-f (id group-args)
   ""
   (unless (symbolp id)
      (error "emt:eg:define ID must be a symbol"))
   (let
      ((item-maker-list (apply #'emt:see-group () group-args)))

      ;;Remove existing things that had this definer-id.  They're
      ;;from an earlier run of this definer.
      (emt:eg:remove-earlier-defs id)

      ;;We do this on a list, for historical reasons.
      ;;Put any new property-makers in place. They are stored in
      ;;special variable emt:eg:*all-prpty-makers*
      (emt:eg:see-new-prpty-makers
	 (mapcar
	    #'(lambda (v)
		 (emt:eg:valuedef->property v id))
	    (emt:eg:helper-rettype.-property-info item-maker-list)))

      (pending:do-all-f
	 (emt:eg:helper-rettype.-value-info item-maker-list)
	 #'emt:eg:try-valuedef->example-2
	 (list id)
	 #'(lambda (delayed id)
	      (format
		 "In eg definition %S, some examples' values could not be resolved: %s"
		 id
		 (mapconcat
		    #'(lambda (v)
			 (format "%s within %s"
			    (emt:eg:valuedef-type.-value-form v)
			    (emt:eg:valuedef-type.-tagset v)))
		    delayed
		    "\n"))))))


;;;_  . emt:eg:define

(defmacro emt:eg:define (id &rest group-args)
   "Define examples.
DEFINERS are implicitly in a `group' structure.
Should document the structure here."

   `(emt:eg:define-f ',id ',group-args))

;;;_  . emt:eg:define+

(defmacro emt:eg:define+ (&rest group-args)
   ""
   
   `(let
       ((emt:eg:*all-prpty-makers*)
	  (emt:eg:all-examples))
       (emt:eg:define-f nil ',group-args)
       emt:eg:all-examples))


;;;_ , Use examples

;;;_  . emt:eg:kv-matches-p
(defun emt:eg:kv-matches-p (kv filter)
   ""

   (cond
      ((symbolp kv)
	 (if
	    (symbolp filter)
	    (eq kv filter)
	    (error 
	       "Filter %s needs to be a symbol.  There's no value
for it to match" filter)))
		      
      ((consp kv)
	 (cond
	    ((symbolp filter)
	       (eq (car kv)  filter))
	    (t
	       (equal kv filter))))
      (t
	 (error "Key %s is neither symbol nor list" kv))))

;;;_  . emt:eg:some-kv-matches

(defun emt:eg:some-kv-matches (one-kv tagset)
   ""
   (some
      #'(lambda (kv)
	   (emt:eg:kv-matches-p kv one-kv))
      tagset))

;;;_  . emt:eg:filter-one
(defun emt:eg:filter-one (list one-kv)
   ""
   (remove* one-kv list
      :test-not
      #'(lambda (one-kv item)
	   (emt:eg:some-kv-matches 
	      one-kv 
	      (emt:example\.-tagset item)))))

;;;_  . emt:eg:filter
(defun emt:eg:filter (list kv-list)
   ""

   (reduce
      #'emt:eg:filter-one
      kv-list
      ;;In `reduce', this occurs as arg 1 to FUNC.  List elements are
      ;;arg 2. 
      :initial-value list))


;;;_  . emt:eg:get-value
(defun emt:eg:get-value (list kv-list)
   ""
   
   (let
      ((vals (emt:eg:filter list kv-list)))
      
      (case (length vals)
	 (0 
	    (signal 'emt:eg:err:not-found kv-list))
	 (1 
	    (emt:example.-value (car vals)))
	 (t 
	    (signal 'emt:eg:err:too-many kv-list)))))

;;;_  . emt:eg
;;;###autoload
(defmacro emt:eg (&rest kv-list)
   ""
   ;;Call `emt:eg:value' instead.
   `(emt:eg:get-value emt:eg:all-examples 
       (append 
	  ;;New
	  (emt:eg:tagset-strip emt:eg:tagset 
	     ',(mapcar
		#'(lambda (kv)
		     (if (consp kv) (car kv) kv))
		kv-list))
	  ;;Original
	  ;;emt:eg:tagset 

	  ',kv-list)))
;;;_  . emt:eg:value
;;;###autoload
(defmacro* emt:eg:value (&key narrow ignore-tags)
   "
Takes keywords
  NARROW - A tagset to narrow by.
  IGNORE-TAGS - tags to be ignored."
   `(emt:eg:get-value emt:eg:all-examples
       (append
	  (emt:eg:tagset-strip emt:eg:tagset ',ignore-tags)
	  ',narrow)))
;;;_  . emt:eg+
(defmacro emt:eg+ (examples &rest kv-list)
   ""
   ;;Merge this and `emt:eg:value' behind one helper.
   `(emt:eg:get-value ,examples
       (append 
	  ;;New
	  (emt:eg:tagset-strip emt:eg:tagset 
	     ',(mapcar
		#'(lambda (kv)
		     (if (consp kv) (car kv) kv))
		kv-list))
	  ',kv-list)))

;;;_ , Narrowing the tagset
;;;_  . emt:eg:narrow-f
(defun emt:eg:narrow-f (qt-kv-filter body)
   "Execute BODY in a particular examples tagset.
Purpose: For programmability.
Takes it arguments literally."
   
   `(let
       ((emt:eg:tagset 
	   (append ,qt-kv-filter emt:eg:tagset)))
       ,@body))
;;;_  . emt:eg:ignore-tags
(defmacro emt:eg:ignore-tags (transparent-tags &rest body)
   ""
   '  ;;Didn't work right.
   (emt:eg:narrow-f 
      `',(emt:eg:tagset-strip emt:eg:tagset transparent-tags)
      body)

   `(let
      ((emt:eg:tagset 
	  (emt:eg:tagset-strip emt:eg:tagset ',transparent-tags)))
      ,@body))

;;;_  . emt:eg:narrow
;;;###autoload
(defmacro* emt:eg:narrow (kv-filter &rest body)
   "Execute BODY in a particular examples tagset.
Purpose: For consise use inside test code."
   (emt:eg:narrow-f `',kv-filter body))


;;;_  . emt:eg:with

(defmacro* emt:eg:with (examples kv-filter &rest body)
   "Execute BODY in a particular examples tagset.
Purpose: For consise use inside test code."
   `(let
      ((emt:eg:all-examples ,examples))
      ,(emt:eg:narrow-f `',kv-filter body)))

;;;_ , Looping
;;;_  . emt:eg:map
;;;###autoload
(defmacro emt:eg:map (tag name &rest body)
   ""
   (let
      ((name (or name (gensym))))
      `(let
	  ((arg-values (emt:eg:all-tag-args ',tag)))
	  (emt:doc 
	     (concat 
		"EG operation: Loop over values of "
		(prin1-to-string ',tag)))
	  (mapcar
	     #'(lambda (,name)
		  ,(emt:eg:narrow-f
		      `(list (list ',tag ,name))
		      `(
			  (emt:doc 
			     (concat 
				(prin1-to-string ',tag)
				" = " 
				(prin1-to-string ,name)))
			  ,@body)))
	  
	     arg-values))))

;;;_ , Utilities
;;;_  . emt:eg:all-tags
(defun emt:eg:all-tags (&optional example-list)
   "Return a list of all tags used in the existing examples"
   
   (remove-duplicates
      (apply #'append
	 (mapcar
	    #'(lambda (x)
		 (mapcar
		    #'(lambda (kv)
			 (if (consp kv)
			    (car kv)
			    kv))
		    (emt:example.-tagset x)))
	    (or example-list emt:eg:all-examples)))))

;;;_  . emt:eg:all-tag-args
(defun emt:eg:all-tag-args (tag)
   "Return a list of all values for a given tag.
Returns `no-arg' if the key takes no argument."
   (catch 'emt:eg:tag-no-arg
      (remove-duplicates
	 (apply #'append
	    (mapcar
	       #'(lambda (x)
		    (apply #'append
		       (mapcar
			  #'(lambda (kv)
			       (if (consp kv)
				  (if (eq tag (car kv))
				     (list (second kv))
				     ())
				  (if (eq tag kv)
				     (throw 'emt:eg:tag-no-arg 'no-arg)
				     ())))
			  (emt:example.-tagset x))))
	       (emt:eg:filter 
		  emt:eg:all-examples
		  emt:eg:tagset))))))

;;;_  . emt:eg:boundp
;;;###autoload
(defun emt:eg:boundp (kv-pair)
   "Return non-nil if there exists at least one item that would be
selected by KV-PAIR."

   (member (second kv-pair) (emt:eg:all-tag-args (first kv-pair))))

;;;_  . emt:eg:ambiguous-p
(defun emt:eg:ambiguous-p (tagset)
   "Return whether or not TAGSET is ambiguous on the existing
examples.
TAGSET should not yet refer to an example."

   ;;By filtering its heads combinatorily.  0 is safe forever, >1 is
   ;;safe but explore further, 1 means ambiguous, except at root.
   ;;Some tags imply others, so don't consider them alone. `project'
   ;;and `library' are always implied.
   (let*
      ()
      
      ))
;;;_   , Tests
;;Not yet.

;;;_  . emt:eg:see-err Include errors in examples
(defun emt:eg:see-err (form)
   "Capture the error raised by FORM.
Use this inside eg, otherwise you risk capturing errors that eg uses
internally.
FORM must be a form that, when evalled, raises an error"
   
   (condition-case err
      (progn
	 (eval form)
	 ;;If it failed to raise an error, raise a (meta-)error.
	 (error "Form failed to raise error: %s" form))

      ;;If it's an eg error, re-raise it (This is important to do)
      (emt:eg:err (signal (car err)(cdr err)))
      ;;Otherwise trap it and pass on its value
      (error err)))



;;;_ , Editing help
;;;_  . Fill in an example-spec from tags
;;That's being done in pcmpl-emtest

;;;_  . From `emt:eg', go to its definition
;;At least, if it's in the same file.
;;Can easily find definer-id

;;;_  . From example, go to file it refers to
;;Like the above, just one more step.
;;Would help to look at a "type" property of items.

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/eg)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/eg.el ends here
