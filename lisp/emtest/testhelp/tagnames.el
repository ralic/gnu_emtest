;;;_ emtest/testhelp/tagnames.el --- Examples managements utility for tester

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

;;;_  . emtg:all-examples
(defvar emtg:all-examples
   ()
   "List of all examples.
Don't change this except thru `emtg:narrow' and its worker functions" )
(defvar emtg:*all-prpty-makers*
   ()
   "List of property-makers, which will apply properties to any new element.
Each element is of the type `emtg:example'" )
;;;_  . emtg:tagset
(defconst emtg:tagset nil 
   "Tagset, to make general constraints known to `emtg'.
This variable is `let' in appropriate scopes" )

;;;_ , Types
;;;_  . Example structure as it occurs on `emtg:all-examples'
(defstruct (emtg:example
	    (:constructor emtg:make-example)
	    (:conc-name emtg:example->)
	    (:copier nil)
	    (:type list))
   "The structure of an example on the list"
   definer-id
   tagset
   value
   property-list
   )

;;Properties' values are a list (Prop-sym value)

;;;_  . Helper return-type
(defstruct (emtg:helper-rettype
	    (:constructor emtg:make-helper-rettype)
	    (:conc-name emtg:helper-rettype->)
	    (:copier nil))
   ""
   (value-info () :type (repeat emtg:valuedef-type))
   (property-info () :type (repeat emtg:valuedef-type)))

(defstruct (emtg:valuedef-type
	    (:constructor emtg:make-valuedef-type)
	    (:conc-name emtg:valuedef-type->)
	    (:copier nil))
   "Data for defining an example's value or a property"
   (tagset () :type (repeat *))
   value-form)


;;;_ , Example definer

;;;_  . emtg:combine-tagsets
(defsubst emtg:combine-tagsets (tagset +tagset)
   ""
   (let* 
      ;;Remove any occurence of nil or t in the added tagset.  They're
      ;;easy to get in accidentally and generaly unwanted.
      ((clean-tagset+ 
	  (delq t
	     (remq nil +tagset))))
      (append clean-tagset+ tagset)))

;;;_   , Tests
;;It's direct
;;;_  . Handle individual markings
;;;_   , emt:see-item
(defun emt:see-item (tagset +tagset value-form &rest others)
   ""
   (emtg:make-helper-rettype
      :value-info
      (list
	 (emtg:make-valuedef-type
	    :tagset
	    (emtg:combine-tagsets tagset +tagset)
	    :value-form
	    value-form))))

;;;_   , emt:see-doc
(defun emt:see-doc (tagset +tagset doc-form)
   ""
   (emtg:make-helper-rettype
      :property-info
      (list
	 (emtg:make-valuedef-type
	    :tagset
	    (emtg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'documentation ,doc-form)))))

;;;_   , emt:see-transparent-tags
(defun emt:see-transparent-tags (tagset +tagset form)
   ""
   (emtg:make-helper-rettype
      :property-info
      (list
	 (emtg:make-valuedef-type
	    :tagset
	    (emtg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'transparent-tags ',form)))))
;;;_   , emt:see-type-must-be
(defun emt:see-type-must-be (tagset +tagset type-spec)
   ""
   (emtg:make-helper-rettype
      :property-info
      (list
	 (emtg:make-valuedef-type
	    :tagset
	    (emtg:combine-tagsets tagset +tagset)
	    :value-form
	    `(list 'type-must-be ',type-spec)))))
;;;_   , emt:see-group

(defun emt:see-group (tagset +tagset &rest definers)
   ""

   (let
      (  
	 (full-tagset
	    (emtg:combine-tagsets tagset +tagset)))
      
      (utiacc:collect
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
	 'emtg:helper-rettype)))


;;;_  . Handle adding definitions
;;;_   , emtg:remove-earlier-defs
(defun emtg:remove-earlier-defs (id)
   ""
   (setq emtg:all-examples
      (remove* id
	 emtg:all-examples
	 :key #'emtg:example->definer-id))
   (setq emtg:*all-prpty-makers*
      (remove* id
	 emtg:*all-prpty-makers*
	 :key #'emtg:example->definer-id)))

;;;_   , emtg:propty-match-ctxt-p
(defun emtg:propty-match-ctxt-p (tagset prpty-maker)
   ""

   (every
      #'(lambda (one-kv)
	   (emtg:some-kv-matches
	      one-kv
	      tagset))
      (emtg:example->tagset prpty-maker)))

;;;_   , emtg:find-properties
(defun emtg:find-properties (target-tagset prpty-makers)
   "Given a tagset, return a list of the properties that apply for it"
   
   (let*
      ;;Filter the property-makers by whether their tagset applies to
      ;;example's tagset (Ie, the reverse of finding example by
      ;;tagset)
      ((relevant-props
	  (remove* target-tagset prpty-makers
	     :test-not
	     #'emtg:propty-match-ctxt-p)))
      ;;Return a list of all their values in any order.
      (mapcar
	 #'emtg:example->value
	 relevant-props)))

;;;_   , emtg:describe-item-context
(defun emtg:describe-item-context (v)
   ""
   (format "Item %s tagged %s in tagname definition %S"
      (emtg:example->value v)
      (emtg:example->tagset v)
      (emtg:example->definer-id v)))


;;;_   , emtg:apply-proplist-specials-to-example
(defun emtg:apply-proplist-specials-to-example (props example)
   ""
   (dolist (p props)
      (when
	 (eq (first p) 'type-must-be)
	 ;;Could have fallbacks here.
	 (unless (featurep 'emtest/testhelp/deep-type-checker)
	    (error "Type-checking requires deep-type-checker"))
	 (condition-case err
	    (emty:check-f
	       (emtg:example->value example)
	       (second p))
	    (wrong-type-argument
	       (signal
		  (car err)
		  (list
		     (concat
			(emtg:describe-item-context example)
			(format " is not a %S" (second p))))))))))


;;;_   , emtg:apply-prpty-makers-to-examples
(defun emtg:apply-prpty-makers-to-examples (prpty-makers examples)
   "Apply the property-makers to each example whose tagset matches.
Return the new list of examples."

   (mapcar
      ;;Adds appropriate properties to one example.
      #'(lambda (x)
	   (let
	      ((props
		  (emtg:find-properties 
		     (emtg:example->tagset x)
		     prpty-makers)))

	      (emtg:apply-proplist-specials-to-example props x)
	      
	      (callf append 
		 (emtg:example->property-list x)
		 props)
	      x))
      examples))

;;;_   , emtg:see-new-prpty-makers
(defun emtg:see-new-prpty-makers (prpty-makers)
   ""
   ;;Apply the property-makers to existing examples. 
   (setq emtg:all-examples
      (emtg:apply-prpty-makers-to-examples
	     prpty-makers
	     emtg:all-examples))

   ;;Record the property-makers.
   (setq emtg:*all-prpty-makers*
      (append prpty-makers emtg:*all-prpty-makers*)))

;;;_   , emtg:see-a-new-example
(defun emtg:see-a-new-example (x)
   ""
   (push x emtg:all-examples))
;;;_    . Tests
;;It's direct

;;;_   , emtg:see-new-examples (Obsolete?)
'
(defun emtg:see-new-examples (examples)
   ""
   ;;Assume examples have already been given properties.
   (setq emtg:all-examples
      (append
	 examples
	 emtg:all-examples)))

;;;_   , emtg:valuedef->example

(defun emtg:valuedef->example (valuedef id all-prpty-makers)
   ""
   (let* 
      (  (item-tagset 
	    (emtg:valuedef-type->tagset valuedef))
	 (props
	    (emtg:find-properties item-tagset
	       all-prpty-makers))
	 (tagset
	    (emtg:tagset-strip-transparents 
	       item-tagset
	       props)) 
	 (value-form
	    (emtg:narrow-f
	       `',tagset
	       ;;body - the form, inside an emtg:narrow that
	       ;;constrains the tagset it uses to find other
	       ;;examples.
	       (list 
		  (emtg:valuedef-type->value-form valuedef))))
	 (value
	    (eval value-form))
	 (x
	    (emtg:make-example
	       :definer-id 
	       id
	       :tagset    
	       item-tagset
	       :value
	       value 
	       :property-list 
	       props)))

      (emtg:apply-proplist-specials-to-example props x)
      x))

;;;_  . Error symbols

;;;_   , emtg:err:not-found
(put 'emtg:err:not-found 'error-conditions
   '(error emtg:err emtg:err:not-found))
(put 'emtg:err:not-found 'error-message
   "Could not find an example")

;;;_   , emtg:err:too-many
(put 'emtg:err:too-many 'error-conditions
   '(error emtg:err emtg:err:too-many))
(put 'emtg:err:too-many 'error-message
   "More than one example found")

;;;_  . emtg:try-valuedef->example (Obsolete)
'
(defun emtg:try-valuedef->example (v id all-prpty-makers)
   ""
   (condition-case err
      (emtg:see-a-new-example
	 (emtg:valuedef->example v id all-prpty-makers))
      ('emtg:err:not-found
	 (push v emtg:delayed-examples))))



;;;_  . emtg:try-valuedef->example-2
(defun emtg:try-valuedef->example-2 (v id)
   ""
   (condition-case err
      (progn
	 (emtg:see-a-new-example
	    (emtg:valuedef->example v id emtg:*all-prpty-makers*))
	 '())
      ('emtg:err:not-found
	 (list v))))


;;;_  . emtg:valuedef->property
(defun emtg:valuedef->property (valuedef id)
   ""

   (let
      (  (tagset 
	    (emtg:valuedef-type->tagset    valuedef))
	 (value-form
	    (emtg:valuedef-type->value-form valuedef)))
	      
      (emtg:make-example
	 :definer-id 
	 id
	 :tagset    
	 tagset
	 :value
	 (eval value-form) 
	 :property-list 
	 ())))

;;;_  . emtg:tagset-strip

(defun emtg:tagset-strip (tagset transparent-tags)
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

;;;_  . emtg:tagset-strip-transparents
(defun emtg:tagset-strip-transparents (tagset props)
   "Return TAGSET with any transparent tags removed.

TAGSET must be a kv-list suitable for `emtg:narrow'.
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
	    (emtg:tagset-strip tagset transparent-tags)))
      
      new-tagset))

;;;_  . emtg:define-f
(defun emtg:define-f (id group-args)
   ""
   (unless (symbolp id)
      (error "emtg:define ID must be a symbol"))
   (let
      ((item-maker-list (apply #'emt:see-group () group-args)))

      ;;Remove existing things that had this definer-id.  They're
      ;;from an earlier run of this definer.
      (emtg:remove-earlier-defs id)

      ;;We do this on a list, for historical reasons.
      ;;Put any new property-makers in place. They are stored in
      ;;special variable emtg:*all-prpty-makers*
      (emtg:see-new-prpty-makers
	 (mapcar
	    #'(lambda (v)
		 (emtg:valuedef->property v id))
	    (emtg:helper-rettype->property-info item-maker-list)))

      (pending:do-all-f
	 (emtg:helper-rettype->value-info item-maker-list)
	 #'emtg:try-valuedef->example-2
	 (list id)
	 #'(lambda (delayed id)
	      (format
		 "In tagname definition %S, some examples' values could not be resolved: %s"
		 id
		 (mapconcat
		    #'(lambda (v)
			 (format "%s within %s"
			    (emtg:valuedef-type->value-form v)
			    (emtg:valuedef-type->tagset v)))
		    delayed
		    "\n"))))))


;;;_  . emtg:define

(defmacro emtg:define (id &rest group-args)
   "Define examples.
DEFINERS are implicitly in a `group' structure.
Should document the structure here."

   `(emtg:define-f ',id ',group-args))

;;;_  . emtg:define+

(defmacro emtg:define+ (&rest group-args)
   ""
   
   `(let
       ((emtg:*all-prpty-makers*)
	  (emtg:all-examples))
       (emtg:define-f nil ',group-args)
       emtg:all-examples))


;;;_ , Use examples

;;;_  . emtg:kv-matches-p
(defun emtg:kv-matches-p (kv filter)
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

;;;_  . emtg:some-kv-matches

(defun emtg:some-kv-matches (one-kv tagset)
   ""
   (some
      #'(lambda (kv)
	   (emtg:kv-matches-p kv one-kv))
      tagset))

;;;_  . emtg:filter-one
(defun emtg:filter-one (list one-kv)
   ""
   (remove* one-kv list
      :test-not
      #'(lambda (one-kv item)
	   (emtg:some-kv-matches 
	      one-kv 
	      (emtg:example->tagset item)))))

;;;_  . emtg:filter
(defun emtg:filter (list kv-list)
   ""

   (reduce
      #'emtg:filter-one
      kv-list
      ;;In `reduce', this occurs as arg 1 to FUNC.  List elements are
      ;;arg 2. 
      :initial-value list))


;;;_  . emtg:get-value
(defun emtg:get-value (list kv-list)
   ""
   
   (let
      ((vals (emtg:filter list kv-list)))
      
      (case (length vals)
	 (0 
	    (signal 'emtg:err:not-found kv-list))
	 (1 
	    (emtg:example->value (car vals)))
	 (t 
	    (signal 'emtg:err:too-many kv-list)))))

;;;_  . emtg
;;;###autoload
(defmacro emtg (&rest kv-list)
   ""
   ;;Call `emtg:value' instead.
   `(emtg:get-value emtg:all-examples 
       (append 
	  ;;New
	  (emtg:tagset-strip emtg:tagset 
	     ',(mapcar
		#'(lambda (kv)
		     (if (consp kv) (car kv) kv))
		kv-list))
	  ;;Original
	  ;;emtg:tagset 

	  ',kv-list)))
;;;_  . emtg:value
;;;###autoload
(defmacro* emtg:value (&key narrow ignore-tags)
   "
Takes keywords
  NARROW - A tagset to narrow by.
  IGNORE-TAGS - tags to be ignored."
   `(emtg:get-value emtg:all-examples
       (append
	  (emtg:tagset-strip emtg:tagset ',ignore-tags)
	  ',narrow)))
;;;_  . emtg+
(defmacro emtg+ (examples &rest kv-list)
   ""
   ;;Merge this and `emtg:value' behind one helper.
   `(emtg:get-value ,examples
       (append 
	  ;;New
	  (emtg:tagset-strip emtg:tagset 
	     ',(mapcar
		#'(lambda (kv)
		     (if (consp kv) (car kv) kv))
		kv-list))
	  ',kv-list)))
;;;_  . emtg:value+
(defun emtg:value+ (kv-list &optional examples)
   "Return the value of the respective example.
This mode is intended for combining computed tagsets.
EXAMPLES can be nil or the name of a tagged set.
KV-LIST is a calculated kv-list.  It can use backquote syntax."
   
   (emtg:get-value (or examples emtg:all-examples) kv-list))

;;;_ , Narrowing the tagset
;;;_  . emtg:narrow-f
(defun emtg:narrow-f (qt-kv-filter body)
   "Execute BODY in a particular examples tagset.
Purpose: For programmability.
Takes it arguments literally."
   
   `(let
       ((emtg:tagset 
	   (append ,qt-kv-filter emtg:tagset)))
       ,@body))
;;;_  . emtg:ignore-tags
(defmacro emtg:ignore-tags (transparent-tags &rest body)
   ""
   '  ;;Didn't work right.
   (emtg:narrow-f 
      `',(emtg:tagset-strip emtg:tagset transparent-tags)
      body)

   `(let
      ((emtg:tagset 
	  (emtg:tagset-strip emtg:tagset ',transparent-tags)))
      ,@body))

;;;_  . emtg:narrow
;;;###autoload
(defmacro* emtg:narrow (kv-filter &rest body)
   "Execute BODY in a particular examples tagset.
Purpose: For consise use inside test code."
   (emtg:narrow-f `',kv-filter body))


;;;_  . emtg:with

(defmacro* emtg:with (examples kv-filter &rest body)
   "Execute BODY in a particular examples tagset.
Purpose: For consise use inside test code."
   `(let
      ((emtg:all-examples ,examples))
      ,(emtg:narrow-f `',kv-filter body)))

;;;_ , Looping
;;;_  . emtg:map
;;;###autoload
(defmacro emtg:map (tag name &rest body)
   ""
   (let
      ((name (or name (gensym))))
      `(let
	  ((arg-values (emtg:all-tag-args ',tag)))
	  (emt:doc 
	     (concat 
		"TAGNAME operation: Loop over values of "
		(prin1-to-string ',tag)))
	  (emth:map&trap
	     #'(lambda (,name)
		  ,(emtg:narrow-f
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
;;;_  . emtg:all-tags
(defun emtg:all-tags (&optional example-list)
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
		    (emtg:example->tagset x)))
	    (or example-list emtg:all-examples)))))

;;;_  . emtg:all-tag-args
(defun emtg:all-tag-args (tag)
   "Return a list of all values for a given tag.
Returns `no-arg' if the key takes no argument."
   (catch 'emtg:tag-no-arg
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
				     (throw 'emtg:tag-no-arg 'no-arg)
				     ())))
			  (emtg:example->tagset x))))
	       (emtg:filter 
		  emtg:all-examples
		  emtg:tagset))))))

;;;_  . emtg:boundp
;;;###autoload
(defun emtg:boundp (kv-pair)
   "Return non-nil if there exists at least one item that would be
selected by KV-PAIR."

   (member (second kv-pair) (emtg:all-tag-args (first kv-pair))))

;;;_  . emtg:ambiguous-p
(defun emtg:ambiguous-p (tagset)
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

;;;_  . emtg:see-err Include errors in examples
(defun emtg:see-err (form)
   "Capture the error raised by FORM.
Use this inside tagnames, otherwise you risk capturing errors
that tagnames uses internally.
FORM must be a form that, when evalled, raises an error"
   
   (condition-case err
      (progn
	 (eval form)
	 ;;If it failed to raise an error, raise a (meta-)error.
	 (error "Form failed to raise error: %s" form))

      ;;If it's an tagnames error, re-raise it (This is important to do)
      (emtg:err (signal (car err)(cdr err)))
      ;;Otherwise trap it and pass on its value
      (error err)))



;;;_ , Editing help
;;;_  . Fill in an example-spec from tags
;;That's being done in pcmpl-emtest

;;;_  . From `emtg', go to its definition
;;At least, if it's in the same file.
;;Can easily find definer-id

;;;_  . From example, go to file it refers to
;;Like the above, just one more step.
;;Would help to look at a "type" property of items.

;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tagnames)
(provide 'emtest/testhelp/eg)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tagnames.el ends here
