;;;_ formatter/dynamic.el --- Dynamic formatter

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: tools

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

(require 'ewoc)
(eval-when-compile (require 'cl))

;;;_. Body
;;;_ , Types
(defstruct (fmtdyn:node
	      (:constructor fmtdyn:make-node)
	      (:conc-name fmtdyn:node->))
   "Data that a dynamic ewoc node holds"
   (nodes-under () :type (repeat vector))
   obj
   format-func
   format-dynvars
   insert-dynvars
   (ewoc () :type ewoc)
   insert-func)

;;;_ , Support
;;;_  . Special variables
(declare (special fmtdyn:*interstice* fmtdyn:*node* fmtdyn:*anchor*
	    fmtdyn:*nodes-under* fmtdyn:*ewoc*)) 

;;Most are valid just within `fmtdyn:printer'.  `fmtdyn:*ewoc*' is
;;valid there and also within `fmtdyn:with', which surrounds the
;;initial insertion call.  Its value is carried within `fmtdyn:node',
;;the others' values do not outlast the `fmtdyn:printer' scope.  None
;;of these are dynvars.

;;;_   , fmtdyn:dynvars 
;;These aren't intended for `utidyv:top' because the proper initial
;;forms don't all come from one place.
(defconst fmtdyn:dynvars 
   '(
       (fmtdyn:*ewoc* nil)
       ;;From loformat
       (loformat:alist) 
       (loformat:text-properties '())
       (recurse-f))
   "Dynamic variables that dynamic uses" )

;;$$MOVE ME Move most of this into loformat
;;$$IMPROVE ME Add recurse-f, instead of naming that as an arg and here.
;;$$IMPROVE ME Add `fmtdyn:*ewoc*' instead of it being a field.

;;;_  . Anchors
;;;_   , fmtdyn:set-anchor
(defun fmtdyn:set-anchor ()
   "Set the current anchor to point."
   (setq fmtdyn:*anchor* (point)))
;;;_   , fmtdyn:get-anchored-text
(defun fmtdyn:get-anchored-text ()
   "Get the text between current anchor and point."
   (buffer-substring fmtdyn:*anchor* (point)))
;;;_  . Interstices
;;;_   , fmtdyn:begin-interstice
(defun fmtdyn:begin-interstice (ewoc prev-node)
   "Begin an interstice."
   (when (and (fmtdyn:in-printer-p) prev-node)
      (let
	 ((interstice
	     ;;Make a new ewoc node.
	     (ewoc-enter-after ewoc prev-node nil)))
	 ;;Make it the current interstice.
	 (setq fmtdyn:*interstice* interstice)
	 ;;Record it as a node under main node.
	 (push interstice fmtdyn:*nodes-under*)
	 ;;Record that it is now our current node.
	 (setq fmtdyn:*node* interstice)
	 ;;Set a new anchor.
	 (fmtdyn:set-anchor))))


;;;_   , fmtdyn:end-interstice
(defun fmtdyn:end-interstice ()
   "End an interstice.
Store the current interstice text in interstice node, if it
exists and doesn't already have data."
   (when (and 
	    (fmtdyn:in-printer-p)
	    fmtdyn:*interstice*
	    (not (ewoc-data fmtdyn:*interstice*)))
      (ewoc-set-data
	 fmtdyn:*interstice*
	 (fmtdyn:get-anchored-text))))
;;;_  . Other support
;;;_   , fmtdyn:delete-recurse
(defun fmtdyn:delete-recurse (node)
   "Recursively delete NODE and ewoc nodes under it."
   (let
      ((data (ewoc-data node)))
      (unless (stringp data)
	 (mapcar 
	    #'fmtdyn:delete-recurse 
	    (fmtdyn:node->nodes-under data))))
   (ewoc-delete node))
;;;_   , fmtdyn:in-printer-p
(defun fmtdyn:in-printer-p ()
   "Return non-nil if we are in the scope of the fmtdyn printer."
   (boundp 'fmtdyn:*interstice*))

;;;_ , Entry points
;;;_  . fmtdyn:create
(defun fmtdyn:create ()
   "Make a fmtdyn object (really an ewoc)"
   (ewoc-create #'fmtdyn:printer nil nil t))

;;;_  . fmtdyn:->ewoc
(defalias 'fmtdyn:->ewoc 'identity)

;;;_  . fmtdyn:with
(defmacro fmtdyn:with (fmtdyn &rest body)
   "Evaluate BODY in a fmtdyn scope.
Bind this around call to loformatter."
   `(let
       ((fmtdyn:*ewoc* (fmtdyn:->ewoc ,fmtdyn)))
       ,@body))

;;;_  . fmtdyn:insert Dynamic formatter
(defun fmtdyn:insert (recurse-f obj func data)
   "Insert a dynamic object.
Suitable for loformatter."
   ;;This may be called not inside a call to `fmtdyn:printer', so it
   ;;must not assume that special variables are set up.  Ewoc,
   ;;however, should already be set up.
   (assert (boundp 'fmtdyn:*ewoc*))
   
   (let
      ((node-data
	  ;;$$IMPROVE ME Possibly tell it it's folded, depending on
	  ;;whether outline thinks we're folding (ie, outline's fold
	  ;;variable)
	  (fmtdyn:make-node
	     :nodes-under     nil
	     :obj             obj
	     :format-func     func
	     :format-dynvars  data
	     :insert-dynvars  
	     (if (fmtdyn:in-printer-p)
		(utidyv:capture-vars fmtdyn:dynvars)
		(utidyv:capture-vars '()))
	     :ewoc            fmtdyn:*ewoc*
	     :insert-func     recurse-f)))
      
      (if (fmtdyn:in-printer-p)
	 ;;Realize the object, within the normal inserting that
	 ;;loformat does, as an as-yet-unprinted ewoc node.  Since the
	 ;;parent node can't hold discontiguous text, we make simple
	 ;;"interstice" nodes in between these dynamic ewoc nodes.
	 (let*
	    ((node-1 fmtdyn:*node*)
	       (node-2 (ewoc-enter-after fmtdyn:*ewoc* node-1 node-data)))
	    (push node-1 fmtdyn:*nodes-under*)
	    (fmtdyn:begin-interstice fmtdyn:*ewoc* node-2))
	 (ewoc-enter-last fmtdyn:*ewoc* node-data))))

;;;_  . fmtdyn:printer
(defun fmtdyn:printer (data)
   "Print DATA.
DATA may be a string or a `fmtdyn:node'.
This function is suitable as an ewoc printer."
   
   (if
      (stringp data)
      ;;If data is a string, just insert it
      (insert data)

      ;;Otherwise it is a `fmtdyn:node'
      (fmtdyn:with (fmtdyn:node->ewoc data)
	 ;;$$IMPROVE ME If it is indicated as folded, just print a button
	 ;;to unfold it, whose action will just unflag it and dirty it.

	 ;;Bind the special variables we use to collect data.
	 (let*
	    ((fmtdyn:*interstice* nil) 
	       (fmtdyn:*anchor* nil)
	       (fmtdyn:*nodes-under* '())
	       (fmtdyn:*node* (ewoc-locate fmtdyn:*ewoc*)))
	    
	    ;;Delete any existing ewoc nodes under this.
	    (mapcar 
	       #'fmtdyn:delete-recurse 
	       (fmtdyn:node->nodes-under data))

	    ;;The first interstice is nil, we only make an anchor.
	    (fmtdyn:set-anchor)

	    (let
	       ((tree
		   ;;Bind its dynvars and hiformat it.
		   (utidyv:with-vars 
		      (fmtdyn:node->format-dynvars data)
		      (funcall
			 (fmtdyn:node->format-func data)
			 (fmtdyn:node->obj data)))))
	       ;;Loformat the result
	       (utidyv:with-vars 
		  (fmtdyn:node->insert-dynvars data)
		  (funcall
		     (fmtdyn:node->insert-func data)
		     tree)))
	    
	    ;;End the current interstice.
	    (fmtdyn:end-interstice)

	    ;;Remember the nodes directly under us.
	    (setf
	       (fmtdyn:node->nodes-under data)
	       fmtdyn:*nodes-under*)))))
;;;_  . Usage
'(defvar my-fmtdyn (fmtdyn:create))
'(let* ((tree (emtvf:top top-node)))
    (fmtdyn:with my-fmtdyn
       (loformat:insert
	  tree
	  ;;Whice presumably includes (dynamic fmtdyn:insert)
	  emtv2:format-alist)))
'(ewoc-refresh (fmtdyn:->ewoc my-fmtdyn))
'(ewoc-invalidate (fmtdyn:->ewoc my-fmtdyn) node1 node2 etc)
;;;_. Footers
;;;_ , Provides

(provide 'formatter/dynamic)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; formatter/dynamic.el ends here
