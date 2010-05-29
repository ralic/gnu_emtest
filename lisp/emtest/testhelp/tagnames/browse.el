;;;_ emtest/testhelp/tagnames/browse.el --- Browser for tagname objects

;;;_. Headers
;;;_ , License
;; Copyright (C) 2010  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: lisp, tools

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

;; A browser for tagnames types.  This is a work in progress.


;;;_ , Requires

(require 'emtest/testhelp/tagnames)
(require 'viewers/formatter)  ;;Just for the browse functionality


;;;_. Body
;;;_ , Browse  (Never worked)

;;;_  . Structures
(defstruct (emtg:browse:relative-distinction
	      (:constructor emtg:browse:make-relative-distinction)
	      (:conc-name emtg:browse:relative-distinction->)
	      (:copier nil))
   ""
   item
   extra-kv-list
   missing-kv-list)

;;;_  . emtg:browse:make-relative-distinction
(defun emtg:browse:item->relative-distinction (item reference-tagset)
   ""
   (emtg:browse:make-relative-distinction
      :item item
      ;;$$Not at all sure these are in right respective positions.
      ;;Feel free to swap them.
      :extra-kv-list 
      (remove
	 nil
	 (mapcar
	    #'(lambda (kv)
		 (unless
		    (emtg:some-kv-matches kv reference-tagset)
		    kv))
	    (emtg:example->tagset item)))

      :missing-kv-list
      (remove
	 nil
	 (mapcar
	    #'(lambda (kv)
		 (unless
		    (emtg:some-kv-matches kv (emtg:example->tagset item))
		    kv))
	    reference-tagset))))

;;;_  . emtg:browse:->stage-1
(defun emtg:browse:->stage-1 (obj)
   ""
   
   (typecase obj
      (emtg:browse:relative-distinction
	 `(w/headline
	     (:weight 2)
	     "An item"
	     (sep 3)
	     (data-persist-used
		,(emtg:browse:relative-distinction->extra-kv-list obj)
		())
	     (sep 3)
	     (data-persist-used
		,(emtg:browse:relative-distinction->missing-kv-list obj)
		())))
      
      ;;Here's a place to handle groupings, if ever we have them.

      ;;Lists are iterated over.  This is slightly messed up with
      ;;`list' vs type vs deep-type.  It's also a PITA wrt setting up
      ;;a list vs setting up a sequence.
      (cons
	 `(sequence
	     ,@(mapcar #'emtg:browse:->stage-1 obj)))))

;;;_  . emtg:browse:top
;;$$UPDATE ME:  Needs to take a tagname list.
(defun emtg:browse:top (reference-tagset narrowing-tagset)
   ""
   
   (interactive)
   (let*
      (
	 ;;This will restrict them if called within a tagname narrowing.
	 ;;Surprising but can't be helped.
	 (all-items
	    (emtg:filter emtg:all-examples narrowing-tagset))
	 ;;This may be used later, in sorting remaining tags
	 (all-tags
	    (emtg:all-tags all-items))
	 (all-distinctions
	    (mapcar
	       #'(lambda (item)
		    (emtg:browse:item->relative-distinction
		       item
		       reference-tagset))
	       all-items))
	 (sorted
	    ;;Punt for now
	    all-distinctions)
	 (grouped
	    ;;Punt for now
	    sorted)
	 (summarized
	    ;;Punt for now
	    grouped)
	 (stage-1-formatted
	    (emtg:browse:->stage-1 summarized)))
      (formatter:top-x
	 "*TAGNAME browse*"
	 stage-1-formatted
	 #'loformat:insert)))



;;;_ , Browse near-miss

;;;_  . emtg:browse

(defun emtg:browse (tagset &optional tags-too-wide) 
   "Pop up a buffer browsing the existing definitions.

TAGSET must be a tagset"
   ;;If TAGS-TOO-WIDE is given, ignore those tags for purposes of
   ;;generating near-matches.
   ;;Could be made interactive by picking from tags.
   (let*
      ((buf (generate-new-buffer "*TAGNAME browse*"))
	 ;;Generate it raw.  Sort it by felicity of fit (0 sorts last)
	 (near-matches-raw
	    (mapcar
	       #'(lambda (kv)
		    (list 
		       kv
		       (emtg:filter 
			  emtg:all-examples 
			  (remove kv tagset))))
	       tagset))
	 (near-matches-lol near-matches-raw)
	 )
      
      ;;Use formatter if this formatting becomes difficult
      (with-current-buffer buf
	 (insert "Near matches for tagset "
	    ;;Want a nicer way of printing tagset.
	    (prin1-to-string tagset)
	    "\n\n")
	 
	 (dolist (nm near-matches-lol)
	    (destructuring-bind
	       (kv-off near-matches) nm
	       (insert 
		  "** "
		  "Without restriction `" (symbol-name (car kv-off))
		  "' there are "
		  (format "%d" (length near-matches))
		  " matches")
	       (insert "\n")

	       (dolist (i near-matches)
		  (let*
		     ((i-tagset (emtg:example->tagset i))
			(diff-tagset 
			   (set-difference 
			      i-tagset tagset
			      :test #'equal)))
		     (insert
			"*** "
			"Adding " (format "%d" (length diff-tagset))
			" tags"
			"\n"
			;;First print what it adds
			"New tags "
			(format "%s"
			   diff-tagset)
			"\n"
			;;Then print its whole tagset
			"Tagset: "
			(format "%s" i-tagset)
			"\n\n")))
	       
	       (insert "\n")))
	 ;;To user can fold it, just put the buffer into outline mode.
	 (outline-mode))
      (pop-to-buffer buf)))
;;;_   , Tests



;;;_. Footers
;;;_ , Provides

(provide 'emtest/testhelp/tagnames/browse)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; emtest/testhelp/tagnames/browse.el ends here
