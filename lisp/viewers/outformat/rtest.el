;;;_ viewers/outformat/rtest.el --- Tests for outformat

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

;;;_. Body

;;;_  . Tests
;;All OBSOLETE
(rtest:deftest emt:plain-viewer:stage2->stage3
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name simple))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-info-about))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-child-events))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-child-event-groups))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-mixed-children-1))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name has-mixed-children-2))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name aborted-no-events))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   '
   (  "An inspection test.  Manual for now."
      (emt:eg:narrow 
	  ((name aborted-after-events))
	 (emt:plain-viewer:stage2->stage3
	    (emt:plain-viewer:->format-stage2
	       (emt:plain-viewer:->format-tree 
		  (emt:eg (type event-list)))
	       emt:plain-viewer:stage2-initial-state))))
   ;;This was premature
   (  "Param: A diag that says a call tried to use a persist.
Response: A string in it is usable by a command."
      (emt:eg:narrow 
	 ;;This would usually have (library persist) but this part
	 ;;merges persist and viewer functionality (though viewer is
	 ;;not yet using these examples)
	 ((project emtest)(section persist-viewer))
	 (require 't/emt-persist "t/emt-persist.el")
	 (emt:db:internal:ts:mock (emt:eg (type versions))
	    (let
	       ((result
		   (emt:plain-viewer:stage2->stage3
		      (emt:plain-viewer:->format-stage2
			 (emt:plain-viewer:->format-tree 
			    (emt:eg (type result-diag)(foundp t)))
			 emt:plain-viewer:stage2-initial-state))))

	       ;;This is not the right place to test that commands
	       ;;work when point is on that string.
	       (assert
		  (emt:somewhere-in-tree
		     #'(lambda (a)
			  (and
			     (stringp a)
			     ;;Not looking at that property's value,
			     ;;just its existence.
			     (get-text-property 1 'emt:diag:tried a)
			     (get-text-property 1 'emt:diag:call a)))
		     result) 
		  t)
	       
	       ;;This is here just for viewability.  Later it will
	       ;;itself be a persist object.
	       ;;`result' wants to be viewable.
	       t
	       )))
      )

   
   )
;;;_. Footers
;;;_ , Provides

(provide 'viewers/outformat/rtest)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; viewers/outformat/rtest.el ends here
